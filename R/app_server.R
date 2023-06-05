#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ## Modal for instructions
  observeEvent(input$btnInfo, {
    showModal(modalDialog(
      title = h3("Search instructions"),
      ## Text content
      p("This Web Application was developed for a PhD project coordinated by researcher Nara from the Faculty of Architecture and Urbanism at the University of São Paulo, Brazil."),
      p("We are utilizing the Twitter API with academic research access to extract tweets within a designated time frame. Consequently, it is imperative to possess a Bearer Token that corresponds to the same account type in order to access our system."),
      br(),
      p(h5("Search input characteristics:")),
      p(tags$b("Search: "), span("include search terms - by separating the words with commas, it is possible to search for each term individually - does not accept empty field.")),
      p("Examples:"),
      p("1- Books"),
      p("tweets with the term 'book' will be searched."),
      p("2-Books, recipes:"),
      p("tweets with the term 'books' will be searched, followed by tweets with the term 'recipes'."),
      p("Default: som, barulho, ruído, zoada, zumbido, burburinho"),
      br(),
      p(tags$b("Add Permutations: "), span("include terms to permute with the main search term - similarly, separate each word with a comma to perform separate queries.")),
      p("Examples:"),
      p("1 - Main query: Books, recipes"),
      p("permute with cake, pie"),
      p("Search results: (Book and cake) or (Book and pie) or (recipes and cake) or (recipes and pie)"),
      p("Default: agitado, movimento, dinâmica, conturbado, interessante, vibrante, animado, intenso, excitante, alegre,  agradavel, prazeroso,  aprazivel, acolhedor, ameno, calmo, tranquilo, sereno, sossegado, pacífico, rotineiro, parado, comum, corriqueiro, pacato, monótono, entediante, tedioso, maçante, sem graça, irritante,  desagradavel,  incomodo, chato, perturbador,  caotico, confuso, bagunçado, desorganizado, desordenado"),
      footer = tagList(
        actionButton("btnClose", "Ok", class = "btn btn-primary")
      ),
      easyClose = TRUE,

    ))
  })

  ## Close modal
  observeEvent(input$btnClose, {
    removeModal()
  })


  observeEvent(input$get, {

    ################# GET INPUT FIELDS - LISTS, END_TIME AND START_TIME
    x <- input$l1
    pri<- stringr::str_trim(strsplit(x,",")[[1]])
    y <- input$l2
    if(y != "" & input$showBtn == "Sim"){
      sec<- stringr::str_trim(strsplit(y,",")[[1]])
    } else{
      sec<- ""
    }
    marcador <- length(sec)
    data = NULL

    withProgress(message = 'Collecting Data', value = 0, {

      ################## LOOP FOR ALL WORDS IN SECOND LIST
      for (i in sec){

        dados = FALSE
        query_search <- paste0('("', pri[1], ' ' , i, '"')

        #### Exist if primary list is empty
        if(x==""){
          showNotification("Erro: Não existem dados disponíveis para os parâmetros da consulta.", type = "error")
          return()
        }

        #### COMBINE FIRST LIST WITH SECOND LIST
        for (j in pri[-1]){
          query_search = paste0(query_search, ' OR "', j, ' ' , i, '"')
        }
        lang = trimws(strsplit(input$lang, "-")[[1]][2])
        query_search = paste0(query_search, ')',' -is:retweet -is:reply -is:quote lang:', lang)

        ####### QUERY BUILDER
        handle <- query_search

        query_search = ""

        #### USE CREDENTIALS
        Sys.setenv(BEARER_TOKEN = input$token)
        bearer_token <- as.character(Sys.getenv("BEARER_TOKEN"))
        headers <- c(`Authorization` = c(sprintf('Bearer %s', bearer_token)))
        go<-TRUE
        incProgress(0, detail = paste("Doing", i))


        ############ START REQUISITIONS - LOOP #################################
        while(go){

          if(!dados){

            params <- list(`max_results` = 500,
                           tweet.fields = "created_at",
                           end_time = parsedate::format_iso_8601(input$date[2]),
                           start_time = parsedate::format_iso_8601(input$date[1])
            )

            dados = TRUE

          } else{

            size <- length(data$id)
            params <- list(`max_results` = 500,
                           tweet.fields = "created_at",
                           end_time = parsedate::format_iso_8601(data$created_at[data$id == data$id[size]]),
                           start_time = parsedate::format_iso_8601(input$date[1])
            )
          }

          #ENDPOINT
          url_handle <-sprintf('https://api.twitter.com/2/tweets/search/all?query=%s', handle)

          #RESPONSE
          tryCatch({

            response <-
              httr::GET(url = url_handle,
                        httr::add_headers(.headers = headers),
                        query = params)

            if(!response$status_code == 200 && response$status_code != 400){

              error_message <- jsonlite::fromJSON(content(response, "text"))

              if(error_message$status == 401){

                showNotification(paste0("Erro: 401 - ", "Não foi possível realizar a autenticação com a API do Twitter."), type = "error")
                return()

              } else{

                showNotification(paste0("Erro: ", error_message$status, " - ", error_message$detail), type = "error")
                return()

              }

            }
          }, error = function(e) {

            showNotification(paste0("Erro: ", e$message), type = "error")

          })

          obj <- httr::content(response, as = "text")

          #BUILD DATAFRAME
          json_data_initial <- as.data.frame(jsonlite::fromJSON(obj, flatten = TRUE)$data)
          if(length(json_data_initial)==0){
            go = FALSE
            dados = NULL

          }
          #REMOVE DUPLICATES
          else{
            data_tw <- dplyr::select(json_data_initial, c(id, created_at, text))
            data_tw$type_list2 = rep(i,length(data_tw$id))
            data_tw = data_tw[!duplicated(data_tw$id),]
            data_tw = data_tw[!duplicated(data_tw$text),]
            data = rbind(data, data_tw)
          }
          Sys.sleep(5)
        }
        incProgress(1/marcador, detail = paste("Doing part", i))
      }

      if(length(data)==0){

        showNotification("Erro: Não existem dados disponíveis para os parâmetros da consulta.", type = "error")

      } else {

        output$downloadButton <- renderUI({
          downloadButton("downloadData", "Download")
        })

      }


    })

    ########### RENDER OUTPUTS
    output$table <- DT::renderDT(data,
                             options = list(responsive = TRUE,
                                            pageLength = 5)
    )

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-twitter", ".csv", sep = "")
      },
      content = function(file) {

        ### MAKE CSV WITH NAME data-twitter.csv
        write.csv(data, file, row.names = FALSE)
      }
    )

  })

}
