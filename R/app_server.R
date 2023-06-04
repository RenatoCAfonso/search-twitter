#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import %>%
#' @noRd
app_server <- function(input, output, session) {
  observeEvent(input$get, {

    ################# GET INPUT FIELDS - LISTS, END_TIME AND START_TIME
    x <- input$l1
    pri<- stringr::str_trim(strsplit(x,",")[[1]])
    y <- input$l2
    sec<- stringr::str_trim(strsplit(y,",")[[1]])
    marcador <- length(sec)
    data = NULL

    withProgress(message = 'Collecting Data', value = 0, {

      ################## LOOP FOR ALL WORDS IN SECOND LIST
      for (i in sec){

        dados = FALSE
        query_search <- paste0('("', pri[1], ' ' , i, '"')

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
        incProgress(0, detail = paste("Doing part", i))


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
