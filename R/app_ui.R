#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @noRd

app_ui <- function(request) {
  shinyjs::useShinyjs()
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinythemes::shinytheme("cyborg"),
      br(),
      column(12, align="center",
             h1("GET DATA FROM TWITTER API"),
             br(),
             br()
      ),

      ######### INPUT FOR PRIMARY LIST
      fluidRow(
        column(6, offset=3, align = "left",
               h3("Search", actionButton("btnInfo", "?", class = "btn btn-success btn-xs", style = "margin-left: 10px;")),

        ),
        tags$div(id = "modalText")
      ),

      fluidRow(
        column(6, offset = 3, align = "left",
               textAreaInput("l1", NULL, width = "100%", rows = 1, value = "som, barulho, ruído, zoada, zumbido, burburinho")
        )
      ),

      ######### INPUT FOR SECUNDARY LIST
      fluidRow(
        column(6, offset=3, align="left",
               radioButtons("showBtn", "Add permutations",
                            choices = c("Yes" = 'Sim', "No" = "Não"),
                            selected = "Não",
                            inline = TRUE,)
        ),
      ),
      conditionalPanel(
        condition = "input.showBtn == 'Sim'",
        fluidRow(
          column(6, offset=3, align="leftr",
                 textAreaInput("l2", h3("Permutation with"), width ='100%', rows=1, value = 'agitado, movimento, dinâmica, conturbado, interessante, vibrante, animado, intenso, excitante, alegre,  agradavel, prazeroso,  aprazivel, acolhedor, ameno, calmo, tranquilo, sereno, sossegado, pacífico, rotineiro, parado, comum, corriqueiro, pacato, monótono, entediante, tedioso, maçante, sem graça, irritante,  desagradavel,  incomodo, chato, perturbador,  caotico, confuso, bagunçado, desorganizado, desordenado'),
          ),
        ),
      ),

      ######### INPUT FOR LANG
      fluidRow(
        column(6, offset = 3, align="left",
               br(),
               radioButtons(
                 "lang",
                 h3("Data language"),
                 choices = c("Portuguese - pt", "English - en", "Spanish - es"),
                 selected = NULL,
                 inline = TRUE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               ),
        )
      ),

      ############ INPUT FOR START_TIME AND END_TIME
      fluidRow(
        column(6, offset = 3, align="left",
               br(),
               dateRangeInput(
                 "date",
                 h3("Date range"),
                 start  = Sys.Date() - 90,
                 end    = Sys.Date()
               )
        )
      ),

      ######### INPUT FOR TOKEN
      fluidRow(
        column(6, offset = 3, align="leftr",
               br(),
               textAreaInput("token", label = h3("Bearer token"), width=920, rows=1, value = ""),
        )
      ),

      ########## BUTTONS
      fluidRow(
        column(12, align="center",
               br(),
               br(),
               actionButton("get", h4("Get data")),
        )
      ),

      ######### DATAFRAME OUTPUT
      fluidRow(align="center",
               column(10, offset=1,
                      br(),
                      DT::dataTableOutput("table")
               )
      ),

      ######### DOWNLOAD BUTTON
      fluidRow(align="center",
               column(10, offset=1,
                      br(),
                      uiOutput("downloadButton"),
                      br(),
                      br()
               )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "searchTwitter"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
