#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @noRd

app_ui <- function(request) {
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
        column(6, offset=3, align="center",
               textAreaInput("l1", h3("Primary List"), width ='100%', rows=4,value = 'som, barulho, ruído, zoada, zumbido, burburinho'),
        )
      ),

      ######### INPUT FOR SECUNDARY LIST
      fluidRow(
        column(6, offset=3, align="center",
               textAreaInput("l2", h3("Secondary List"), width ='100%', rows=4, value = 'agitado, movimento, dinâmica, conturbado, interessante, vibrante, animado, intenso, excitante, alegre,  agradavel, prazeroso,  aprazivel, acolhedor, ameno, calmo, tranquilo, sereno, sossegado, pacífico, rotineiro, parado, comum, corriqueiro, pacato, monótono, entediante, tedioso, maçante, sem graça, irritante,  desagradavel,  incomodo, chato, perturbador,  caotico, confuso, bagunçado, desorganizado, desordenado'),
        ),
      ),

      ######### INPUT FOR LANG
      fluidRow(
        column(12, align="center",
               br(),
               br(),
               radioButtons(
                 "lang",
                 h3("Data Language"),
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
        column(12, align="center",
               br(),
               dateRangeInput(
                 "date",
                 h3("Date Range"),
                 start  = Sys.Date() - 90,
                 end    = Sys.Date()
               )
        )
      ),

      ######### INPUT FOR TOKEN
      fluidRow(
        column(12, align="center",
               br(),
               textAreaInput("token", label = h3("Bearer Token"), width=920, rows=1, value = ""),
        )
      ),

      ########## BUTTONS
      fluidRow(
        column(12, align="center",
               br(),
               br(),
               actionButton("get", h4("Get data")),
               downloadButton("downloadData", "Download")
        )
      ),

      ######### DATAFRAME OUTPUT
      fluidRow(align="center",
               column(10, offset=1,
                      br(),
                      DT::dataTableOutput("table")
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
