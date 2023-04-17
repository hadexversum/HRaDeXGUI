#' fit_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fit_parameters_ui <- function(id){
  ns <- NS(id)

  wellPanel(
    span("The process is currently conducted using fixed parameters. This will change in the future.", style="color:red"),
    h3("Used parameters"),
    p("Workflow - 3 exp / 1 exp"),


    splitLayout(
      div("For three-exp fit:", br(),
          "0 < n = 0.33 < 1         ", br(),
          "1 < k = 2 < 30        ", br(),
          "0.1 < k = 0.1  < 1       ", br(),
          "0 < k = 0.01 < 0.1    ", br()
      ),
      div("For one-exp fit:", br(),
          "0 < n = 0.33 < 1", br(),
          "0 < k = < 30", br())

    ),
    br(),
    div("Control:", br(),
        "Max iteration: 1000 ", br(),
        "Scale: levenberg", br()),
    div("")
)

   # collapsible_card(
  #   title = "Calculation",
  #   checkboxInput(
  #     inputId = ns("theoretical"),
  #     label = "Theoretical calculations",
  #     value = FALSE
  #   ),
  #   checkboxInput(
  #     inputId = ns("fractional"),
  #     label = "Fractional values",
  #     value = FALSE
  #   ),
  #   fancy_icon = "cogs"
  # )
}

#' fit_parameters Server Functions
#'
#' @noRd
mod_fit_parameters_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fit_parameters_ui("fit_parameters_1")

## To be copied in the server
# mod_fit_parameters_server("fit_parameters_1")
