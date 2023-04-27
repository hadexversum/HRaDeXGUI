#' settings_class_definition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_class_definition_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Class definition",
    splitLayout(
      div(
        span("Fast exchange", style="color:red"),
        numericInput(inputId = ns("k_fast_upper"),
                     label = "Upper boundary",
                     value = 30,
                     min = 0, max = 30, step = 0.5),
        numericInput(inputId = ns("k_fast_lower"),
                     label = "Lower boundary",
                     value = 1,
                     min = 0, max = 30, step = 0.5),
        numericInput(inputId = ns("k_fast_start"),
                     label = "Initial value",
                     value = 1,
                     min = 0, max = 30, step = 0.5)
      ),
      div(
        span("Medium exchange", style="color:green"),
        numericInput(inputId = ns("k_medium_upper"),
                     label = "Upper boundary",
                     value = 1,
                     min = 0, max = 30, step = 0.1),
        numericInput(inputId = ns("k_medium_lower"),
                     label = "Lower boundary",
                     value = 0.1,
                     min = 0, max = 30, step = 0.1),
        numericInput(inputId = ns("k_medium_start"),
                     label = "Initial value",
                     value = 0.1,
                     min = 0, max = 30, step = 0.1),
      ),
      div(
        span("Slow exchange", style="color:blue"),
        numericInput(inputId = ns("k_slow_upper"),
                     label = "Upper boundary",
                     value = 0.1,
                     min = 0, max = 30, step = 0.01),
        numericInput(inputId = ns("k_slow_lower"),
                     label = "Lower boundary",
                     value = 0,
                     min = 0, max = 30, step = 0.01),
        numericInput(inputId = ns("k_slow_start"),
                     label = "Initial value",
                     value = 0.01,
                     min = 0, max = 30, 0.01)
        
      )
    ),
    p("Besides that, there are two extreme cases defined by the rule of thumb: immediate exchange (fire red), and no exchange (black). For more information see the documentation and help.  "),
    fancy_icon = "cogs"
  )
}
    
#' settings_class_definition Server Functions
#'
#' @noRd 
mod_settings_class_definition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    fit_params <- reactive({
      
      list(upper_3 = c(input[["k_fast_upper"]], input[["k_medium_upper"]], input[["k_slow_upper"]]),
           start_3 = c(input[["k_fast_start"]], input[["k_medium_start"]], input[["k_slow_start"]]),
           lower_3 = c(input[["k_fast_lower"]], input[["k_medium_lower"]], input[["k_slow_lower"]]))
    
      })
    
   return(fit_params)
   
  })
}
    
## To be copied in the UI
# mod_settings_class_definition_ui("settings_class_definition_1")
    
## To be copied in the server
# mod_settings_class_definition_server("settings_class_definition_1")
