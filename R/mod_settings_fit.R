#' settings_fit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_fit_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Fit options",
    numericInput(inputId = ns("fit_maxiter"),
                 label = "Select number of iterations",
                 value = 200, 
                 min = 10, max = 1000, step = 10),
    selectInput(inputId = ns("fit_scale"),
                label = "Select method",
                choices = c("levenberg"),
                selected = "levenberg"),
    fancy_icon = "cogs"
  )
}
    
#' settings_fit Server Functions
#'
#' @noRd 
mod_settings_fit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    control <- reactive({
      list(maxiter = input[["fit_maxiter"]],  scale = input[["fit_scale"]])
    })  
    
    return(control)
      
  })
}
    
## To be copied in the UI
# mod_settings_fit_ui("settings_fit_1")
    
## To be copied in the server
# mod_settings_fit_server("settings_fit_1")
