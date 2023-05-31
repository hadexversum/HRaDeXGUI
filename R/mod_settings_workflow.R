#' settings_workflow UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_workflow_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Workflow",
    selectInput(
      inputId = ns("type"),
      label = "Select desired workflow",
      choices = c("3exp/1exp" = 31, "2exp/1exp" = 21, "3exp/2exp/1exp" = 321),
      selected = "3exp/2exp/1exp",
      multiple = FALSE
    ),
    fancy_icon = "cogs"
  )
}
    
#' settings_workflow Server Functions
#'
#' @noRd 
mod_settings_workflow_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    workflow_type <- reactive({ input[["type"]] })
 
    return(workflow_type)
  
    })
}
    
## To be copied in the UI
# mod_settings_workflow_ui("settings_workflow_1")
    
## To be copied in the server
# mod_settings_workflow_server("settings_workflow_1")
