#' create_fit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_fit_ui <- function(id){
  ns <- NS(id)
  tagList(

  )

}

#' create_fit Server Functions
#'
#' @noRd
mod_create_fit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_create_fit_ui("create_fit_1")

## To be copied in the server
# mod_create_fit_server("create_fit_1")
