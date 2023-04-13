#' show_overall_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot
mod_show_overall_analysis_ui <- function(id){
  ns <- NS(id)
  renderPlot(ns("plot_analysis"))
}

#' show_overall_analysis Server Functions
#'
#' @noRd
mod_show_overall_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output[[ns("plot_analysis")]] <- plotOutput({

    })

  })
}

## To be copied in the UI
# mod_show_overall_analysis_ui("show_overall_analysis_1")

## To be copied in the server
# mod_show_overall_analysis_server("show_overall_analysis_1")
