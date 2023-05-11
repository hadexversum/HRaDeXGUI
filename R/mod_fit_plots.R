#' fit_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fit_plots_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("plot_fit_plots"))
}
    
#' fit_plots Server Functions
#'
#' @noRd 
mod_fit_plots_server <- function(id, kin_dat, list_params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output[["plot_fit_plots"]] <- renderUI({
      
      # browser()
      
      lapply(1:nrow(list_params),  function(i){
        
        print(i)
        
        fit_dat <- dplyr::filter(kin_dat, 
                                 Sequence == list_params[i, "sequence"],
                                 Start == list_params[i, "start"]) 
        
        fit_values <- list_params[i, ]
        
        renderPlot(HRaDeX::plot_uc_fit(fit_dat,
                                       fit_values,
                                       triplex = T))
        
      })
      
    })
    
  })
}
    
## To be copied in the UI
# mod_fit_plots_ui("fit_plots")
    
## To be copied in the server
# mod_fit_plots_server("fit_plots")
