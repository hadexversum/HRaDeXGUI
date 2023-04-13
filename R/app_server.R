#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dat <- mod_input_data_server("input_data")
  mod_create_fit_server("create_fit")

  kin_dat <- reactive({
    
    HRaDeX::prepare_kin_dat(dat(), 
                            state = dat()[["State"]][1],
                            time_0 = 0.001,
                            time_100 = max(dat()[["Exposure"]]))
    
  })
  
  peptide_list <- reactive({ HRaDeX::get_peptide_list(kin_dat()) })
  
  control <- list(maxiter = 1000,  scale = "levenberg")
  start_3 = c(n_1 = 0.33, k_1 = 2, n_2 = 0.33, k_2 = 0.1, n_3 = 0.33, k_3 = 0.01)
  lower_3 = c(n_1 = 0, k_1 = 1, n_2 = 0, k_2 = 0.1, n_3 = 0, k_3 = 0)
  upper_3 = c(n_1 = 1, k_1 = 30, n_2 = 1, k_2 = 1, n_3 = 1, k_3 = 0.1)
  start_1 = c(n = 1, k = 0.1)
  lower_1 = c(n = 0, k = 0)
  upper_1 = c(n = 2, k = 30)
  
  
  params_list <- reactive({
    HRaDeX::get_params_list(kin_dat(), peptide_list(), 
                            control, 
                            start_1,lower_1, upper_1, 
                            start_3,lower_3, upper_3)
  })
  
  fixed_params <- reactive({ HRaDeX::fix_params_list(params_list(), lower_3, upper_3) })

  output[["plot_cov_class_plot"]] <- renderPlot({ HRaDeX::plot_cov_class(fixed_params()) })
  
  output[["get_params_summary_image_plot"]] <- renderPlot({ HRaDeX::get_params_summary_image(fixed_params()) })
  
  output[["plot_start_params_plot"]] <- renderPlot({ HRaDeX::plot_start_params(start_1, start_3) })
  
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ HRaDeX::plot_3_exp_map_v2(fixed_params()) })
  
  output[["plot_n_plot"]] <- renderPlot({ HRaDeX::plot_n(fixed_params()) })
  
  output[["plot_r2_hist_plot"]] <- renderPlot({ HRaDeX::plot_r2_hist(fixed_params()) })
  
  output[["params_list_data"]] <- DT::renderDataTable({ params_list() })
  

}
