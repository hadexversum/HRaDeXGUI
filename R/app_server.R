#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dat <- mod_input_data_server("input_data")
  
  # mod_create_fit_server("create_fit")
  
  fit_params <- mod_settings_class_definition_server("class_definition")
  
  fit_control <- mod_settings_fit_server("settings_fit_control")
  
  s_fit_state <- mod_settings_state_server(
    id = "fit_state",
    mode = "SINGLE",
    p_states_chosen_protein = reactive(unique(dat()[["State"]]))
  )
  
  kin_dat <- reactive({
    
    HRaDeX::prepare_kin_dat(dat(), 
                            state = s_fit_state %()% state,
                            time_0 = 0.001,
                            time_100 = max(dat()[["Exposure"]]))
    
  })
  
  peptide_list <- reactive({ HRaDeX::get_peptide_list(kin_dat()) })
  

  start_3 <- reactive({
    
    c(n_1 = 0.33, k_1 = fit_params()[["start_3"]][1], 
      n_2 = 0.33, k_2 = fit_params()[["start_3"]][2],
      n_3 = 0.33, k_3 = fit_params()[["start_3"]][3])
  })
  
  lower_3 <- reactive({
    
    c(n_1 = 0, k_1 = fit_params()[["lower_3"]][1], 
      n_2 = 0, k_2 = fit_params()[["lower_3"]][2],
      n_3 = 0, k_3 = fit_params()[["lower_3"]][3])
    
  })
  
  upper_3 <- reactive({
    
    c(n_1 = 1, k_1 = fit_params()[["upper_3"]][1], 
      n_2 = 1, k_2 = fit_params()[["upper_3"]][2],
      n_3 = 1, k_3 = fit_params()[["upper_3"]][3])
    
  })
  
  upper_1 <- reactive({
    
    c(n = 1,
      k = max(fit_params()[["upper_3"]]))
  })
  
  lower_1 <- reactive({
    
    c(n = 0,
      k = min(fit_params()[["lower_3"]]))
    
  })
  
  start_1 <- reactive({
    
    c(n = 1, 
      k = fit_params()[["start_3"]][2])
    
  }) 
  
  params_list <- reactive({
    
    HRaDeX::get_params_list(kin_dat(), peptide_list(), 
                            fit_control(), 
                            start_1(), lower_1(), upper_1(), 
                            start_3(), lower_3(), upper_3(),
                            trace = F)
  })
  
  fixed_params <- reactive({ HRaDeX::fix_params_list(params_list(), lower_3(), upper_3()) })
  
  output[["plot_cov_class_plot"]] <- renderPlot({ HRaDeX::plot_cov_class(fixed_params()) })
  
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ HRaDeX::plot_3_exp_map_v2(fixed_params()) })
  
  output[["plot_n_plot"]] <- renderPlot({ HRaDeX::plot_n(fixed_params()) })
  
  output[["plot_r2_hist_plot"]] <- renderPlot({ HRaDeX::plot_r2_hist(fixed_params()) })
  
  output[["params_list_data"]] <- DT::renderDataTable({ params_list() })
  
  output[["fit_info"]] <- renderText({ HRaDeX::get_fit_values_info(fixed_params() )})
}
