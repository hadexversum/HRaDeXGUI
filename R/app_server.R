#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dat <- mod_input_data_server("input_data")
  
  fit_k_params <- mod_settings_class_definition_server("class_definition")
  
  fit_control <- mod_settings_fit_server("settings_fit_control")
  
  workflow_type <- mod_settings_workflow_server("workflow")
  
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
  
  list_params <- reactive({
    
    HRaDeX::create_fit_dataset(kin_dat(), 
                               control = fit_control(), 
                               fit_k_params = fit_k_params(), 
                               trace = F, 
                               workflow = workflow_type())
  })
  
  output[["plot_cov_class_plot"]] <- renderPlot({ HRaDeX::plot_cov_class(list_params()) })
  
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ HRaDeX::plot_3_exp_map_v2(list_params()) })
  
  output[["plot_n_plot"]] <- renderPlot({ HRaDeX::plot_n(list_params()) })
  
  output[["plot_r2_hist_plot"]] <- renderPlot({ HRaDeX::plot_r2_hist(list_params()) })
  
  output[["params_list_data"]] <- DT::renderDataTable({ 
    
      dplyr::mutate(list_params(), 
             n_1 = round(n_1, 2),
             k_1 = round(k_1, 2),
             n_2 = round(n_2, 2),
             k_2 = round(k_2, 2),
             n_3 = round(n_3, 2),
             k_3 = round(k_3, 2))
      })
  
  output[["fit_info"]] <- renderText({ HRaDeX::get_fit_values_info(list_params() )})
  
  mod_fit_plots_server("fit_plots",
                       kin_dat = kin_dat(), 
                       list_params = list_params())
}
