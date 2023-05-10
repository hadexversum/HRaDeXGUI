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
  
  fit_k_params <- reactive({
    
    data.frame(start = unlist(fit_params()[2]), 
               lower = unlist(fit_params()[3]),
               upper = unlist(fit_params()[1]),
               row.names = c("k_1", "k_2", "k_3"))
    
  })
  

  params_list <- reactive({
    
    HRaDeX::create_fit_dataset(kin_dat(), 
                               control = fit_control(), 
                               fit_k_params = fit_k_params(), 
                               trace = F, 
                               workflow = workflow_type())
  })
  
  output[["plot_cov_class_plot"]] <- renderPlot({ HRaDeX::plot_cov_class(params_list()) })
  
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ HRaDeX::plot_3_exp_map_v2(params_list()) })
  
  output[["plot_n_plot"]] <- renderPlot({ HRaDeX::plot_n(params_list()) })
  
  output[["plot_r2_hist_plot"]] <- renderPlot({ HRaDeX::plot_r2_hist(params_list()) })
  
  output[["params_list_data"]] <- DT::renderDataTable({ 
    
      dplyr::mutate(params_list(), 
             n_1 = round(n_1, 2),
             k_1 = round(k_1, 2),
             n_2 = round(n_2, 2),
             k_2 = round(k_2, 2),
             n_3 = round(n_3, 2),
             k_3 = round(k_3, 2))
      })
  
  output[["fit_info"]] <- renderText({ HRaDeX::get_fit_values_info(params_list() )})
}
