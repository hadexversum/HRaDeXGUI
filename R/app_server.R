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
  
  # output[["run_status"]] <- renderText(
  #   paste("Please press the button to confirm selected parameters.")
  # )
  
  kin_dat <- reactive({
    
    validate(need(input[["do_run"]] > 0, "Run the analysis by pressing the button on the left."))
    
    print("Creating kinetic data")
    print(paste0("Protein: ", dat()[["Protein"]][[1]]))
    print(paste0("State: ", s_fit_state %()% state))
    
    HRaDeX::prepare_kin_dat(dat(), 
                            state = s_fit_state %()% state,
                            time_0 = min(dat()[["Exposure"]]),
                            time_100 = max(dat()[["Exposure"]]))
    
    
    
  })
  
  list_params <- reactive({
    
    print("Creating fit dataset")
    print(paste0("Workflow type ", workflow_type()))
    print("Fit params:")
    print(fit_k_params())
    
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
    
    tmp_dat <- dplyr::select(list_params(), -id)
    
      dplyr::mutate(tmp_dat, 
             n_1 = round(n_1, 3),
             k_1 = round(k_1, 3),
             n_2 = round(n_2, 3),
             k_2 = round(k_2, 3),
             n_3 = round(n_3, 3),
             k_3 = round(k_3, 3)) 
  })
  
  output[["download_fit_params_table"]] <- downloadHandler(
    filename = "fit_params_data.csv",
    content = function(file){
      write.csv(dplyr::select(list_params(), -id), file)
    }
  )
  
  output[["fit_info"]] <- renderText({ HRaDeX::get_fit_values_info(list_params() )})
  
  # mod_fit_plots_server("fit_plots",
  #                      kin_dat = kin_dat(), 
  #                      list_params = list_params())
  
  output[["plot_fit_plots"]] <- renderUI({
    
    lapply(1:nrow(list_params()),  function(i){
      
      fit_dat <- dplyr::filter(kin_dat(), 
                               Sequence == list_params()[i, "sequence"],
                               Start == list_params()[i, "start"]) 
      
      fit_values <- list_params()[i, ]
      
      renderPlot(HRaDeX::plot_uc_fit(fit_dat,
                                     fit_values,
                                     duplex = T,
                                     triplex = F))
      
    })
    
  })
  
}
