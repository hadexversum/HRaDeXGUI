#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  p_states_chosen_protein <- reactive(unique(dat()[["State"]]))
  
  observe({
    updateSelectInput(
      session,
      inputId = "fit_state",
      choices = p_states_chosen_protein(),
      selected = p_states_chosen_protein()[1]
    )
  })
  
  
  ######################
  ####### PARAMS #######
  ######################
  
  dat <- mod_input_data_server("input_data")
  
  fit_k_params <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    data.frame(start = c(input[["k_fast_start"]], input[["k_medium_start"]], input[["k_slow_start"]]),
               lower = c(input[["k_fast_lower"]], input[["k_medium_lower"]], input[["k_slow_lower"]]),
               upper = c(input[["k_fast_upper"]], input[["k_medium_upper"]], input[["k_slow_upper"]]),
               row.names = c("k_1", "k_2", "k_3")) 
  })
  
  ##
  
  fit_control <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    list(maxiter = input[["fit_maxiter"]],  scale = input[["fit_scale"]])
  
  }) 
  
  ##
  
  workflow_type <- eventReactive(input[["do_run"]],{ 
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    input[["type"]] 
    
  })
  
  ##
  
  fit_state <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    input[["fit_state"]]
  })
  
  ##
  
  # state_ok <- reactiveVal(0)
  # 
  # state_after_button <- eventReactive(input[["do_run"]], {
  #   s_fit_state %()% state })
  # 
  # s_fit_state <- mod_settings_state_server(
  #   id = "fit_state",
  #   mode = "SINGLE",
  #   p_states_chosen_protein = reactive(unique(dat()[["State"]])))
  # 
  
  ######################
  
  params_ready <- reactiveVal(0)
  
  ######################
  ####### EFFECT #######
  ######################
  
  params_fixed <- reactive({
    data.frame(start = c(input[["k_fast_start"]], input[["k_medium_start"]], input[["k_slow_start"]]),
                             lower = c(input[["k_fast_lower"]], input[["k_medium_lower"]], input[["k_slow_lower"]]),
                             upper = c(input[["k_fast_upper"]], input[["k_medium_upper"]], input[["k_slow_upper"]]),
                             row.names = c("k_1", "k_2", "k_3")) 
  })
  
  observe({
   if(!all(params_fixed() == fit_k_params())) params_ready(-1)
  })
  
  observe({
    if(input[["fit_maxiter"]] != fit_control()[["maxiter"]] | input[["fit_scale"]] != fit_control()[["scale"]]) params_ready(-1)
  })
    
  observe({
    if(input[["type"]] != workflow_type()) params_ready(-1)
  })
  
  # observe({
  #   if(state_after_button() != s_fit_state %()% state) {
  #     params_ready(-1)
  #     state_ok(0)
  #   }
  # })
  
  observe({
    if(fit_state() != input[["fit_state"]]) params_ready(-1)
  })
  
  # observe({
  #   if(state_after_button() == s_fit_state %()% state) state_ok(1)
  # })
  
  output[["run_status"]] <- renderText({
    
    mes <- ""
    
    if(input[["do_run"]] == 0) mes <- "Press the button to initiate the process."
    
    if(params_ready() == 1) mes <- ""
    
    if(params_ready() == -1) mes <- "The parameters changed. Please press the button to rerun analysis."
    
    mes
    
  })
  
  observe({
    if(all(params_fixed() == fit_k_params()) & input[["fit_maxiter"]] == fit_control()[["maxiter"]] & input[["fit_scale"]] == fit_control()[["scale"]] & input[["type"]] == workflow_type() & fit_state() == input[["fit_state"]]) params_ready(1)
  })
  
  ## checks
  
  
  kin_dat <- reactive({
  
    validate(need(input[["do_run"]] > 0, "Run the analysis by pressing the button on the left."))
    # validate(need(state_ok() == 1, "Please confirm state changes."))
    
    message("Creating kinetic data")
    message(paste0("Protein: ", dat()[["Protein"]][[1]]))
    message(paste0("State: ", fit_state())) #s_fit_state %()% state))
    
    HRaDeX::prepare_kin_dat(dat(), 
                            state = fit_state(), #s_fit_state %()% state,
                            time_0 = min(dat()[["Exposure"]]),
                            time_100 = max(dat()[["Exposure"]]))
    
    
    
  })
  
  list_params <- reactive({
    
    message("Creating fit dataset")
    message(paste0("Workflow type ", workflow_type()))
    message("Fit params:")
    message(fit_k_params())
    
    HRaDeX::create_fit_dataset(kin_dat(), 
                               control = fit_control(), 
                               fit_k_params = fit_k_params(), 
                               trace = F, 
                               workflow = workflow_type())
  })
  
  plot_cov_class_plot_out <- reactive({ HRaDeX::plot_cov_class(list_params()) })
  output[["plot_cov_class_plot"]] <- renderPlot({ plot_cov_class_plot_out() })
  
  plot_3_exp_map_v2_plot_out <- reactive({ HRaDeX::plot_3_exp_map_v2(list_params()) })
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ plot_3_exp_map_v2_plot_out() })
  
  plot_n_plot_out <- reactive({ HRaDeX::plot_n(list_params()) })
  output[["plot_n_plot"]] <- renderPlot({ plot_n_plot_out() })
  
  plot_r2_hist_plot_out <- reactive({ HRaDeX::plot_r2_hist(list_params()) })
  output[["plot_r2_hist_plot"]] <- renderPlot({ plot_r2_hist_plot_out() })
  
  output[["params_list_data"]] <- DT::renderDataTable({ 
    
    # browser()
    
    tmp_dat <- dplyr::select(list_params(), -id)
    
      dplyr::mutate(tmp_dat, 
             n_1 = round(n_1, 3),
             k_1 = round(k_1, 3),
             n_2 = round(n_2, 3),
             k_2 = round(k_2, 3),
             n_3 = round(n_3, 3),
             k_3 = round(k_3, 3),
             r2 = round(r2, 3))
  })
  
  ##
  
  output[["download_fit_params_table"]] <- downloadHandler(
    filename = "fit_params_data.csv",
    content = function(file){
      write.csv(dplyr::select(list_params(), -id), file)
    }
  )
  
  ##
  
  fit_info_txt <- reactive({ HRaDeX::get_fit_values_info(list_params()) })
  
  output[["fit_info"]] <- renderText({ fit_info_txt() })
  
  ##
  
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
  
  ##
  
  output[["fit_report"]] <- downloadHandler(
    
    filename <- "HRaDeX_Report.html",
    content <- function(file) {
      rmarkdown::render(input = app_sys("app/report_template.Rmd"),
                        output_file = file, quiet = TRUE)
    }
  )
}
