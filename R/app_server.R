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

  p_times <- reactive(unique(dat()[["Exposure"]]))

  observe({
    updateSelectInput(
      session,
      inputId = "time_0",
      choices = p_times(),
      selected = min(p_times())
    )
  })
  
  observe({
    updateSelectInput(
      session,
      inputId = "time_100",
      choices = p_times(),
      selected = max(p_times())
    )

  })
  
  observe({
    
    input[["reset_class_def"]]
    
    updateNumericInput(inputId = "k_fast_upper",
                       value = 30) 
    updateNumericInput(inputId = "k_fast_lower",
                       value = 1)
    updateNumericInput(inputId = "k_fast_start",
                       value = 1)
    updateNumericInput(inputId = "k_medium_upper",
                       value = 1)
    updateNumericInput(inputId = "k_medium_lower",
                       value = 0.1)
    updateNumericInput(inputId = "k_medium_start",
                       value = 0.1)
    updateNumericInput(inputId = "k_slow_upper",
                       value = 0.1)
    updateNumericInput(inputId = "k_slow_lower",
                       value = 0.0001)
    updateNumericInput(inputId = "k_slow_start",
                       value = 0.01)
  })
  
  ##
  
  output[["k_params_plot"]] <- renderUI({
    
    renderPlot({
      HRaDeX::plot_start_params(params_fixed())
    })
    
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
  
  use_fractional <- eventReactive(input[["do_run"]],{
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    input[["fractional"]]
    
  })
  
  ##
  
  use_replicate <- eventReactive(input[["do_run"]],{
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    
    input[["replicate"]]
    
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

  p_time_100 <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    as.numeric(input[["time_100"]])
    
  })
  
  p_time_0 <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    as.numeric(input[["time_0"]])
    
  })
  
  fd_check <- eventReactive(input[["do_run"]], {
    
    validate(need(input[["do_run"]]>0, "Initiate analysis by clicking the button."))
    input[["is_FD"]]
    
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

  observe({
    if(p_time_0() != as.numeric(input[["time_0"]])) params_ready(-1)
  })
  
  observe({
    if(p_time_100() != as.numeric(input[["time_100"]])) params_ready(-1)
  })
  
  observe(
    if(input[["is_FD"]] != fd_check()) params_ready(-1)
  )
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

  observe({
    if(use_fractional() != input[["fractional"]]) params_ready(-1)
  })
  
  observe({
    if(use_replicate() != input[["replicate"]]) params_ready(-1)
  })
  
  ##
  
  output[["run_status"]] <- renderText({

    mes <- ""

    if(input[["do_run"]] == 0) mes <- "Press the button to initiate the process."

    if(params_ready() == 1) mes <- ""

    if(params_ready() == -1) mes <- "The parameters changed. Please press the button to rerun analysis."

    mes

  })

  observe({
    if(all(params_fixed() == fit_k_params()) & input[["fit_maxiter"]] == fit_control()[["maxiter"]] & input[["fit_scale"]] == fit_control()[["scale"]] & input[["type"]] == workflow_type() & fit_state() == input[["fit_state"]] & p_time_0() == as.numeric(input[["time_0"]]) & p_time_100() == as.numeric(input[["time_100"]]) & input[["is_FD"]] == fd_check() & use_fractional() == input[["fractional"]] & use_replicate() == input[["replicate"]]) params_ready(1)
  })

  ## checks


  kin_dat <- reactive({

    validate(need(input[["do_run"]] > 0, "Run the analysis by pressing the button on the left."))
    validate(need(fit_state() %in% unique(dat()[["State"]]), "New file detected, rerun the analysis by pressing the button."))
    # validate(need(state_ok() == 1, "Please confirm state changes."))
    
    # browser()

    message("Creating kinetic data")
    message(paste0("Protein: ", dat()[["Protein"]][[1]]))
    message(paste0("State: ", fit_state())) #s_fit_state %()% state))

    HRaDeX::prepare_kin_dat(dat(),
                            state = fit_state(), #s_fit_state %()% state,
                            time_0 = p_time_0(),
                            time_100 = p_time_100(),
                            replicate = use_replicate(),
                            FD = fd_check())



  })

  list_params <- reactive({

    message("Creating fit dataset")
    message(paste0("Workflow type ", workflow_type()))
    message("Fit params:")
    message(fit_k_params())
    
    # browser()

    HRaDeX::create_fit_dataset(kin_dat(),
                               control = fit_control(),
                               fit_k_params = fit_k_params(),
                               trace = F,
                               fractional = use_fractional(),
                               workflow = workflow_type())
  })

  
  hires_params <- reactive({
    
    HRaDeX::calculate_hires(list_params(),
                            fractional = use_fractional())

  })
  
  hires_plot <- reactive({
    
    HRaDeX::plot_hires(hires_params())
    
  })
  
  hires_components_plot <- reactive({
    
    HRaDeX::plot_hires_components(hires_params(), 
                                  fractional = use_fractional())
  })
  
  ###################
  ## TAB: OVERVIEW ##
  ###################
  
  output[["hires_plot_out"]] <- renderPlot({ hires_plot() })
  
  output[["hires_components_plot_out"]] <- renderPlot({ hires_components_plot() })

  plot_cov_class_plot_out <- reactive({ HRaDeX::plot_cov_class(list_params(), fractional = use_fractional()) })
  output[["plot_cov_class_plot"]] <- renderPlot({ plot_cov_class_plot_out() })
  
  plot_3_exp_map_v2_plot_out <- reactive({ HRaDeX::plot_3_exp_map_v2(list_params()) })
  output[["plot_3_exp_map_v2_plot"]] <- renderPlot({ plot_3_exp_map_v2_plot_out() })
  
  plot_n_plot_out <- reactive({ HRaDeX::plot_n(list_params(), fractional = use_fractional()) })
  output[["plot_n_plot"]] <- renderPlot({ plot_n_plot_out()})
  
  plot_rss_hist_plot_out <- reactive({ HRaDeX::plot_rss_hist(list_params()) })
  output[["plot_rss_hist_plot"]] <- renderPlot({ plot_rss_hist_plot_out() })
  
  fit_info_txt <- reactive({ HRaDeX::get_fit_values_info(list_params()) })

  output[["fit_info"]] <- renderText({ fit_info_txt() })

  ##
  
  output[["fit_report"]] <- downloadHandler(

    filename <- "HRaDeX_Report.html",
    content <- function(file) {
      rmarkdown::render(input = app_sys("./app/report_template.Rmd"),
                        output_file = file, quiet = TRUE)
    }
  )
  
  #################
  ## TAB: PARAMS ##
  #################
  
  output[["params_list_data"]] <- DT::renderDataTable({
    
    tmp_dat <- dplyr::select(list_params(), -id)
    
    dplyr::mutate(tmp_dat,
                  n_1 = round(n_1, 3),
                  k_1 = round(k_1, 3),
                  n_2 = round(n_2, 3),
                  k_2 = round(k_2, 3),
                  n_3 = round(n_3, 3),
                  k_3 = round(k_3, 3),
                  rss = round(rss, 4))
  })
  
  output[["download_fit_params_table"]] <- downloadHandler(
    filename = "fit_params_data.csv",
    content = function(file){
      write.csv(dplyr::select(list_params(), -id), file)
    }
  )
  
  ################
  ## TAB: PLOTS ##
  ################
  
  output[["plot_fit_plots"]] <- renderUI({
    
    lapply(1:nrow(list_params()),  function(i){
      
      fit_dat <- dplyr::filter(kin_dat(),
                               Sequence == list_params()[i, "sequence"],
                               Start == list_params()[i, "start"])
      
      fit_values <- list_params()[i, ]
      
      renderPlot(HRaDeX::plot_uc_fit(fit_dat,
                                     fit_values,
                                     fractional = use_fractional(),
                                     replicate = use_replicate()))
    })
  })
  
  #####################
  ## TAB: PLOTS DATA ##
  #####################
  
  output[["uc_data"]] <- DT::renderDataTable({
    
    tmp_dat <- dplyr::mutate(kin_dat(),
                      frac_deut_uptake = round(frac_deut_uptake, 4),
                      deut_uptake = round(deut_uptake, 4),
                      err_frac_deut_uptake = round(err_frac_deut_uptake, 4),
                      err_deut_uptake = round(err_deut_uptake, 4))
    
    dplyr::arrange(tmp_dat, Start, End, Exposure)
    
  })
  

  ##
  
  output[["download_uc_table"]] <- downloadHandler(
      filename = "uc_data.csv",
      content = function(file){
        write.csv(kin_dat(), file)
      }
  )
  
}
