#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      br(),
      sidebarLayout(
        sidebarPanel(
          class = "HaDeX-tab-content-element",
          br(),
          img(src='./www/logo.png', width = "40%", align = "center"),
          br(),
          br(),
          ## input data
          mod_input_data_ui("input_data"),
          span("Be careful! This app is still under development.", style="color:red"),
          br(),
          br(),
          ## run app
          fluidPage(
            fluidRow(
              column(width = 4,
                     actionButton(inputId = "do_run",
                                  label = "Calculate things!",
                                  icon = icon("wand-magic-sparkles"))),
              column(width = 8,
                     verbatimTextOutput("run_status"),
                     tags$style(type = "text/css", "#run_status {white-space: pre-wrap}"))
            )
          ),
          br(),
          ## state settings
          # mod_settings_state_ui("fit_state", "SINGLE"),
          collapsible_card(
            title = "State",
            selectizeInput_h(
              inputId = "fit_state",
              label = "Select state"
            ),
            fancy_icon = "cogs"
          ),
          br(),
          ## control settings
          collapsible_card(
            title = "Time controls",
            splitLayout(
              selectInput(
                inputId = "time_0",
                label = "Deut 0%",
                choices = c(0, 0.001, 0.167, 1),
                selected = 0.001,
                multiple = FALSE
              ),
              selectInput(
                inputId = "time_100",
                label = "Deut 100%",
                choices = c(120, 150, 1440),
                selected = 1440,
                multiple = FALSE
              )
            ),
            fancy_icon = "cogs"
          ),
          br(),
          ## workflow settings
          collapsible_card(
            title = "Workflow",
            selectInput(
              inputId = "type",
              label = "Select desired workflow",
              choices = c("3exp/1exp" = 31, "2exp/1exp" = 21, "3exp/2exp/1exp" = 321),
              selected = "3exp/2exp/1exp",
              multiple = FALSE
            ),
            fancy_icon = "cogs"
          ),
          br(),
          
          ## class settings
          collapsible_card(
            title = "Class definition",
            splitLayout(
              div(
                span("Fast exchange", style="color:red"),
                numericInput(inputId = "k_fast_upper",
                             label = "Upper boundary",
                             value = 30,
                             min = 0, max = 30, step = 0.5),
                numericInput(inputId = "k_fast_lower",
                             label = "Lower boundary",
                             value = 1,
                             min = 0, max = 30, step = 0.5),
                numericInput(inputId = "k_fast_start",
                             label = "Initial value",
                             value = 1,
                             min = 0, max = 30, step = 0.5)
              ),
              div(
                span("Medium exchange", style="color:green"),
                numericInput(inputId = "k_medium_upper",
                             label = "Upper boundary",
                             value = 1,
                             min = 0, max = 30, step = 0.1),
                numericInput(inputId = "k_medium_lower",
                             label = "Lower boundary",
                             value = 0.1,
                             min = 0, max = 30, step = 0.1),
                numericInput(inputId = "k_medium_start",
                             label = "Initial value",
                             value = 0.1,
                             min = 0, max = 30, step = 0.1),
              ),
              div(
                span("Slow exchange", style="color:blue"),
                numericInput(inputId = "k_slow_upper",
                             label = "Upper boundary",
                             value = 0.1,
                             min = 0, max = 30, step = 0.01),
                numericInput(inputId = "k_slow_lower",
                             label = "Lower boundary",
                             value = 0,
                             min = 0, max = 30, step = 0.01),
                numericInput(inputId = "k_slow_start",
                             label = "Initial value",
                             value = 0.01,
                             min = 0, max = 30, 0.01)
                
              )
            ),
            plotOutput_h("k_params_plot"),
            p("Besides that, there are two extreme cases defined by the rule of thumb: immediate exchange (fire red), and no exchange (black). For more information see the documentation and help.  "),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            title = "Fit options",
            numericInput(inputId = "fit_maxiter",
                         label = "Select number of iterations",
                         value = 200, 
                         min = 10, max = 1000, step = 10),
            selectInput(inputId = "fit_scale",
                        label = "Select method",
                        choices = c("levenberg"),
                        selected = "levenberg"),
            fancy_icon = "cogs"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Overview",
              fluidPage(
                fluidRow(
                  column(
                    width = 9,
                    plotOutput_h("plot_cov_class_plot"),
                    fluidRow(
                      column(
                        width = 4,
                        plotOutput_h("plot_3_exp_map_v2_plot")
                      ),
                      column(
                        width = 4,
                        plotOutput_h("plot_n_plot")
                      ),
                      column(
                        width = 4,
                        plotOutput_h("plot_r2_hist_plot")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 9, 
                        verbatimTextOutput("fit_info")
                      )
                    )
                  ),
                  column(
                    width = 3, 
                   
                    br(),
                    br(),
                    img(src='./www/rgb_plaster.png', width = "80%",  align = "left")
                  )
                ),
                
                downloadButton(
                  outputId = "fit_report",
                  label = "Create report",
                  icon = icon("fas fa-download")
                )
              )
              ),
            tabPanel(
              "Params",
              DT::dataTableOutput("params_list_data"),
              downloadButton("download_fit_params_table", "Save table (.csv)")
            ),
            tabPanel(
              "Plots",
              class = "HaDeX-tab-content-element",
              hadex_with_spinner(uiOutput("plot_fit_plots"))
            )
          )
        )
      )


    ),

    mod_create_fit_ui("create_fit")
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "utils",
    app_sys("app/utils")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HRaDeXGUI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
