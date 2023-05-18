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
          mod_input_data_ui("input_data"),
          span("Be careful! This app is still under development.", style="color:red"),
          br(),
          br(),
          ## state settings
          mod_settings_state_ui("fit_state", "SINGLE"),
          br(),
          mod_settings_workflow_ui("workflow"),
          br(),
          mod_settings_class_definition_ui("class_definition"),
          br(),
          mod_settings_fit_ui("settings_fit_control")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Overview",
              fluidPage(
                fluidRow(
                  column(
                    width = 9,
                    plotOutput_h("plot_cov_class_plot")
                  ),
                  column(
                    width = 3, 
                   
                    br(),
                    br(),
                    img(src='./www/rgb_class.png', width = "100%",  align = "center")
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    plotOutput_h("plot_3_exp_map_v2_plot")
                  ),
                  column(
                    width = 3,
                    plotOutput_h("plot_n_plot")
                  ),
                  column(
                    width = 3,
                    plotOutput_h("plot_r2_hist_plot")
                  )
                ),
                fluidRow(
                  column(
                    width = 9, 
                    verbatimTextOutput("fit_info")
                  )
                )
              )
              ),
            tabPanel(
              "Params",
              DT::dataTableOutput("params_list_data")
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
