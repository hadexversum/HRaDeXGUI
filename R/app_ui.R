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
          br(),
          img(src='./www/logo.png', width = "40%", align = "center"),
          br(),
          br(),
          mod_input_data_ui("input_data"),
          mod_fit_parameters_ui("fit_parameters")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Overview",
              fluidPage(
                fluidRow(
                  column(
                    width = 9, 
                    plotOutput("plot_cov_class_plot")
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    plotOutput("get_params_summary_image_plot"),
                    plotOutput("plot_start_params_plot")),
                  column(
                    width = 3, 
                    plotOutput("plot_3_exp_map_v2_plot"),
                    plotOutput("plot_n_plot")
                  ),
                  column(
                    width = 3, 
                    plotOutput("plot_r2_hist_plot")
                  )
                )
              )
              ),
            tabPanel(
              "Params",
              DT::dataTableOutput("params_list_data")
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
