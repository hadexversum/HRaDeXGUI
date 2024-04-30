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
    apply_ui_settings(),
    shinyjs::useShinyjs(),
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
            title = "Uptake calculations",
            fluidRow(
              column(width = 6, 
                     selectizeInput_h(
                       inputId = "fit_state",
                       label = "Select state: "
                     ),
                     checkboxInput_h(
                       inputId = "replicate",
                       label = "Use replicate data?",
                       value = FALSE
                     )),
              column(width = 6,
                     numericInput_h(inputId = "omit_residue",
                                    label = "Omit peptides first residues?",
                                    value = 0, 
                                    min = 0, max = 2)
                     )
            ),
            fancy_icon = "cogs"
          ),
          br(),
          ## moving the start of the sequence
          collapsible_card(
            title = "Sequence",init_collapsed = T,
            p("Sequence from the experimental data:"),
            textOutput("protein_sequence_file"),
            br(),
            p("Sequence from the PDB file: "),
            textOutput("protein_sequence_pdb"),
            br(),
            div(id = "part_alignement",
                p("Sequences alignement: "),
                verbatimTextOutput("pdb_file_alignement"),
                br()),
            numericInput(inputId = "protein_start",
                         label = "Move the start of the sequence?",
                         value = 1),
            div(id = "part_changed_sequence",
                p("Sequence after change:"),
                textOutput("sequence_file_moved"))
          ),
          br(),
          ## control settings
          collapsible_card(
            title = "Deuterium uptake controls",
            fluidRow(
              column(
                width = 6,
                super_selectInput(
                  inputId = "time_0",
                  label = "Deut 0%",
                  choices = c(0, 0.001, 0.167, 1),
                  selected = 0.001,
                  multiple = FALSE
                ),
                super_checkboxInput(
                  inputId = "fractional",
                  label = "Perform analysis on fractional values?",
                  value = TRUE
                )
              ),
              column(
                width = 6,
                div(
                  id = "fractional_part",
                  selectInput(
                    inputId = "time_100",
                    label = "Deut 100%",
                    choices = c(120, 150, 1440),
                    selected = 1440,
                    multiple = FALSE
                  ),
                  super_checkboxInput(
                    inputId = "is_FD",
                    label = "Use as FD control instead of time point?",
                    value = FALSE
                  )
                )

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
              selected =  321,
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
                             value = 3,
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
                             value = 0.3,
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
                             value = 0.0001,
                             min = 0.00001, max = 30, step = 0.01),
                numericInput(inputId = "k_slow_start",
                             label = "Initial value",
                             value = 0.03,
                             min = 0, max = 30, 0.01)

              )
            ),
            # plotOutput_h("k_params_plot"),
            p("The plot below shows the uptake curve based on the initial values with bounds for all the classes. It reflects the current state of the table above. "),
            hadex_with_spinner(uiOutput("k_params_plot")),
            p("Besides that, there are two extreme cases defined by the rule of thumb: immediate exchange (fire red), and no exchange (black). For more information see the documentation and help.  "),
            div(
              style = "display:inline-block; float:right",
              actionButton(inputId = "reset_class_def",
                         label = "Reset values",
                         icon = icon("broom"))
              ),
            br(),
            br(),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            title = "Fit options",
            numericInput(inputId = "fit_maxiter",
                         label = "Select number of iterations",
                         value = 100,
                         min = 10, max = 1000, step = 10),
            selectInput(inputId = "fit_scale",
                        label = "Select method",
                        choices = c("levenberg"),
                        selected = "levenberg"),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            title = "Structure",
            p("If the structure of the protein is available, upload the pdb file:"),
            fileInput(inputId = "pdb_file",
                      label = "PDB file: ",
                      accept = c(".pdb", ".cif")),
            fancy_icon = "cogs"
          ),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Overview",
              fluidPage(
                fluidRow(
                  column(
                    width = 9,
                    br(),
                    wellPanel(
                      radioButtons(inputId = "hires_method",
                                   label = "Select method of data aggregation:",
                                   choices = c("Shortest peptide" = "shortest",
                                               "Weighted values inverse to peptide length" = "weighted"))
                    ),
                    super_girafeOutput(outputId = "hires_plot_out", height = '90%'),
                    super_girafeOutput(outputId = "estimated_k_hires_plot_out", height = '90%'),
                    # plotOutput_h("hires_mono_plot_out"),
                    super_girafeOutput(outputId = "hires_components_plot_out", height = '90%'),
                    super_girafeOutput("plot_cov_class_plot", height = '90%'),
                    fluidRow(
                      column(
                        width = 4,
                        super_girafeOutput("plot_3_exp_map_v2_plot", height = '90%')
                      ),
                      column(
                        width = 4,
                        super_girafeOutput("plot_n_plot", height = '90%')
                      ),
                      column(
                        width = 4,
                        super_girafeOutput("plot_rss_hist_plot", height = '90%')
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
              "Hires params",
              br(),
              p("Table presents the classification results on residue level."),
              DT::dataTableOutput("hires_params_data"),
              downloadButton("download_hires_params_data", "Save table (.csv)"),
            ),
            tabPanel(
              "Fit Params",
              br(),
              p("Table presents the classification results on peptide level."),
              DT::dataTableOutput("params_list_data"),
              downloadButton("download_fit_params_table", "Save table (.csv)"),
              br(),
              br(),
              p("Click on the peptide in the table to see the uptake curves below!"),
              br(),
              splitLayout(
                ggiraph::girafeOutput("plot_selected_uc_1"),
                ggiraph::girafeOutput("plot_selected_uc_2")
              )

            ),
            tabPanel(
              "Plots",
              class = "HaDeX-tab-content-element",
              hadex_with_spinner(uiOutput("plot_fit_plots")),
              br()
            ),
            tabPanel(
              "Plots Data",
              DT::dataTableOutput("uc_data"),
              downloadButton("download_uc_table", "Save table (.csv)"),
              br()
            ),
            tabPanel(
              "Structure",
              br(),
              r3dmol::r3dmolOutput("protein_structure", width = "100%", height = "1000px"),
              p("To make the image of the structure, set the protein in desired position, stop the spinning and make a screen shot. ")
            ),
            tabPanel(
              "About", 
              br(),
              wellPanel(
                includeMarkdown(app_sys("app/man/about.md"))
              )
              
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

#' @noRd
apply_ui_settings <- function(){


}
