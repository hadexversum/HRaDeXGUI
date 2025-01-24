#' input_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @details This is module to upload the data. HDeXaminer is not supported.
#'
#' @importFrom shiny NS tagList
mod_input_data_ui <- function(id){
  ns <- NS(id)

  wellPanel(
    # fillRow(
      # flex = c(NA, 1),
      super_fileInput(
        inputId = ns("data_file"),
        label = "Choose file:",
        multiple = FALSE,
        accept = c(".csv", ".xlsx", ".xls"),
        placeholder = "No file selected"
      ),
      div(
        id = "HaDeX-file-status-panel",
        h6("File status:"),
        div(
          id = "HaDeX-file-status-message",
          verbatimTextOutput(ns("data_file_info"))
        )
      )
    # )
  )

}

#' input_data Server Functions
#'
#' @noRd
mod_input_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    shinyhelper::observe_helpers(help_dir = "/tmp/gui/helpfiles/", withMathJax = TRUE)

    dat_raw <- reactive({
      data_file <- input[["data_file"]]

      message <- "Check file requirements!"
      
      if (is.null(data_file)) {
        alpha_dat
      } else {
    
        tryCatch({
          x_file <- HaDeX::read_hdx(data_file[["datapath"]])
        }, error = function(e){
          message <<- e[["message"]]
            }, silent = TRUE
        )
        
        validate(need(exists("x_file"), message))
        x_file
        
      }
    })

    
    data_source <- reactive({ attr(dat_raw(), "source") })

    file_protein <- reactive({ unique(dat_raw()[["Protein"]])})
    
    output[["data_file_info"]] <- renderText({
      paste0(
        if (is.null(input[["data_file"]]))
          "Example file: alpha.csv"
        else "Supplied file is valid.",
        "\nFound protein: ", file_protein(), 
        "\nDetected data source: ", data_source()
      )
    })

  dat_cleaned <- reactive({
    
    dplyr::mutate(dat_raw(),
                  Exposure = round(Exposure, 3))
    
  })
  
  dat <- reactive( dat_cleaned() ) # ignoring hdexaminer for now

  return(dat)

})

}

## To be copied in the UI
# mod_input_data_ui("input_data_1")

## To be copied in the server
# mod_input_data_server("input_data_1")
