remotes::install_github("clnsmth/metajam", build = TRUE)

unlink(
  paste0(tempdir(), '/data_package'),
  recursive = TRUE,
  force = TRUE
)

#https://gist.github.com/jcheng5/3830244757f8ca25d4b00ce389ea41b3
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
      ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

doi_obj <- 'doi:10.6073/pasta/b745934d136ce9ca8de26c5063eee86a'
source("C:/Projects/ggplotgui/R/read_data_archived.R")

#edi_obj <- read_data_archived(doi_obj)

library(shiny)
library(metajam)


ui <- fluidPage(
  #Where user inputs doi address
  tags$h4("Enter EDI Data Repository DOI"),
  textInput("doi", tags$em("e.g., doi:10.6073/pasta/7e48a6e1f..."),
    'doi:10.6073/pasta/b745934d136ce9ca8de26c5063eee86a'),
  #Button that initiates another function
  actionButton("fetch_button", "Fetch Data"),
  tags$br(),
  selectInput("repo_file", "Select file:", choices = "",
    selected = 'No files selected'),
  #textOutput("doi_text")
  #pre(id = "console_text"),
  dataTableOutput("repo_table")
)

server <- function(input, output, session) {
  
  # edi_download_wrapper <- function(x) {
  #   read_data_archived(x)
  # }
  

  #Makes a responsive output when the "Download" button is pushed 
  edi_object_reactive <- eventReactive(input$fetch_button, {
      read_data_archived(input$doi)
    }
  )

  
  observe({
    file_names_raw <- names(edi_object_reactive())
    file_names <- file_names_raw[-length(file_names_raw)]
    
    updateSelectInput(session, "repo_file",
      choices = c("No files selected", file_names),
      selected = 'No files selected')
  })
  
  list_shiny <- reactive({
    edi_object_reactive()[[input$repo_file]]
  })
  
  df_shiny <- reactive({
    edi_object_reactive()[[input$repo_file]]$data
  })
  
  output$repo_table <- renderDataTable({
    df_shiny()
  })

}

shinyApp(ui, server)
