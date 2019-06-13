EDI_leftPanel <- function(dataset = NA) {
  sidebarPanel(width = 3,
      conditionalPanel(
        condition = "input.tabs=='Raw Data'",
        h4("Raw Data"),
        radioButtons(
          "data_input", "",
          choices = if (is.data.frame(dataset)) {
            list("Load sample data" = 1,
                 "Upload text file" = 2,
                 "Paste data" = 3,
                 "Data passed through R environment" = 4)
              } else {
              list("Load sample data" = 1,
                   "Upload file" = 2,
                   "Paste data" = 3)
              },
          selected = if (is.data.frame(dataset)) 4 else 1),
        conditionalPanel(
          condition = "input.data_input=='1'",
          h5("dataset 'mpg' from library(ggplot2) loaded"),
          downloadButton("downloadData",
                         "Download data"))
          ),
        conditionalPanel(
          condition = "input.data_input=='2'",
          h5("Upload file: "),
          fileInput("upload", "", multiple = FALSE),
          selectInput("file_type", "Type of file:",
                      list("text (csv)" = "text",
                           "Excel" = "Excel",
                           "SPSS" = "SPSS",
                           "Stata" = "Stata",
                           "SAS" = "SAS"),
                      selected = "text"),
          conditionalPanel(
            condition = "input.file_type=='text'",
            selectInput("upload_delim", "Delimiter:",
                        list("Semicolon" = ";",
                             "Tab" = "\t",
                             "Comma" = ",",
                             "Space" = " "),
                        selected = "Semicolon")),
          actionButton("submit_datafile_button",
                       "Submit datafile"),

        conditionalPanel(
          condition = "input.data_input=='3'",
          h5("Paste data below:"),
          tags$textarea(id = "data_paste",
                        placeholder = "Add data here",
                        rows = 10,
                        cols = 20, ""),
          actionButton("submit_data_button", "Submit data"),
          selectInput("text_delim", "Delimiter:",
                      list("Semicolon" = ";",
                           "Tab" = "\t",
                           "Comma" = ",",
                           "Space" = " "),
                      selected = "Semicolon")
        )
      ),
      conditionalPanel(
        condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot' ||
                    input.tabs=='R-code'",
        h4("Create visualization"),
        selectInput(inputId = "Type",
                    label = "Type of graph:",
                    choices = c("Boxplot", "Histogram", "Scatter"), # c("Boxplot", "Density", "Dot + Error", "Dotplot", "Histogram", "Scatter", "Violin"),
                    selected = "Histogram"),
        selectInput("x_var", "X-variable", choices = ""),
        selectInput("x_cast", "X-coerce", choices = c('default','character', 'numeric', 'date')),
      
        conditionalPanel(condition = "input.Type!='Histogram'", # "input.Type!='Density' && input.Type!='Histogram'",
          selectInput("y_var", "Y-variable", choices = ""),
          selectInput("y_cast", "Y-coerce", choices = c('default', 'character', 'numeric', 'date'))
                  ),
        conditionalPanel(condition = "input.Type =='Histogram'", # "input.Type!='Density' && input.Type!='Histogram'",
              p("No y-variable relevant")
            ),
        selectInput("group", "Group (or color)", choices = ""),
        selectInput("facet_row", "Facet Row", choices = ""),
        selectInput("facet_col", "Facet Column", choices = ""),
        
        
        uiOutput("data_range"),
        
        conditionalPanel(
          condition = "input.Type == 'Boxplot'", # "input.Type == 'Boxplot' || input.Type == 'Violin' || input.Type == 'Dot + Error'",
          checkboxInput(inputId = "jitter",
                        label = strong("Show data points (jittered)"),
                        value = FALSE)
        ),
        # conditionalPanel(
        #   condition = "input.Type == 'Boxplot'",
        #   checkboxInput(inputId = "notch",
        #                 label = strong("Notched box plot"),
        #                 value = FALSE)
        # ),
        conditionalPanel(
          condition = "input.Type == 'Scatter' || input.Type == 'Histogram'", # "input.Type == 'Density' || input.Type == 'Histogram'",
          sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
        ),
        
        
        #need to fix the binwidth, bins, pad because server doesn't read the value correctly. 
        # conditionalPanel(
        #   condition = "input.Type == 'Histogram'", # "input.Type == 'Histogram' || input.Type=='Dotplot'",
        #   numericInput("binwidth", "Binwidth:", value = 1)
        # ),
        # conditionalPanel(
        #   condition = "input.Type == 'Dotplot'",
        #   selectInput("dot_dir", "Direction stack:",
        #               choices = c("up", "down", "center", "centerwhole"),
        #               selected = "up")
        # ),
        # conditionalPanel(
        #   condition = "input.Type == 'Density' || input.Type == 'Violin'",
        #   sliderInput(inputId = "adj_bw",
        #               label = "Bandwidth adjustment:",
        #               min = 0.01, max = 2, value = 1, step = 0.1)
        # ),
        conditionalPanel(
          condition = "input.Type == 'Scatter'",
          checkboxInput(inputId = "line",
                        label = strong("Show regression line"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.line == true",
            selectInput("smooth", "Smoothening function",
                        choices = c("lm", "loess", "gam"))
          ),
          conditionalPanel(
            condition = "input.line == true",
            checkboxInput(inputId = "se",
                          label = strong("Show confidence interval"),
                          value = FALSE)
          )
        )
        # ,
        # conditionalPanel(
        #   condition = "input.Type == 'Dot + Error'",
        #   selectInput("CI", "Confidence Interval:",
        #               choices = c("68% (1 SE)" = 1,
        #                           "90%" = 1.645,
        #                           "95%" = 1.96,
        #                           "99%" = 2.575),
        #               selected = 1.96)
        # )
      ),
      conditionalPanel(
        condition = "input.tabs == 'About'",
        h4("About")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Data Summary'",
        h4("Summary of numerical values")
      )
    )
}
