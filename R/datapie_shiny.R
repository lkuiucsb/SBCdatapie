#' Creating a graphical user interface for creating ggplot-graphs.
#' 
#' To run use \code{datapie_shiny()}
#'
#' @param dataset A dataset (optional).
#' @return A GUI for visualizing data from \code{dataset}.

#' @import ggplot2
#' @import shiny
#' @import shinyjs
#' @import readxl
#' @import haven
#' @import RColorBrewer
#' @import magrittr
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim
#' @export
datapie_shiny <- function( dataset = NA ) {
  
  ######### UI definition #############
  ui <- fluidPage(
    #Allows app to print messages to the console
    shinyjs::useShinyjs(),
    
    ###### Application title ########
    headerPanel("datapie"),
    
    ###### Left panel: loading data and main graphing options ########
    sidebarPanel(width = 3,
                 conditionalPanel(
                   condition = "input.tabs=='Raw Data'",
                   h4("Raw Data"),
                   radioButtons(
                     "data_input", "",
                     choices = list("Load sample data" = 1,
                                    "Fetch data from DOI" = 2,
                                    "Upload text file" = 3), #, "Paste text file" = 4), 
                     selected = 1),
                   conditionalPanel(
                     condition = "input.data_input=='1'",
                     h5("Sample dataset from library(datapie) is loaded."),
                     downloadButton("downloadData",
                                    "Download data")
                     ),
                   conditionalPanel(
                     condition = "input.data_input=='2'",
                     h5("Enter DOI"),
                     textInput("doi", "e.g., doi:10.56as4f980...", NULL),
                     actionButton("fetch_button", "Fetch Data"),
                     shinyFiles::shinyDirButton("dir", "Save Data", "Upload"),
                     textOutput("text"),
                     selectInput("repo_file", "Select file:", 
                                 choices = "",
                                 selected = "No file selected")
                     ),
                   conditionalPanel(
                     condition = "input.data_input=='3'",
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
                                   selected = "Semicolon")
                       ),
                     actionButton("submit_datafile_button",
                                  "Submit datafile")
                     )
                   ),
                 conditionalPanel(
                   condition = "input.tabs == 'Summary Report'",
                   h4("Overall and variable-by-variable summaries and plots"),
                   actionButton("generate_example_report", "Generate report"),
                   h4("If using DOI, remember to select a file. Now wait. Once report shows up you can view and click on download."),
                   downloadButton("download_report", "Download report (HTML)")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot'",
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
                   #textOutput("text_output"),
                   
                   conditionalPanel(
                     condition = "input.Type == 'Boxplot'", # "input.Type == 'Boxplot' || input.Type == 'Violin' || input.Type == 'Dot + Error'",
                     checkboxInput(inputId = "jitter",
                                   label = strong("Show data points (jittered)"),
                                   value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "input.Type == 'Scatter' || input.Type == 'Histogram'", # "input.Type == 'Density' || input.Type == 'Histogram'",
                     sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
                   ),
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
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'R-code'",
                   h4("R-code")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'Help'",
                   h4("Help")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'About'",
                   h4("About")
                 )
    ),
    
    ########Define Tabs#########
    mainPanel(width = 6,
              tabsetPanel(
                type = "tabs",
                tabPanel("Raw Data",
                  dataTableOutput("out_table"),
                  textOutput("message_text")),
                tabPanel("Summary Report", 
                         #dataTableOutput("summary_table"),
                         htmlOutput("report_html")
                         ),
                tabPanel("Plot",
                         mainPanel(
                           downloadButton("download_plot_PDF",
                                          "Download pdf of figure"),
                           
                           downloadButton("download_plot_Tiff",
                                          "Download tiff of figure"),
                           plotOutput("out_ggplot"))
                ),
                tabPanel("Interactive Plot", plotlyOutput("out_plotly")),
                tabPanel("R-code", verbatimTextOutput("out_r_code")),
                tabPanel("Help",
                         p("This is place holder text")),
                tabPanel("About",
                         h3("EDI hackathon"),
                         p("This extends the datapie to download data directly from the EDI repository and also has additional summary and visulization functions."),
                         h3("datapie -Orginal Background"),
                         p(
                           a("R", href = "https://www.r-project.org/"), "is amazing, but daunting
  for many. The programming style of R, compared to the point-and-click
  style of typical software, is a hurdle for many. Perhaps particularly so
  for those in the social sciences, whose statistical needs are often met by
  other software packages. Yet such packages are often very limited in terms
  of their options to visualize the data at hand. I believe that the amazing
  visualization-capabilities of R might be one way to get more people to use it.
  To lower the barrier to start using R, this package allows users to visualize
  their data using an online graphical user interface (GUI) that makes use of
  R's visualization package",
                           a("ggplot", href = "http://ggplot2.org/"),
                           ". There are two ways of using this functionality: 1) online, where users
  can upload their data and visualize it without needing R, by visiting ",
                           a("this website",
                             href = "https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/"),
                           "; 2) from within the R-environment (by using the ", code("ggplot_shiny()"),
                           "function). Importantly, the R-code will also be provided such that the user
  can recreate the graphs within the R-environment. The main aim (or hope) is
  to get more people using R and its wonderful (graphing) capabilities."
                         ),
                         h3("App info"),
                         p(
                           "This application was built in ",
                           a("R", href = "https://www.r-project.org/"),
                           "version 3.3.2, and uses the following packages: ",
                           a("ggplot2", href = "http://ggplot2.tidyverse.org/"), ",",
                           a("Shiny", href = "http://www.rstudio.com/shiny/"), ",",
                           a("stringr", href = "http://stringr.tidyverse.org/"), ",",
                           a("plotly", href = "https://plot.ly/r/"), ",",
                           a("readr", href = "http://readr.tidyverse.org/"), ",",
                           a("readxl", href = "http://readxl.tidyverse.org/"), ",",
                           a("haven", href = "http://haven.tidyverse.org/"), ", and",
                           a("RColorBrewer.", href = "http://stringr.tidyverse.org/")
                         ),
                         p(
                           "This application was created by ",
                           a("Gert Stulp", href = "http://www.gertstulp.com/"),
                           ". Please do report bugs and send feature requests to ",
                           a("g.stulp[at]rug.nl", href = "mailto:g.stulp@rug.nl"),
                           ". Visit ",
                           a("https://github.com/gertstulp/ggplotgui",
                             href = "https://github.com/gertstulp/ggplotgui"),
                           "for further description and code."
                         ),
                         h3("Acknowledgements"),
                         p(
                           "Thanks to Wilmer Joling for setting up the ",
                           a("website", href = "https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/"),
                           "which is based on the magical but incomprehensible",
                           a("docker", href = "https://www.docker.com/"),
                           ". Thanks to ",
                           a("Hadley Wicham", href = "http://hadley.nz/"),
                           " for making such good packages (and open access
  books describing them), that allow even low-skilled
  and low-talented programmers like myself to be able to
  contribute to R"
                         )
                ),
                id = "tabs"
              ),
              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                               tags$div("Loading...",id="loadmessage"))
    ), #close mainPanel
    
    ####### Right Panel ########
    conditionalPanel(
      condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot'",
      sidebarPanel(
        width = 3,
        h4("Change aesthetics"),
        tabsetPanel(
          tabPanel(
            "Text",
            checkboxInput(inputId = "label_axes",
                          label = strong("Change labels axes"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = "label x-axis")
            ),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_y", "Y-axis:", value = "label y-axis")
            ),
            checkboxInput(inputId = "add_title",
                          label = strong("Add title"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              textInput("title", "Title:", value = "Title")
            ),
            checkboxInput(inputId = "adj_fnt_sz",
                          label = strong("Change font size"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl",
                           "Size axis titles:",
                           value = 12),
              numericInput("fnt_sz_ax",
                           "Size axis labels:",
                           value = 10)
            ),
            checkboxInput(inputId = "rot_txt",
                          label = strong("Rotate text x-axis"),
                          value = FALSE),
            checkboxInput(inputId = "adj_fnt",
                          label = strong("Change font"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt == true",
              selectInput("font", "Font",
                          choices = c("Courier",
                                      "Helvetica",
                                      "Times"),
                          selected = "Helvetica")
            )
          ),
          tabPanel(
            "Theme",
            conditionalPanel(
              condition = "input.group != '.'",
              checkboxInput(inputId = "adj_col",
                            label = strong("Change colors"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.adj_col",
                selectInput(inputId = "palet",
                            label = strong("Select palette"),
                            choices = list(
                              "Qualitative" = c("Accent",
                                                "Dark2",
                                                "Paired",
                                                "Pastel1",
                                                "Pastel2",
                                                "Set1",
                                                "Set2",
                                                "Set3"),
                              "Diverging" = c("BrBG",
                                              "PiYG",
                                              "PRGn",
                                              "PuOr",
                                              "RdBu",
                                              "RdGy",
                                              "RdYlBu",
                                              "RdYlGn",
                                              "Spectral"),
                              "Sequential" = c("Blues",
                                               "BuGn",
                                               "BuPu",
                                               "GnBu",
                                               "Greens",
                                               "Greys",
                                               "Oranges",
                                               "OrRd",
                                               "PuBu",
                                               "PuBuGn",
                                               "PuRd",
                                               "Purples",
                                               "RdPu",
                                               "Reds",
                                               "YlGn",
                                               "YlGnBu",
                                               "YlOrBr",
                                               "YlOrRd")),
                            selected = "set1")
              )
            ),
            conditionalPanel(
              condition = "input.jitter",
              checkboxInput("adj_jitter",
                            strong("Change look jitter"), FALSE),
              conditionalPanel(
                condition = "input.adj_jitter",
                textInput("col_jitter", "Colour (name or RGB):",
                          value = "black"),
                numericInput("size_jitter", "Size:", value = 2),
                sliderInput("opac_jitter", "Opacity:",
                            min = 0, max = 1, value = 0.5, step = 0.01),
                sliderInput("width_jitter", "Width jitter:",
                            min = 0, max = 0.5, value = 0.25, step = 0.01)
              )
            ),
            checkboxInput("adj_grd",
                          strong("Remove gridlines"), FALSE),
            conditionalPanel(
              condition = "input.adj_grd",
              checkboxInput("grd_maj",
                            strong("Remove major gridlines"), FALSE),
              checkboxInput("grd_min",
                            strong("Remove minor gridlines"), FALSE)
            ),
            selectInput("theme", "Theme",
                        choices = c("bw" = "theme_bw()",
                                    "classic" = "theme_classic()",
                                    "dark" = "theme_dark()",
                                    "grey" = "theme_grey()",
                                    "light" = "theme_light()",
                                    "line_draw" = "theme_linedraw()",
                                    "minimal" = "theme_minimal()"),
                        selected = "theme_bw()")
          ),
          tabPanel(
            "Legend",
            conditionalPanel(
              condition = "input.group != '.'",
              radioButtons(inputId = "adj_leg",
                           label = NULL,
                           choices = c("Keep legend as it is",
                                       "Remove legend",
                                       "Change legend"),
                           selected = "Keep legend as it is"),
              conditionalPanel(
                condition = "input.adj_leg=='Change legend'",
                textInput("leg_ttl", "Title legend:",
                          value = "title legend"),
                selectInput("pos_leg", "Position legend",
                            choices = c("right",
                                        "left",
                                        "top",
                                        "bottom"))
              )
            )
          ),
          tabPanel(
            "Size",
            checkboxInput("fig_size",
                          strong("Adjust plot size on screen"), FALSE),
            conditionalPanel(
              condition = "input.fig_size",
              numericInput("fig_height", "Plot height (# pixels): ",
                           value = 480),
              numericInput("fig_width", "Plot width (# pixels):", value = 480)
            ),
            checkboxInput("fig_size_download",
                          strong("Adjust plot size for download"), FALSE),
            conditionalPanel(
              condition = "input.fig_size_download",
              numericInput("fig_height_download",
                           "Plot height (in cm):", value = 14),
              numericInput("fig_width_download",
                           "Plot width (in cm):", value = 14)
            )
          )
        ) # Close tabsetPanel
      ) # Close sidebarPanel
    ) # Close conditionalPanel
    
    #######
  ) #end fluidPanel
  
  ############Server function ############
  server <- shinyServer(function(input, output, session) {
  
  #####################################
  ######### GET DATA FROM DOI #########
  #####################################
    
    #Initialize the output that will be displayed using the "Fetch data..." option
    # values$shiny_data is the object that most downstream functions will want to
    # use
    values <- reactiveValues(shiny_data = NULL)
  
    #Make repo download available to downstream app tools
    list_shiny <- eventReactive(input$fetch_button, {
      #Allow messages to be printed to the console
      withCallingHandlers({
        #Initialize the package used to print messages
        shinyjs::html("message_text", "")
      #Read in data
      #In the future, a lot of this logic could be placed in the function
      # data_package_shiny_handler, thereby unifying the concepts expressed
      # below with said function.
        if(is.logical(all.equal(isolate(values$shiny_data), data_example))) {
          #This is required to make sure the app doesn't crash when the "Fetch
          # Data" button is pressed twice without an good doi.
          # cat("App condition 1\n") #Debugging
          data_list <- data_example
        } else if(!is.null(values$shiny_data)) {
          #The downloaded data is initially set to NULL, so it is easier to check
          # if 
          if(attr(isolate(values$shiny_data), "doi") == input$doi) {
            #Return the existing dataset when the same doi is input
            # cat("App condition 2A\n") #Debugging
           data_list <- isolate(values$shiny_data)
          } else if(is.null(input$doi) ||
              is.na(input$doi) ||
              nchar(input$doi) < 1){
            #Return the existing dataset when an invalid doi is entered
            # cat("App condition 2B\n") #Debugging
            data_list <- isolate(values$shiny_data)
          } else {
            #Otherwise download the data
            data_list <- data_package_shiny_handler(input$doi,
              isolate(values$shiny_data))
            # cat("App condition 2C\n") #Debugging
          }
        } else {
          # cat("App condition 3\n") #Debugging
          #If there is not data (NULL) then try to download the data package
          data_list <- data_package_shiny_handler(input$doi,
            isolate(values$shiny_data))
        }
      },
        #Generate the loading message
        message = function(m) {
          shinyjs::html(id = "message_text",
            html = paste(m$message, "<br>"),
            add = TRUE)
        })
      data_list
    }
      )
    
    #Update the values after the "Fetch data" button is pressed and the list_shiny
    # code is run.
    observeEvent(input$fetch_button, {
      #populate the object
      values$shiny_data <- list_shiny()
    })
    
    #Update the data package columns to be selected
    observe({
      #Extract the file names in the data package
      file_names <- names(list_shiny())
      
      #Use the file names to populate the dropdown list
      updateSelectInput(
        session,
        "repo_file",
        choices = c("No file selected", file_names),
        selected = 'No file selected')
    })
    
    ################################################
    ########## DOWNLOAD DATA PACKAGE ###############
    ################################################
    
    # Choose a local path to which the data package will be "downloaded" (the
    # data package is actually being copied form the tempdir() to a user 
    # specified location.
    shinyFiles::shinyDirChoose(input, 'dir', roots = c(home = '~'),
                               filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw"))
    global <- reactiveValues(datapath = getwd())
    dir <- reactive(input$dir)
    output$dir <- renderText({global$datapath})
    observeEvent(ignoreNULL = TRUE, eventExpr = {input$dir},
                 handlerExpr = {
                   if (!"path" %in% names(dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(dir()$path[-1]), 
                                           collapse = .Platform$file.sep))})
    text_reactive <- eventReactive(input$dir, {
      suppressMessages(data_package_copy(global$datapath))})
    output$text <- renderText({
      if (global$datapath != getwd()){
        msg <- text_reactive()
        if (isTRUE(msg)){
          'Download complete'
        } else {
          'Data package already exists'
        }
      }
    })
    
    ################################################
    ##### generate and show static report ##########
    ################################################
    
    
    get_report <-
      
      
      # do all this once "generate report" is clicked
      
      eventReactive(input$generate_example_report, {
        
        temp_output <- paste0(tempdir(), "/reports_output/")
        
        # if using sample data 
        
        if (input$data_input == 1) {
          report_filename <- paste0("report_", data_example[[1]][["summary_metadata"]][1, 2], ".html")
          
          # check for existing report 
          
          if (!file.exists(paste0(temp_output, report_filename))) {
            report_filename <-
              static_report_complete(
                entity_list = data_example[[1]],
                output_path = temp_output,
                shiny = T
              )
          }
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(paste0(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          return(includeHTML(paste0(temp_output, report_filename)))
          
          # if using data from DOI
          
        } else if (input$data_input == 2) {
          report_filename <- paste0(list_shiny()[[input$repo_file]][["summary_metadata"]][1, 2], ".html")
          if (!file.exists(paste0(temp_output, report_filename))) {
          entity_list <- list_shiny()[[input$repo_file]]
          report_filename <-
            try(static_report_complete(entity_list = entity_list,
                                       output_path = temp_output,
                                       shiny = T))
          }
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(paste0(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          return(includeHTML(paste0(temp_output, report_filename)))
          
          # if using uploaded data
          
        } else if (input$data_input == 3) {
          return("Sorry, we don't currently support report generation for user-uploaded data.")
        }
      })
    
    # render 
    
    output$report_html <- renderUI({
      get_report()
    })
    
  #####################################
  ### GET VARIABLE NAMES FOR INPUT ####
  #####################################
  
      observe({
        nms <- names(df_shiny())
        # Make list of variables that are not factors
        nms_cont <- names(Filter(function(x) is.integer(x) ||
                                   is.numeric(x) ||
                                   is.double(x),
                                 df_shiny()))
  
        # Make list of variables that are not factors
        nms_fact <- names(Filter(function(x) is.factor(x) ||
                                   is.logical(x) ||
                                   is.character(x),
                                 df_shiny()))
  
        avail_all <- c("No groups" = ".", nms)
        avail_con <-
          if (identical(nms_cont, character(0)))
            c("No continuous vars available" = ".")
          else c(nms_cont)
        avail_fac <-
          if (identical(nms_fact, character(0)))
            c("No factors available" = ".")
          else c("No groups" = ".", nms_fact)
  
        updateSelectInput(session, "y_var", choices = c("No y-var" = "' '", nms))
        updateSelectInput(session, "x_var", choices = nms)
        updateSelectInput(session, "group", choices = avail_all)
        updateSelectInput(session, "facet_row",  choices = avail_fac)
        updateSelectInput(session, "facet_col",  choices = avail_fac)
      })
  
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
      df_shiny <- reactive({
        if (input$data_input == 1) {
          data <- data_example[[1]]$data
        } else if (input$data_input == 2) {
          if(!exists("list_shiny")) {
            return(data.frame(x = "Enter DOI and press 'Fetch Data' button"))
          }
          else {
            data <- list_shiny()[[input$repo_file]]$data
          }
          
        } else if (input$data_input == 3) {
          file_in <- input$upload
          # Avoid error message while file is not uploaded yet
          if (is.null(input$upload)) {
            return(data.frame(x = "Select your datafile"))
          } else if (input$submit_datafile_button == 0) {
            return(data.frame(x = "Press 'submit datafile' button"))
          } else {
            isolate({
              if (input$file_type == "text") {
                data <- read_delim(file_in$datapath,
                                   delim = input$upload_delim,
                                   col_names = TRUE)
              } else if (input$file_type == "Excel") {
                data <- read_excel(file_in$datapath)
              } else if (input$file_type == "SPSS") {
                data <- read_sav(file_in$datapath)
              } else if (input$file_type == "Stata") {
                data <- read_dta(file_in$datapath)
              } else if (input$file_type == "SAS") {
                data <- read_sas(file_in$datapath)
              }
            })
          }
        } 
        #   else if (input$data_input == 3) {
        #   if (input$data_paste == "") {
        #     data <- data.frame(x = "Copy your data into the textbox,
        #                        select the appropriate delimiter, and
        #                        press 'Submit data'")
        #   } else {
        #     if (input$submit_data_button == 0) {
        #       return(data.frame(x = "Press 'submit data' button"))
        #     } else {
        #       isolate({
        #         data <- read_delim(input$data_paste,
        #                            delim = input$text_delim,
        #                            col_names = TRUE)
        #       })
        #     }
        #   }
        # } 
        #   else if (input$data_input == 4){
        #   data <- dataset
        # }
        return(data)
      })
  
  #####################################
  ####### CREATE GRAPH-CODE ###########
  #####################################
  
      string_code <- reactive({
  
        # Variable used for how to deal with x/y in ggplot
        gg_x_y <- input$Type == "Histogram" ||
                  input$Type == "Density"
        # Variable used for how to deal with color/fill
        gg_fil <- input$Type == "Histogram" ||
                  input$Type == "Density" ||
                  input$Type == "Dotplot"
  
        
        # Only plot jitter when graphs allow them
        if (gg_fil || input$Type == "Scatter")
          jitt <- FALSE else jitt <- input$jitter
        
        
        p <- paste(
          "ggplot(df, aes(",
            if(input$x_cast == 'character'){
              "x = as.character(input$x_var)"
            }else if(input$x_cast == 'numeric'){
              "x = as.numeric(input$x_var)"
            }else if(input$x_cast == 'Date'){
              "x = as.Date(input$x_var)"
            }else{
              "x = input$x_var"
            },
          if (!gg_x_y) {
            if(input$y_cast == 'character'){
              ", y = as.character(input$y_var)"
            }else if(input$y_cast == 'numeric'){
              ", y = as.numeric(input$y_var)"
            }else if(input$y_cast == 'Date'){
              ", y = as.Date(input$y_var)"
            }else{
              ", y = input$y_var"
            }
          },
          if (input$group != "." && gg_fil) {
            ", fill = input$group"
          } else if (input$group != "." && !gg_fil) {
            ", color = input$group"
          },
          ")) + ",
          if (input$Type == "Histogram")
            if(is.numeric(df_shiny()[,input$x_var])){
              paste("geom_histogram(alpha = input$alpha, ",
                    "binwidth = input$binwidth)", sep = "")
            }else{
              paste("geom_histogram(alpha = input$alpha, stat='count')", sep = "")
              },
          # if (input$Type == "Density")
          #   paste("geom_density(position = 'identity', alpha = input$alpha, ",
          #         "adjust = input$adj_bw)", sep = ""),
          if (input$Type == "Boxplot")
              "geom_boxplot()",
          #   "geom_boxplot(notch = input$notch)",
          # if (input$Type == "Violin")
          #   "geom_violin(adjust = input$adj_bw)",
          # if (input$Type == "Dotplot")
          #   paste("geom_dotplot(binaxis = 'y', binwidth = input$binwidth, ",
          #         "stackdir = 'input$dot_dir')", sep = ""),
          # if (input$Type == "Dot + Error")
          #   paste("geom_point(stat = 'summary', fun.y = 'mean') +\n  ",
          #         "geom_errorbar(stat = 'summary', fun.data = 'mean_se', ", "
          #         width=0, fun.args = list(mult = input$CI))", sep = ""),
          if (input$Type == "Scatter")
            "geom_point(alpha = input$alpha, size = 2)",
          if (input$Type == "Scatter" && input$line)
            "+ geom_smooth(se = input$se, method = 'input$smooth')",
          if (jitt)
            paste(" + geom_jitter(size = input$size_jitter, ",
                  "alpha = input$opac_jitter, width = input$width_jitter, ",
                  "color = 'input$col_jitter')", sep = ""),
          sep = ""
        )
  
        # if at least one facet column/row is specified, add it
        facets <- paste(input$facet_row, "~", input$facet_col)
        if (facets != ". ~ .")
          p <- paste(p, "+ facet_grid(", facets, ")")
  
        # if labels specified
        if (input$label_axes)
          p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")
  
        # if title specified
        if (input$add_title)
          p <- paste(p, "+ ggtitle('input$title')")
  
        # if legend specified
        if (input$adj_leg == "Change legend")
          p <- paste(p, "+ labs(",
                     if (gg_fil) "fill" else "color",
                     " = 'input$leg_ttl')",
                     sep = "")
  
        # if color legend specified
        if (input$adj_col)
          p <- paste(p, "+ scale_",
                     if (gg_fil) "fill" else "color",
                     "_brewer(palette = 'input$palet')",
                     sep = "")
  
        # If a theme specified
        p <- paste(p, "+", input$theme)
  
        # If theme features are specified
        if (input$adj_fnt_sz ||
            input$adj_fnt ||
            input$rot_txt ||
            input$adj_leg != "Keep legend as it is" ||
            input$adj_grd) {
          p <- paste(
            p,
            paste(
              " + theme(\n    ",
              if (input$adj_fnt_sz)
  "axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
              if (input$adj_fnt_sz)
  "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
              if (input$adj_fnt)
  "text = element_text(family = 'input$font'),\n    ",
              if (input$rot_txt)
  "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
              if (input$adj_leg == "Remove legend")
  "legend.position = 'none',\n    ",
              if (input$adj_leg == "Change legend")
  "legend.position = 'input$pos_leg',\n    ",
              if (input$grd_maj)
  "panel.grid.major = element_blank(),\n    ",
              if (input$grd_min)
  "panel.grid.minor = element_blank(),\n    ",
  ")",
              sep = ""
            ),
            sep = ""
          )
        }
  
        # Replace name of variables by values
        p <- str_replace_all(
               p,
               c("input\\$y_var" = input$y_var,
                 "input\\$y_cast" = input$y_cast,
                 "input\\$x_var" = input$x_var,
                 "input\\$x_cast" = input$x_cast,
                 "input\\$group" = input$group,
                 # "input\\$notch" = as.character(input$notch),
                 "input\\$binwidth" = as.character(input$binwidth),
                 # "input\\$adj_bw" = as.character(input$adj_bw),
                 # "input\\$dot_dir" = as.character(input$dot_dir),
                 "input\\$alpha" = as.character(input$alpha),
                 "input\\$se" = as.character(input$se),
                 "input\\$smooth" = as.character(input$smooth),
                 # "input\\$CI" = as.character(input$CI),
                 "input\\$size_jitter" = as.character(input$size_jitter),
                 "input\\$width_jitter" = as.character(input$width_jitter),
                 "input\\$opac_jitter" = as.character(input$opac_jitter),
                 "input\\$col_jitter" = as.character(input$col_jitter),
                 "input\\$lab_x" = as.character(input$lab_x),
                 "input\\$lab_y" = as.character(input$lab_y),
                 "input\\$title" = as.character(input$title),
                 "input\\$palet" = as.character(input$palet),
                 "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
                 "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
                 "input\\$font" = as.character(input$font),
                 "input\\$leg_ttl" = as.character(input$leg_ttl),
                 "input\\$pos_leg" = as.character(input$pos_leg))
        )
        # Creates well-formatted R-code for output
        p <- str_replace_all(p, ",\n    \\)", "\n  \\)")
  
        p
      })
  
  
  #####################################
  ###### GRAPHICAL/TABLE OUTPUT #######
  #####################################
  
      output$out_table <- renderDataTable(
        df_shiny()
      )
      
      output$summary_table <- renderDataTable(
        df_shiny() %>%
          dplyr::select_if(is.numeric) %>%
          tidyr::gather(key="column_name", value='value') %>%
          dplyr::group_by(column_name) %>%
          dplyr::summarise(min=min(value, na.rm=TRUE), 
                    quatile_25 = quantile(value, 0.25, na.rm=TRUE),
                    quatile_50 = quantile(value, 0.50, na.rm=TRUE),
                    quatile_75 = quantile(value, 0.75, na.rm=TRUE),
                    max = max(value, na.rm=TRUE), 
                    unique_count = length(unique(value)),
                    finite_count = sum(is.finite(value)),
                    count = length(value))
      )
  
      width <- reactive({ input$fig_width })
      height <- reactive({ input$fig_height })
      width_download <- reactive({ input$fig_width_download })
      height_download <- reactive({ input$fig_height_download })
  
      output$out_ggplot <- renderPlot(width = width,
                                      height = height, {
        # evaluate the string RCode as code
        df <- get_subset()
        p <- eval(parse(text = string_code()))
        p
      })
  
      output$out_plotly <- renderPlotly({
        # evaluate the string RCode as code
        df <- df_shiny()
        p <- eval(parse(text = string_code()))
        ggplotly(p)
      })
  
  #####################################
  #### GENERATE R-CODE FOR OUTPUT #####
  #####################################
  
      output$out_r_code <- renderText({
        
        # data sample r-code
        if(input$data_input == 1) {
          gg_code <- string_code()
          gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")
          
          paste(
            "## You can use the code below to make the 'Plot' tab figure.\n\n",
            "# You will need the following package(s):\n",
            "library(\"datapie\")\n",
            "library(\"ggplot2\")\n\n",
            "# The code below will load the sample data from\n",
            "# library(datapie) into your current R session.\n",
            "# df <- datapie::data_example\n\n",
            "# The code below will generate the 'Plot' tab figure.\n",
            "graph <- ",
            gg_code,
            "\ngraph\n\n",
            "# If you want the plot to be interactive,\n",
            "# you need the following package(s):\n",
            "library(\"plotly\")\n\n",
            "# The code below will generate the 'Interactive Plot'\n",
            "# tab figure.\n",
            "ggplotly(graph)\n\n",
            "# The code below will save your plot.\n",
            "ggsave('my_graph.pdf', graph, width = ",
            width_download(),
            ", height = ",
            height_download(),
            ", units = 'cm')",
            sep = ""
          )
        }
        
        # doi data r-code
        else if (input$data_input == 2) {
          gg_code <- string_code()
          gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")
          
          paste(
            "## You can use the code below to make the 'Plot' tab figure.\n\n",
            "# You will need the following package(s):\n",
            "library(\"datapie\")\n",
            "library(\"ggplot2\")\n\n",
            "# The code below will download data from a DOI to a temporary\n",
            "# directory on your computer. The DOI in the code below is\n",
            "# the DOI you entered in the 'Raw Data' tab.\n",
            "datapie::data_package_download(data.pkg.doi = '",
            input$doi,
            "')\n\n",
            "# The code below will save the data from the the DOI you\n",
            "# entered in the 'Raw Data' tab to your current R session.\n",
            "df_list <- datapie::data_package_read()\n\n",
            "# The code below will create a variable df to hold the data\n",
            "# file you selected in the 'Raw Data' tab.\n",
            "df <- df_list$",
            input$repo_file,
            "$data\n\n",
            "# The code below will generate the 'Plot' tab figure:\n",
            "graph <- ",
            gg_code,
            "\ngraph\n\n",
            "# If you want the plot to be interactive,\n",
            "# you need the following package(s):\n",
            "library(\"plotly\")\n\n",
            "# The code below will generate the 'Interactive Plot'\n",
            "# tab figure.\n",
            "ggplotly(graph)\n\n",
            "# The code below will save your plot.\n",
            "ggsave('my_graph.pdf', graph, width = ",
            width_download(),
            ", height = ",
            height_download(),
            ", units = 'cm')",
            sep = ""
          )
        }
        
        # uploaded data r-code
        else {
          paste("We do not support R-code generation from uploaded\n",
                "data at this time.")
          }
        })

  #####################################
  #### Download codes #################
  #####################################
  
    output$download_plot_PDF <- downloadHandler(
        filename <- function() {
          paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
        },
        content <- function(file) {
          df <- df_shiny()
          p <- eval(parse(text = string_code()))
          ggsave(file, p, width = width_download(),
                 height = height_download(), units = "cm")
        },
        contentType = "application/pdf" # MIME type of the image
    )
    output$download_plot_Tiff <- downloadHandler(
        filename <- function() {
          paste("Figure_ggplotGUI_", Sys.time(), ".tiff", sep = "")
        },
        content <- function(file) {
          df <- df_shiny()
          p <- eval(parse(text = string_code()))
          ggsave(file, p, width = width_download(),
                 height = height_download(), units = "cm")
        },
        contentType = "application/tiff" # MIME type of the image
      )
     output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(data, con)
       }
     )
  ###############################
  ### scale bar #################
  ###############################
  
  output$data_range <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$x_var))
      return()
  
    # Get the data set with the selected column
    df<-df_shiny()
    
    if (input$x_var!="") {
    df1 <- unlist(df[,input$x_var])
  
      if (!is.character(df1)&input$x_cast!="character") {
        sliderInput("range", "Range of interest:", min = min(df1), max = max(df1), value = c(min(df1),max(df1)))
        } else {
           h5("No scale bar for categorical variable")
      }
    } else {return()}
  })
  
  ####################################
  ####subset data from scale bar ####
  #####################################
     
    get_subset <- reactive({
       
       min_value <- input$range[1] 
       max_value <- input$range[2]
       
       df <- df_shiny() 
  
        if (!is.null(min_value)) {
        df2<-df %>% filter(df[,input$x_var]>=min_value&df[,input$x_var]<=max_value)
        } else {
          df2<-df
        }
       df2
     })
     
  #for debugging purpose, uncomment the
     # output$text_output <- renderText({
     #   min_value <- input$range[1]
     #   max_value <- input$range[2]
     #   if (!is.null(min_value)) {
     #      paste("range is", min_value)
     #      } else {
     #        "no range"
     #      }
     # })
  
     # End R-session when browser closed
     session$onSessionEnded(stopApp)
  })
  
  #####Construct the shinyApp#####
  shinyApp(ui, server)
}
