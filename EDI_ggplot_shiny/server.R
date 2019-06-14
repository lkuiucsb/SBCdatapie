#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
#####################################
######### GET DATA FROM DOI #########
#####################################
  
  # make full edi output available to downstream app tools
  list_shiny <- reactive({
    if (input$data_input == 2) {
      read_data_archived(input$doi)
    } else if (!is.null(list_shiny)){
      list_shiny
    } else {
      NULL
    }
  })
  
#   if (input$data_type == 2) {
#     # making edi data into reactive object
#     edi_object_reactive <- eventReactive(input$fetch_button, {
#       # wrapping edi read_data_archived function
#       edi_download_wrapper <- function(x) {
#         read_data_archived(x)
#         }
#       edi_download_wrapper(input$doi)
#     })
#     
    # watch file selection
    observe({
      file_names_raw <- names(list_shiny())
      file_names <- file_names_raw[-length(file_names_raw)]

      updateSelectInput(session, "repo_file", choices = c("No file selected", file_names), selected = 'No file selected')
    })
# }
   
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
        data <- ggplot2::mpg
      } else if (input$data_input == 2) {
        if(is.null(input$repo_data)) {
          return(data.frame(x = "Fetch your DOI"))
        } else if (input$fetch_button == 0) {
          return(data.frame(x = "Press 'Fetch Data' button"))
        } else {
          isolate({
            data <- list_shiny()[[input$repo_file]]
          })
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

    width <- reactive ({ input$fig_width })
    height <- reactive ({ input$fig_height })
    width_download <- reactive ({ input$fig_width_download })
    height_download <- reactive ({ input$fig_height_download })

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

      gg_code <- string_code()
      gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")

      paste(
        "## You can use the below code to generate the graph.\n",
        "## Don't forget to replace the 'df' with the name\n",
        "## of your dataframe\n\n",
        "# You need the following package(s):\n",
        "library(\"ggplot2\")\n\n",
        "# The code below will generate the graph:\n",
        "graph <- ",
        gg_code,
        "\ngraph\n\n",
        "# If you want the plot to be interactive,\n",
        "# you need the following package(s):\n",
        "library(\"plotly\")\n",
        "ggplotly(graph)\n\n",
        "# If you would like to save your graph, you can use:\n",
        "ggsave('my_graph.pdf', graph, width = ",
        width_download(),
        ", height = ",
        height_download(),
        ", units = 'cm')",
        sep = ""
      )

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
  
  df1 <- unlist(df[,input$x_var])

  if (!is.character(df1)) {
  sliderInput("range", "Range of interest:", min = min(df1), max = max(df1), value = c(min(df1),max(df1)))
  } else {
    h5("No scale bar for categorical variable")
  }
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
#shinyApp(ui, server)


