EDI_mainPanel <- function() {
  mainPanel(width = 6,
      tabsetPanel(
        type = "tabs",
        tabPanel("Raw Data", dataTableOutput("out_table")),
        tabPanel("Data summary", dataTableOutput("summary_table")),
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
        tabPanel("Quick Start",
                 p("This is place holder text")),
        tabPanel("About",
                 h3("EDI hackathon"),
                    p("This extends the ggplotgui to download data directly from the EDI repository and also has additional summary and visulization functions."),
                 h3("ggplotgui -Orginal Background"),
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
      )
  )
}