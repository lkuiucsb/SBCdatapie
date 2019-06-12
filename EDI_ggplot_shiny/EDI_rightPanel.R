EDI_rightPanel <- function(){
  
    conditionalPanel(
      condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot' ||
                    input.tabs=='R-code'",
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
}