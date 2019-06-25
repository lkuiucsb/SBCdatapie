#'
#' Wrapper function to generate complete HTML report: entity-level  plus variable-level reports for one entity.
#'
#' @param data_entity_list (list) A list object containing information on a single data entity in metajam output format.
#' @param output_path (character) Path to save complete HTML report to. If NULL, will default to a RStudio pop-up window.
#'
#' @return A HTML file with complete static on the chosen data entity.

static_report_complete <- function(entity_list, output_path = NULL) {
  
  begin_time <- Sys.time()

  
  # append "File_Name" in summary metadata to report name
  report_name <-
    paste0("report_", entity_list[["summary_metadata"]][1, 2], ".html")
  
  # make entity-level report
  entity_df <- entity_list[["data"]]
  dt1 <- HackathonFunction1(entity_df)
  try(dt2 <- HackathonFunction2(entity_df))
  try(dt3 <- HackathonFunction3(entity_df))
  
  # detect spatial information
  space_cols <- space_detective(entity_list)
  
  
  # loop over columns in entity data and make variable-level reports
  var_report <- lapply(colnames(entity_df), static_report_variable, entity_df = entity_df, space_cols = space_cols)
  names(var_report) <- colnames(entity_df)
  
  end_time <- Sys.time()

  # create environment for RMarkdown
  envir <-
    list(
      entity_list = entity_list,
      report_title = paste("Data report for", entity_list[["summary_metadata"]][1, 2]),
      df = entity_df,
      space_cols = space_cols,
      dt1 = dt1,
      dt2 = if (exists("dt2"))
        dt2
      else
        NULL,
      dt3 = if (exists("dt3"))
        dt3
      else
        NULL,
      var_report = var_report,
      begin_time = begin_time,
      end_time = end_time    
      )
  
  # set template path. Use the first one once this is in a package.
  # template_path <- system.file("rmd", "static_report_template.Rmd", package = "dummypackagename")
  template_path <- "./inst/rmd/static_report_template.Rmd"
  
  # set report output path. Outside of package context, this is in relation to the location of the static template
  output_path <- "../../output/"
  
  try(rmarkdown::render(
    input = template_path,
    output_format = "html_document",
    output_file = paste0(output_path, report_name),
    envir = envir
  ))
  #return(var_report)
  
  message(paste("Report generated."))
}