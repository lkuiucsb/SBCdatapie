#'
#' A wrapper function to generate data entity-level summary plus variable-level reports for one entities
#'
#' @param data_entity_list A metajam list output for a single data entity.
#'

make_entity_report <- function(data_entity_list) {
  
  begin_time <- Sys.time()
  
  # if there's missing codes specified in attribute metadata, then try and replace where these missing codes occur with NA's
  if ("attribute_metadata" %in% names(data_entity_list) & "missingValueCode" %in% names(data_entity_list[["attribute_metadata"]])) {
    data_entity_list <- use_missing_code(data_entity_list)
  }
  
  print_report <- function(i) {
    cat(i[["var_name"]])
    cat("\n ")
    i[["plots"]][["xsummary"]]
    cat("\n ")
    i[["plots"]][["x"]]
  }
  
  # append "File_Name" in summary metadata to report name
  report_name <-
    paste0("report_", data_entity_list[["summary_metadata"]][1, 2], ".html")
  
  # make entity-level report
  entity_data <- data_entity_list[["data"]]
  dt1 <- HackathonFunction1(entity_data)
  try(dt2 <- HackathonFunction2(entity_data))
  try(dt3 <- HackathonFunction3(entity_data))
  
  # detect spatial information
  space_cols <- space_detective(data_entity_list)
  
  
  # loop over columns in entity data and make variable-level reports
  var_report <- lapply(colnames(entity_data), make_var_report, entity_data = entity_data, space_cols = space_cols)
  names(var_report) <- colnames(entity_data)
  
  end_time <- Sys.time()

  # create environment for RMarkdown
  envir <-
    list(
      data_entity_list = data_entity_list,
      report_title = paste("Data report for", data_entity_list[["summary_metadata"]][1, 2]),
      df = entity_data,
      space_cols = space_cols,
      dt1 = dt1,
      dt2 = if (exists("dt2"))
        dt3
      else
        NULL,
      dt3 = if (exists("dt3"))
        dt3
      else
        NULL,
      print_report = print_report,
      var_report = var_report,
      begin_time = begin_time,
      end_time = end_time    
      )
  
  # set template path. Use the first one once this is in a package.
  # template_path <- system.file("rmd", "static_report_template.Rmd", package = "dummypackagename")
  template_path <- "./inst/rmd/static_report_template.Rmd"
  
  # set report output path. Outside of package context, this is in relation to the location of the static template
  report_output_path <- "../../output/"
  
  try(rmarkdown::render(
    input = template_path,
    output_format = "html_document",
    output_file = paste0(report_output_path, report_name),
    envir = envir
  ))
  #return(var_report)
  
  message(paste("Report generated."))
}