#'
#' A wrapper function to generate data entity-level summary plus variable-level reports for one entities
#'
#' @param data_entity_list A metajam list output for a single data entity.
#'

make_entity_report <- function(data_entity_list) {
  begin_time <- Sys.time()
  try(if ("attribute_metadata" %in% names(data_entity_list) & "missingValueCode" %in% names(data_entity_list[["data"]])) {
    data_entity_list <- use_missing_code(data_entity_list)
  })
  
  print_report <- function(i) {
    cat(i[["var_name"]])
    cat("\n ")
    i[["plots"]][["xsummary"]]
    cat("\n ")
    i[["plots"]][["x"]]
  }
  
  report_name <-
    paste0("report_", data_entity_list[["summary_metadata"]][1, 2], ".html")
  entity_data <- data_entity_list[["data"]]
  dt1 <- HackathonFunction1(entity_data)
  space_cols <- space_detective(data_entity_list)
  try(dt2 <- HackathonFunction2(entity_data))
  try(dt3 <- HackathonFunction3(entity_data))
  var_report <- lapply(colnames(entity_data), make_var_report, entity_data = entity_data, space_cols = space_cols)
  names(var_report) <- colnames(entity_data)
  end_time <- Sys.time()

  
  envir <-
    list(
      data_entity_list = data_entity_list,
      report_title = paste("Data report for", data_entity_list[["summary_metadata"]][1, 2]),
      df = entity_data,
      space_cols = space_cols,
      dt1 = dt1,
      dt2 = dt2,
      dt3 = if (exists("dt3"))
        dt3
      else
        NULL,
      print_report = print_report,
      var_report = var_report,
      begin_time = begin_time,
      end_time = end_time    
      )
  
  
  try(rmarkdown::render(
    input = "./R/static_report_template.Rmd",
    output_format = "html_document",
    output_file = report_name,
    envir = envir
  ))
  #return(var_report)
  
  message(paste("Report generated."))
}