#'
#' A wrapper function to generate data entity-level summary plus variable-level reports for one entities
#' 
#' @param data_entity_list A metajam list output for a single data entity.
#'

make_entity_report <- function(data_entity_list) {
  report_name <-
    paste0("report_", data_entity_list[["summary_metadata"]][1, 2], ".html")
  df <- data_entity_list[["data"]]
  dt1 <- HackathonFunction1(df)
  space_cols <- space_detective(data_entity_list)
  try(dt2 <- HackathonFunction2(df))
  try(dt3 <- HackathonFunction3(df))
  var_report <- lapply(names(df), make_var_report)
  envir <-
    list(
      report_title = paste("Data report for", data_entity_list[["summary_metadata"]][1, 2]),
      df = df,
      space_cols = space_cols,
      dt1 = dt1,
      dt2 = dt2,
      dt3 = if(exists("dt3")) dt3 else NULL,
      var_report = var_report
    )
  
  
  try(rmarkdown::render(
    input = "./R/static_report_template.Rmd",
    output_format = "html_document",
    output_file = report_name,
    envir = envir
  ))
  return(envir)
  
  message(paste("Report generated."))
}