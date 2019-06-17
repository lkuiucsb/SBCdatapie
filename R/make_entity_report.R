#'
#' A wrapper function to generate data entity-level summary plus variable-level reports for one entities
#' 
#' @param data_entity_list A metajam list output for a single data entity.
#'

make_entity_report <- function(data_entity_list) {
  report_name <-
    paste0("report_", data_entity_list[["summary_metadata"]][["file_name"]], ".html")
  

  
  envir <-
    list(
      data_entity_list = data_entity_list,
      df = time.detector(data_entity_list[["data"]]),
      space_cols = space_detective(data_entity_list),
      dt1 = HackathonFunction1(df),
      dt2 = HackathonFunction2(df),
      dt3 = HackathonFunction3(df),
      var_report = lapply(names(df), make_var_report)
    )
  
  
  rmarkdown::render(
    input = "./R/static_report_template.Rmd",
    output_format = "html_document",
    output_file = report_name,
    envir = envir
  )
  
  message(paste("Report generated."))
}