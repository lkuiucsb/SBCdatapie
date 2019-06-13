#'
#' A wrapper function to generate data entity-level summary plus variable-level reports for one entities
#' 
#' @param data_entity_list A metajam list output for a single data entity.
#'

make_entity_report <- function(data_entity_list) {
  report_name <- paste0("report_",data_entity_list[["summary_metadata"]][["file_name"]], ".html")
  
  rmarkdown::render(
    input = "./static_report_template.Rmd",
    output_format = "html_document",
    output_file = report_name,
    envir = list(data_entity_list = data_entity_list)
  )
  
  message(paste("Report generated."))
}