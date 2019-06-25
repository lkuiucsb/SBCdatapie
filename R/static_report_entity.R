#'
#' Generate entity-level report.
#'
#' @param entity_df (data.frane) (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#'
#' @return A list containing entity-level report items.
#'
#' @export
#'

static_report_entity <- function(entity_df) {
  list(numvars = SummaryTable1(entity_df),
       catvars = tryCatch({SummaryTable3(entity_df)}, error = function(cond){return(NULL)}),
       levels = tryCatch({SummaryTable2(entity_df)}, error = function(cond){return(NULL)})
  )
}