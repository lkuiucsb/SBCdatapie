make_var_report <- function(space_cols = space_cols, entity_data, varname) {
  
  var <- entity_data[[varname]]
  
  if (is.numeric(var)) {
    xsummary <- make_numeric_variable_summary_df(var)
    x <- make_numeric_histogram(entity_data, var, varname)
    plots <- list(xsummary=xsummary, x=x)
  } else if (is.factor(var) | is.character(var)) {
    xsummary <- make_cat_variable_summary_df(var)
    x <- make_categorical_histogram(entity_data, var, varname)
    plots <- list(xsummary=xsummary, x=x)
  } else {
    plots <- NULL
  }
  
  try(missing <- if (sum(plots$xsummary$num_missing) > 0) {
    make_missing_plot(var)
  })
  # spatial heatmap function for variable
  
  space <- space_plot(space_cols = space_cols, df = entity_data, var = varname)
  
  var_output <- list(
    var_name = varname,
    plots = plots,
    missing = if (exists("missing"))
      missing
    else NULL,
    space = if (exists("space"))
      space
    else NULL
    )
  
  return(var_output)
}