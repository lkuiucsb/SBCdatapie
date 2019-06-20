make_var_report <- function(varname) {
  var <- df[[varname]]
  plots <- if (is.numeric(var)) {
    #print(varname[1])
    xsummary <- make_numeric_variable_summary_df(var)
    print(kable(xsummary))
    #pander(xsummary)
    x <- make_numeric_histogram(df, var, varname)
    return(x)
  } else if (is.factor(var) | is.character(var)) {
    #print(varname[1])
    xsummary <- make_cat_variable_summary_df(var)
    print(kable(xsummary, format = "pandoc"))
    #pander(xsummary)
    x <- make_categorical_histogram(df, var, varname)
    return(x)
  }
  missing <- if (sum(xsummary$num_missing) > 0) {
    #print("View where missing values occur in the data")
    x <- make_missing_plot(var)
    return(x)
  }
  # spatial heatmap function for variable
  
  space <- space_plot(space_cols, varname)
  
  return(list(plots, missing, space))
}