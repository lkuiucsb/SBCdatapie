#' freqclocks_forDates
#' 
#' This function creates ggplot clocks of the frequency of data occurring at different time frequencies, if Date type data.
#' 
#' @usage
#'   freqclocks_forDates(df, var, varname)
#'
#' @param df
#'   Define this parameter.
#' @param var
#'   Define this parameter.
#' @param varname
#'   Define this parameter.
#'   
#' @return
#'   (ggplot clocks) Of the frequency of data occurring at different time frequencies, if Date type data.
#' 
#' @export
#'

freqclocks_forDates <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- tidyr::gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = long.df[long.df$count %in% TRUE,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Coverage") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    #theme(legend.position = "top") + 
    scale_x_date(labels=date_format("%b-%Y"))
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = months(long.df[long.df$count %in% TRUE,1], T), group = column.name)) + 
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) + 
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  Date.plots <- plot_grid(freq.timeline, month.clock,
                          ncol=1, align = "v", rel_widths = c(3,1))
  return(list(freq.timeline, month.clock))
}