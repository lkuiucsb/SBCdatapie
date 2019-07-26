#' freqclocks_forPOSIX
#' 
#' This function creates ggplot clocks of the frequency of data occurring at different time frequencies, if POSIX type data.
#' 
#' @usage
#'   freqclocks_forPOSIX(df, var, varname)
#'
#' @param data.df3
#'   Define this parameter.
#'
#' @return
#'   (ggplot clocks) Of the frequency of data occurring at different time frequencies, if POSIX type data.
#' 
#' @export
#'

freqclocks_forPOSIX <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- tidyr::gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = long.df[long.df$count %in% TRUE,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Coverage") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    theme(legend.position = "top") + 
    scale_x_datetime(labels=date_format("%b-%Y"))
  
  # plot how much data occus in each hour of the day
  hour.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = hour(long.df[long.df$count %in% TRUE,1]), group = column.name)) + 
    theme_bw() +
    geom_histogram(bins = 24, aes(fill = column.name)) + 
    coord_polar() +
    ylab("Data Coverage") + xlab("Hour of day") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = months(long.df[long.df$count %in% TRUE,1], T), group = column.name)) + 
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) + 
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  both.clocks <- plot_grid(month.clock, hour.clock,
                           nrow=1, align = "h", rel_widths = c(1,1))
  POSIX.plots <- plot_grid(freq.timeline, both.clocks,
                           ncol=1, align = "v", rel_widths = c(1,1))
  return(three.plots)
}