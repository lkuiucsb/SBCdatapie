#' time.detector
#' 
#' This function takes character-class columns and attempts to coerce them into Date-class, if reasonable test whether a column conforms to standard date formats.
#' 
#' @usage
#'   time_detector(df)
#'
#' @param df
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'

time_detector <- function(df) {
  
  if(all(!as.vector(t(as.data.frame(sapply(df, class))[,1])) %in% c("Date", "POSIXc", "POSIXlt", "POSIXct"))) {
    for(i in 1:dim(df)[2]) {
      if(is.character(df[,i]) | is.factor(df[,i])) {
        
        var <- as.character(unique(df[,i]))
        formatted.date.col <- NA
        
        date.formats <- c("%d%b%Y", "%d%.%m%.%Y", "%m%.%d%.%Y", "%d%.%m%.%y", "%m%.%d%.%y", "%d%.%b%.%Y", "%b%.%d%.%Y", "%Y%.%m%.%d", "%Y%.%d%.%m", "%y%.%m%.%d", "%b %d, %Y")
        
        date.tests <- data.frame(matrix(NA, nrow = length(var), ncol = length(date.formats)), row.names = as.character(var))
        colnames(date.tests) <- date.formats
        
        date.tests2 <- as.data.frame(
          lapply(as.list(date.formats), function(x){
            ifelse(is.na(parse_date(var, x)), NA, 1)
          }))
        names(date.tests2) <- date.formats
        
        max.dates.coerced <- max(colSums(date.tests2), na.rm = T)
        coerced.date <- NA
        
        if(!is.infinite(max.dates.coerced)) {
          coerced.date <- parse_date(df[,i], date.formats[colSums(date.tests2) %in% max.dates.coerced])
        }
        
        df$coerced.date <- coerced.date
        
      } else {next}
    }
  }
  return(df)
}

