#' time_series_sites_annually.R
#' 
#' Plot water use annually (ANNUAL_VAL) overlaying multiple sites 
#' 
#' # The filter.R function will be used to filter to a subset of sites
#'
#' 
#' @param s_wuds dataframe, the swuds water use data 
#' @param 
#' @param 
#' @param 
#' @param 
#' 
#' @export
#' @import ggplot2 scales
#' 
#' @examples 
#' s_wuds <- swuds_sample #example data from Ohio
#' 
#' year1 <- 2001
#' year2 <- 2010
#' years <- c(year1, year2)
#' 

#' 
#'  df1 <- s_wuds[which(s_wuds$FROM_COUNTY_NM == "Delaware County"),]
#'   filtered <- df1[which(df1$FROM_NAT_WATER_USE_CD == "WS"),]
#'
#'  
#' testp3 <- time_series_sites_annually(filtered, year1, year2)
#' 
#' library(ggplot2)
#' library(scales)
#' 
#' 
#'
#' 
time_series_sites_annually <- function(filtered,year1,year2) { 
  
 # will call filter.R eventually 
  
 # filtered <- filter(attributes-conditions)
  
  filtered_range<-filtered[which(filtered$YEAR >= year1 & filtered$YEAR <= year2),]
  

  label2 <- c("Annual Use for Selected Sites, in MGD")
  
  p3<-ggplot(filtered_range, aes_string(x="YEAR", y="ANNUAL_VAL", col="FROM_STATION_NM")) + scale_x_continuous(breaks= pretty_breaks()) + geom_point() + geom_line() + labs(title=label2) 
  plot(p3)
  
}
