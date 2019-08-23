#' quantiles.R
#' 
#' Select desired quartile or decile of monthly rows of filtered data based on ANNUAL_VAL
#' 
#' @param s_wuds dataframe, the swuds water use data
#' @param quant chr, quantile either quartile or decile
#' @param segment chr, segment of quartiles or deciles to use: 1 is segment with smallest ANNUAL_VAL
#' @param
#' 
#' @examples 
#' s_wuds <- swuds_sample #example data from Ohio
#' 
#' quant <- "quartile"
#' segment <- "1"
#' 
#' quant <- "decile"
#' segment<- "10"
#' 
#' 
#' testquant <-filtered_quantiles_segment(filtered, quant, segment)
#' 
#' 
#'# The filter.R function will be used to filter to a subset of sites for quantile.R process.
#'# filtered <- filter(attributes-conditions)
#'   
#'   df1 <- s_wuds[which(s_wuds$FROM_COUNTY_NM == "Delaware County"),]
#'   filtered <- df1[which(df1$FROM_NAT_WATER_USE_CD == "WS"),]
#'   
#' library(dplyr)
#'
#'  

filtered_quantiles_segment <- function(filtered, quant, segment) 
 
  #  Add mean of ANNUAL_VAL by FROM_SITE_NO for quantile evaluation, join to filtered
  #  s_wuds_sitemean <- s_wuds %>% group_by(FROM_SITE_NO) %>% dplyr::mutate(sitemeanANNUAL_VAL = mean(ANNUAL_VAL))
  #  s_wuds_sitemeanb <- subset(s_wuds_sitemean, select = c(FROM_SITE_NO, dec_date, sitemeanANNUAL_VAL))
  #  df2 <- merge(filtered, s_wuds_sitemeanb, all.x=TRUE) # Use this if want percentiles based on mean of all ANNUAL_VAL
  
  
    if (quant == "quartile"){    # this percentile based on ANNUAL_VAL:
      
      filtered_quantiles <- filtered %>%       
        mutate(quartile = ntile(ANNUAL_VAL, 4))
   
      filtered_quantiles_segment <-  filtered_quantiles[which(filtered_quantiles$quartile == segment),]
      
    } else if (quant == "decile"){
      filtered_quantiles <- filtered %>% 
        mutate(decile = ntile(ANNUAL_VAL, 10))
      filtered_quantiles_segment <-  filtered_quantiles[which(filtered_quantiles$decile == segment),]
    }
 