#' inventory_conveyances.R
#' 
#' Inventory SWUDS network conveyances 
#' 
#' 
#' @param s_wuds dataframe, the swuds water use data

#' @param
#' 
#' @examples 
#' s_wuds <- swuds_sample #example data from Ohio
#' 
#' filtered_inventoried_conveyances(filtered)
#' 
#' 
#' 
#'# The filter.R function will be used to filter to a subset of sites for quantile.R process.
#'# filtered <- filter(attributes-conditions)
#'   
#'   # All Public Supply intake data in Delaware County n 1,368
#'   df1 <- s_wuds[which(s_wuds$FROM_COUNTY_NM == "Delaware County"),]
#'   filtered <- df1[which(df1$FROM_NAT_WATER_USE_CD == "WS"),]
#'   
#'   # All Public Supply intake data n 37,584
#'   filtered <- s_wuds[which(s_wuds$FROM_NAT_WATER_USE_CD == "WS"),]
#'   
#'   # All sectors n 96,876 intakes
#'   filtered <- s_wuds
#'   
#' library(dplyr)
#' library(janitor)
#'  

filtered_inventoried_conveyances <- function(filtered) { 
  
    # Define conveyance types as from-site types --> to-site types
  filtered$conveyance_types <- paste(filtered$FROM_SITE_TP_CD," --> ",filtered$To_Site_Type_Code,sep="",collapse = NULL)
  
  # 4 conveyance_types for PS
  # 12 conveyance_types for all sectors

  filtered_inventoried_cntypes <- filtered %>% group_by(conveyance_types) %>% tally() %>% adorn_totals()
  
  
 return(filtered_inventoried_cntypes)
}