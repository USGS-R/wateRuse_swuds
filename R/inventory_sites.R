#' inventory_sites.R
#' 
#' Inventory SWUDS network sites,optionally by water-use sector
#' 
#' 
#' @param s_wuds dataframe, the swuds water use data
#' @param filtered dataframe, filtered s_wuds

#' 
#' @examples 
#' s_wuds <- swuds_sample #example data from Ohio
#' 
#' filtered_inventoried_sites(filtered, bysector=FALSE)
#' filtered_inventoried_sites(filtered, bysector=TRUE)
#' 
#'# The filter.R function will be used to filter to a subset of sites for processing.
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

filtered_inventoried_sites <- function(filtered, bysector){
  if (bysector==TRUE){ 
  filtered_inventoried_fromsites <- filtered %>% group_by(FROM_NAT_WATER_USE_CD, FROM_SITE_TP_CD) %>% tally()
  filtered_inventoried_tosites <- filtered %>% group_by(To_National_Water_Use_Code, To_Site_Type_Code) %>% tally()
  
  filtered_inventoried_fromsites$From_sites <- filtered_inventoried_fromsites$n
  filtered_inventoried_tosites$To_sites <- filtered_inventoried_tosites$n
  
  filtered_inventoried_fromsites$Site_Type_Code <- filtered_inventoried_fromsites$FROM_SITE_TP_CD
  filtered_inventoried_tosites$Site_Type_Code <- filtered_inventoried_tosites$To_Site_Type_Code
  
  #  join these with one column listing site types: Site_Type_Code
 
  siteinventory <- dplyr::full_join(filtered_inventoried_fromsites, filtered_inventoried_tosites, by = "Site_Type_Code")
  siteinventoryout <- subset(siteinventory, select=c(FROM_NAT_WATER_USE_CD, Site_Type_Code, From_sites, To_National_Water_Use_Code, To_sites)) %>% adorn_totals() 
  } else {
  filtered_inventoried_fromsites <- filtered %>% group_by(FROM_SITE_TP_CD) %>% tally()
  filtered_inventoried_tosites <- filtered %>% group_by(To_Site_Type_Code) %>% tally()
  
  filtered_inventoried_fromsites$From_sites <- filtered_inventoried_fromsites$n
  filtered_inventoried_tosites$To_sites <- filtered_inventoried_tosites$n
  
  filtered_inventoried_fromsites$Site_Type_Code <- filtered_inventoried_fromsites$FROM_SITE_TP_CD
  filtered_inventoried_tosites$Site_Type_Code <- filtered_inventoried_tosites$To_Site_Type_Code
  
  #  join these with one column listing site types: Site_Type_Code
                 
    siteinventory <- dplyr::full_join(filtered_inventoried_fromsites, filtered_inventoried_tosites, by = "Site_Type_Code")
    siteinventoryout <- subset(siteinventory, select=c(Site_Type_Code, From_sites, To_sites)) %>% adorn_totals()
}
return(siteinventoryout)
}

