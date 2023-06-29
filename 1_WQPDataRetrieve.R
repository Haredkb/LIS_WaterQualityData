## LIS Legacy DataBase Creation
## Reading EPA Water Quality Portal (WQP)

#https://www.waterqualitydata.us//#bBox=-73.926%20%20%2C40.691%20%2C-71.072%20%20%2C45.302&siteType=Stream&siteType=Aggregate%20surface-water-use&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
 #https://waterdata.usgs.gov/blog/dataretrieval/
#Library
library(dataRetrieval)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(sp)
library(sf)

#Create Bounding Box for LIS Study area
LISS_poly <- st_read("data/LIS_watershed_GEO.shp")
BB <- round(st_bbox(LISS_poly),3)
poly_crs <- st_crs(LISS_poly)

N.names <-  c("Nitrate")
#HUC <- "0108*" #I think long island sounds HUC (*wild card) another way to filter

#Find what sites (SW and all others) within LIS bounding box have nitrate samples. 
NitrateSites <- whatWQPsites(bBox=BB,
                          characteristicName = N.names)# "Nitrate")


#Trim the site extent to true LIS watershed, as whatWQPsites can only use bounding box 
sf::sf_use_s2(FALSE)
# WQP_sites <- #LISS_WQP %>%
#   NitrateSites %>%
#   st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs =4269)%>%
#   dplyr::filter(MonitoringLocationTypeName == "Stream" | MonitoringLocationTypeName == "River/Stream")%>%
#   st_intersection(., LISS_poly)#trim from boundingbox to true watershed shape

#save the LIS WQP sites
saveRDS(WQP_sites, "WQP_SW_sites.RDS")

#API codes
#https://www.waterqualitydata.us/#bBox=-73.926%2C40.691%2C-71.072%2C45.302%20&siteType=Aggregate%20surface-water-use&siteType=Stream&siteType=Spring&huc=0108*&characteristicName=Nitrate&characteristicName=Nitrate-N&characteristicName=Nitrate-Nitrite&characteristicName=Nitrate-nitrogen&characteristicName=Nitrate%20as%20N&characteristicName=Nitrate-Nitrogen&characteristicName=Nitrate%20%2B%20Nitrite&characteristicName=Inorganic%20nitrogen%20(nitrate%20and%20nitrite)&characteristicName=Inorganic%20nitrogen%20(nitrate%20and%20nitrite)%20as%20N&characteristicName=Inorganic%20nitrogen%20(nitrate%20and%20nitrite%20and%20ammoni&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
#https://www.waterqualitydata.us/ogcservices/wfs/?request=GetFeature&service=wfs&version=2.0.0&typeNames=wqp_sites&SEARCHPARAMS=bBox%3A-73.926%2C40.691%2C-71.072%2C45.302%20%3BsiteType%3AAggregate%20surface-water-#data get
#https://www.waterqualitydata.us/ogcservices/wfs/?request=GetFeature&service=wfs&version=2.0.0&typeNames=wqp_sites&SEARCHPARAMS=bBox%3A-73.926%2C40.691%2C-71.072%2C45.302%20%3BsiteType%3AAggregate%20surface-water-use%7CStream%7CSpring%3Bhuc%3A0108*%3BsampleMedia%3AWater%7Cwater%3Bproviders%3ANWIS%7CSTEWARDS%7CSTORET&outputFormat=application%2Fjson

#Find what parameters exist at these stations 
LISS_nwis_para_wqp <- WQP_sites %>%
  group_by(CharacteristicName)%>%
  summarise(n_samples = n(),
            n_sites = length(unique(MonitoringLocationIdentifier)))

write.csv(LISS_nwis_para_wqp, "data/LIS_WQP_ParameterCounts.csv")


################################################
## Data Download
################################################
library(magrittr)
library(dataRetrieval)

N_WQP_sf <- NitrateSites %>%
  st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = poly_crs)%>%
  st_intersection(., LISS_poly)

#List the Types of Location Data
unique(N_WQP_sf$MonitoringLocationTypeName)

#GW Sites with Nitrogen
N_WQP_gw <- N_WQP_sf %>%
  dplyr::filter(grepl('Well', MonitoringLocationTypeName))

sites_WQP_gw <- unique(N_WQP_gw$MonitoringLocationIdentifier)

#WQP Stream Sites
river_names <- c("River/Stream", "Stream", "Stream: Canal", "Stream: Tidal stream", "Stream: Ditch", "Wetland Riverine-Emergent")

N_WQP_sf %<>%
  dplyr::filter(MonitoringLocationTypeName %in% river_names)
#check output type 
unique(N_WQP_sf$MonitoringLocationTypeName)

#List of sites with N data 
sites_WQP <- unique(N_WQP_sf$MonitoringLocationIdentifier)

###########################
## Pull Nitrogen Sites ###
###########################
N.names <-  c("Ammonia",
              "Ammonia-nitrogen",
              "Ammonium",
              "Nitrate",
              "Nitrate + Nitrite",
              "Nitric oxide",
              "Nitrite",
              "Nitrofen",
              "Nitrogen",
              "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)")
#stream sites
WQP_df_N <- readWQPdata(siteNumbers = sites_WQP) #, as.Date("2000-01-01"), 
#as.Date("2021-02-01"))#dates dont actually work?

saveRDS(WQP_df_N, "data/N_WQP_Sites_df.RDS")
save.csv(WQP_df_N, "data/N_WQP_Sites_df.csv")

#WQP_df_N <- readRDS("data/N_WQP_Sites_df.RDS")

WQP_sw_N_sql <- WQP_df_N %>%
  mutate(date = as.character(as.Date(ActivityStartDate, format = "Y-m-d")))

connection <- dbConnect(RSQLite::SQLite(), "LISS_N.db")
dbWriteTable(connection, "WQP_data_SW", WQP_sw_N_sql, overwrite = TRUE)

unique(WQP_df_N$CharacteristicName)

WQP_df_N_1 <- WQP_df_N %>%
  dplyr::filter(CharacteristicName %in% N.names)

WQP_df_N_summary <- WQP_df_N_1 %>%
  filter(CharacteristicName == "Nitrate")%>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n_nitrate = n(),
            sDate = min(ActivityStartDate),
            eDate = max(ActivityStartDate))%>%
  left_join(., N_WQP_sf)%>% #to get geometry
  st_as_sf(crs = poly_crs)

#gw sites
WQP_gw_N <- readWQPdata(siteNumbers = sites_WQP_gw, as.Date("1970-01-01"), as.Date("2021-02-01"))
saveRDS(WQP_gw_N, "./data/N_WQP_Sites_gw.RDS")
write.csv(WQP_gw_N, "data/N_WQP_Sites_gw.csv")

#to preserve dates in sql
WQP_gw_N_sql <- WQP_gw_N %>%
  mutate(date = as.character(as.Date(ActivityStartDate, format = "Y-m-d")))
dbWriteTable(connection, "WQP_data_GW", WQP_gw_N_sql, overwrite = TRUE)
#Location Data
WQP_gw_N_loc <- attr(WQP_gw_N, "siteInfo")
dbWriteTable(connection, "WQP_loc_GW", WQP_gw_N_loc, overwrite = TRUE)

WQP_gw_N <- readRDS("data/N_WQP_Sites_gw.RDS")

WQP_gw_N_1 <- WQP_gw_N %>%
  dplyr::filter(CharacteristicName %in% N.names)

WQP_gw_N_summary <- WQP_gw_N_1 %>%
  dplyr::filter(CharacteristicName == "Nitrate")%>%
  dplyr::mutate(decade = plyr::round_any(year(ActivityStartDate), 10)) %>%
  group_by(MonitoringLocationIdentifier, decade)%>%
  dplyr::summarise(n_nitrate = n(),
                   sDate = min(ActivityStartDate),
                   eDate = max(ActivityStartDate),
                   max_no3 = as.numeric(max(ResultMeasureValue))+0.01)%>%
  left_join(., N_WQP_gw)%>% #to get geometry
  st_as_sf(crs = poly_crs) 



