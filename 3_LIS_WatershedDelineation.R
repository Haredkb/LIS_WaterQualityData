###################################################################################################
###
### Iterative watershed delineation using the USGS StreamStats API
###
### originally created by Eric Moore 11/30/2021
### modified by Danielle Hare
###################################################################################################


### Libraries
library(devtools)
#devtools::install_github("markwh/streamstats")
library(streamstats)
library(tidyverse)
library(sp)
library(sf)
### a file of lat/lon pourpoints 
###FROM HARE LISS
N_WQP_Sites_df <- readRDS("./data/N_WQP_Sites_df.RDS")
N_WQP_loc <- attr(N_WQP_Sites_df, which = "siteInfo" )
variable_list <- as.data.frame(unique(N_WQP_Sites_df$CharacteristicName))
##assess location duplications
dup <- N_WQP_loc %>% group_by(dec_lat_va, dec_lon_va) %>% filter( n() > 1 )
# manual data adjustment
N_WQP_loc <- N_WQP_loc %>% 
  filter(!grepl("West Thompson Lake", station_nm))
coordinates(N_WQP_loc)=~dec_lon_va+dec_lat_va
#make a shapefile of points
proj4string(N_WQP_loc)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(N_WQP_loc,CRS("+proj=longlat"))
raster::shapefile(LLcoor, "LIS_Database_Points.shp")

## Updated Points Snapped - has own issues compare in the end
N_WQP_loc <- st_read("data/LIS_Database_Points_SnapNHD.shp")

cut <- N_WQP_loc  %>% 
  dplyr::select(1:5) #%>% 
  #slice(list= c(1:5))
cut <- cbind(cut, st_coordinates(cut))
row_seq <- seq(1:nrow(cut))


Start_time <- Sys.time()



lapply(row_seq,
       function(x){
         if(!file.exists(paste0("output/WatershedDelineation/snappedNHD/", cut$site_no[x], ".shp"))){
         ### create a temporary watershed delineation
         try(temp_ws <- streamstats::delineateWatershed(xlocation = round(cut$X[x],5), #round(cut$dec_lon_va[x],5), 
                                                        ylocation = round(cut$Y[x],5),#round(cut$dec_lat_va[x],5), 
                                                        rcode = NULL,
                                                        crs = 4326, 
                                                        includeparameters = "true"))
         
         Sys.sleep(4)
         ## write the shapefile of the temporary watershed with a new name
         try(streamstats::writeShapefile( watershed = temp_ws,
                              layer = cut$site_no[x],
                              dir = "output/WatershedDelineation/snappedNHD",
                              what = c("boundary", "pourpoint")))
         
         #try(ws_all <- st_union(temp_ws)) 
         return(paste0(cut$site_no[x]))
         ### Counting of watersheds
       }
       })

out_ws <- lapply(row_seq,
                 function(x){
                   if(file.exists(paste0("output/WatershedDelineation/snappedNHD", cut$site_no[x], ".shp"))){
                     shp_file <- "AVAILABLE"
                   }else(shp_file <- "ERROR")
                   
                   re <- data.frame(cut$site_no[x], x, shp_file)
                   return(re)
                   })
out_ws <- do.call(rbind, out_ws)     

#how many files missing?
out_ws %>%
group_by(shp_file)%>%
  summarise(n = n())

#output a table for stream stats manual 
review_ws <- out_ws %>%
  dplyr::filter(shp_file == "ERROR")%>%
  mutate(site_no = cut.site_no.x.)%>%
  left_join(., select(N_WQP_loc, 1:5)) 

write.csv(review_ws, "output/LIS_WSDelin_Review.csv")
write.csv(N_WQP_loc, "output/LIS_SW_locations.csv")                     
                     
# combineWatersheds(shapefile_list, id)
# sf::st_read(temp_ws)
# 
# #bind all shpfiles 
# file_list <- list.files("data/WatershedDelineation", pattern = "*shp", full.names = TRUE)
# shapefile_list <- lapply(file_list, read_sf)
# 
# leafletWatershed(temp_ws)
# joined = sp::SpatialPolygons(lapply(shapefile_list, function(x){x@polygons[[1]]}))
# 
# 
# all_ws <- do.call(rbind, shapefile_list)
# 
# all_ws <- sf::st_as_sf(data.table::rbindlist(shapefile_list, fill = TRUE))
# 
# ggplot()+
#   geom_sf(all_ws)
# 
# ## plot watershed
# library(sf)\
# aoi_boundary_HARV <- st_read(
#   "data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")


