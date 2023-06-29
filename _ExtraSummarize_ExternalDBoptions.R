#############################
#############################
## Creating SQL for LISS ##
#############################
###############################
#Based on tutorial here: https://www.datacamp.com/community/tutorials/sqlite-in-r


library(RSQLite)
library(plyr) #for rounding years
library(lubridate)
library(ggplot2)
library(sf)
library(DVstats)
library(tidyverse)
###############
## WQP       ##
###############
WQP_df_N$id <- WQP_df_N$MonitoringLocationIdentifier
WQP_df_N$id <- rownames(WQP_df_N)
rownames(WQP_df_N) <- c()

connection <- dbConnect(RSQLite::SQLite(), "LISS_N.db")

# Write the mtcars dataset into a table names mtcars_data
dbWriteTable(connection, "WQP_data", WQP_df_N)

dbWriteTable(connection, "WQP_data_GW", WQP_gw_N)

# List all the tables available in the database
dbListTables(connection)

#################
## NWIS uv ######
################

nwis_uvfiles <- list.files("./data/nwis", full.names = TRUE, recursive = TRUE)
l = 1
dat = list()
for(f in nwis_uvfiles){
  
dat[[l]] = read.delim(f, header = TRUE, stringsAsFactors = FALSE,  
                 sep = "\t", comment.char = "#", na.strings=c(""," ","NA")) #,

dat[[l]] = dat[[l]][-1,]

l = l+1
#                 na.strings = "A:[91]")
}

#rename all dataframes columns 

#dataframe of rename columns
df_rename <- 
  data.frame(
    id = c("32295", "99133", "91058", "00010", "00300", "00301", "00400", "00095", "00065", "00060", "63680", "72137"),
    name = c("fDOM", "NO3_NO2_mgL", "TotN_lbNd","Temp_C", "DO_mgL", "DO_%","pH","SpC_mScm_25", "height_ft","Q_cfs","Turb_FNU","Q_tidallycorr_cfs")
  )

#rename columns
#Must be another way, just tricky with the "contains" nature of the names
for(n in 1:length(dat)){
  df <- dat[[n]]%>%
    select(-contains("cd"))
  
  for(r in 1:nrow(df_rename)){
    if(any(grepl(df_rename[r,]$id,colnames(df)))){
      
      colnames(df)[grepl(df_rename[r,]$id,colnames(df))] <- df_rename[r,]$name
      
    }
  }
  dat[[n]] <- df
}

#combine dataframes
nwis_uv <- do.call(dplyr::bind_rows, dat)

#from USGSuv
dbWriteTable(connection, "NWIS_uv_N", nwis_uv, overwrite = TRUE)


# Close the database connection
dbDisconnect(connection)

################################
### NEON #############
#########################

#install.packages("neonUtilities")
library(neonUtilities)
library(httr)
library(jsonlite)
#stream chemical data
st_chem <- "DP1.20093.001"
gw_chem <- "DP1.20092.001"
#Discharge
q <- "DP4.00130.001"
#dissolved gases
st_gas <- "DP1.20097.001"

# # Request data using the GET function & the API call
req <- GET(paste0("http://data.neonscience.org/api/v0/products/", raw, "/HOPB"))
req <- GET("https://data.neonscience.org/api/v0/data/DP1.20093.001/HOPB/")
# Make the data readable by jsonlite
req.text <- content(req, as="text")

# Flatten json into a nested list
avail <- jsonlite::fromJSON(req.text,
                            simplifyDataFrame=T,
                            flatten=T)
# 
# 
# wood.urls <- unlist(avail$data$siteCodes$availableDataUrls)
# avail$data$siteCodes$siteCode
# # Get available data for RMNP Oct 2019
# woody <- GET(wood.urls[grep("HOPB", wood.urls)])
# woody.files <- jsonlite::fromJSON(content(woody, as="text"))
zipsByProduct(dpID=st_chem, site="HOPB", package="basic", check.size=T)

chem_df <- loadByProduct(dpID=st_chem, site="HOPB", package="expanded") #a neonUtilites package
names(chem_df )

# To get each table in the list as an independent object, outside of the list:
list2env(chem_df , .GlobalEnv)

#2nd try
trip.temp <- loadByProduct(dpID="DP1.20093.001", 
                           site=c("HOPB"),
                          startdate="2000-01", 
                          enddate="2022-03")
list2env(trip.temp, .GlobalEnv)

neon_df <- swc_externalLabDataByAnalyte

# Write the mtcars dataset into a table names mtcars_data
dbWriteTable(connection, "NEON_df", neon_df)



#######################
##### Farmington ######
#######################

farm_CT <- read.csv("data/farmingtonWSA_data.csv")
# Write the mtcars dataset into a table names mtcars_data
dbWriteTable(connection, "farmCT_data", farm_CT)


#######################
##### Farmington ######
#######################

ne_sparrow <- read.csv("data/ne_sparrow_model_input.txt")
# Write the mtcars dataset into a table names mtcars_data
dbWriteTable(connection, "NEsparrow_data", ne_sparrow)

#######################
## Create 2 new tables, one with data and one with location data
#######################

df_loc <- swc_fieldSuperParent %>%
  dplyr::select(namedLocation, decimalLatitude, decimalLongitude, elevation, geodeticDatum, collectDate)%>%
  dplyr::rename("id" = 1, "lat" = 2, "lng" = 3, "ele" = 4, "datum" = 5, "date" = 6)%>%
  mutate(agency = "NEON",
         date = as.Date(date))


df_loc_farm <- farm_CT%>%
  dplyr::select(SiteCode, Latitude, Longitude, Elevation_m, LatLongDatumSRSName, SiteName, LocalDateTime)%>%
  dplyr::rename("id" = 1, "lat" = 2, "lng" = 3, "ele" = 4, "datum" = 5, "name" = 6, "date" = 7)%>%
  mutate(agency = "FarmingtonRiverWSA",
         date = as.Date(date))


df_dat_far<- farm_CT %>%
  group_by(SiteName, VariableName)%>%
  summarise_all(funs(mean, max, min, sd), na.rm = TRUE)


df_loc <- dplyr::bind_rows(df_loc, df_loc_farm)
# %>%
#   group_by(id) %>%
#   summarise(lat = mean(lat),
#             lng = mean(lng),
#             ele = mean(ele),
#             datum = first(datum),
#             name = first(name),
#             agency = first(agency),
#             sDate = min(date),
#             eDate = max(date),
#             n = n())

N_WQP_Sites_df <- readRDS("./data/N_WQP_Sites_df.RDS")
N_WQP_Sites_gw <- readRDS("./data/N_WQP_Sites_gw.RDS")
N_WQP_loc_gw <- attr(N_WQP_Sites_gw, which = "siteInfo" )
dbWriteTable(connection, "WQP_loc_gw", N_WQP_loc_gw)
N_WQP_loc <- attr(N_WQP_Sites_df, which = "siteInfo" )
variable_list <- as.data.frame(unique(N_WQP_Sites_df$CharacteristicName))
##assess location duplications
dup <- N_WQP_loc %>% group_by(dec_lat_va, dec_lon_va) %>% filter( n() > 1 )
# manual data adjustment

N_WQP_loc <- N_WQP_loc %>% 
  filter(!grepl("West Thompson Lake", station_nm)) #they appear to be mislabelled as streams
  
# N_WQP_loc <- N_WQP_loc %>% 
#   dplyr::filter(grepl("MATTA", station_nm, ignore.case = TRUE))
# ##matta"besset" (basset), river does one location dup, but cant determine why, keeping seperate for now

##############
##### Filter and Cleaned DataFrame ######
########  SURFACE WATER  ###############
N_WQP_dat <- N_WQP_Sites_df %>%
  left_join(., N_WQP_loc) %>%#N_WQP_loc)%>%
  dplyr::filter(str_detect(CharacteristicName, "Stream flow, instantaneous")| str_detect(CharacteristicName, "^Nitrate"))%>% #| str_detect(CharacteristicName, "^Specific conductance"))%>%
  #dplyr::filter(str_detect(CharacteristicName, "^Nitrate"))%>%#starts with N
  #dplyr::filter(str_detect(CharacteristicName, "^Specific conductance"))%>%#starts with N
  dplyr::mutate(VariableName = CharacteristicName,
                site_id = site_no,
                site_nm = MonitoringLocationName,
                date = ActivityStartDate,
                datetime = as.POSIXct(paste(date, ActivityStartTime.Time), format="%Y-%m-%d %H:%M:%S"),
                DataValue = as.numeric(ResultMeasureValue),
                DataValue = ifelse(DataValue < 0 , NA , DataValue),
                #Convert ugL to mgL
                DataValue = ifelse(str_detect(ResultMeasure.MeasureUnitCode, "ug/l")==TRUE, DataValue/1000, DataValue),
                DataValue = ifelse(str_detect(ResultMeasure.MeasureUnitCode, "ueq/L")==TRUE, DataValue*62, DataValue), #https://www.knowyourh2o.com/outdoor-3/conversion-factors-for-water-quality
                #Convert NO3 to NO3-N
                DataValue = DataValue / ifelse(str_detect(ResultMeasure.MeasureUnitCode, "asNO3")==TRUE,4.427,1),
                DataValue = ifelse(str_detect(ResultMeasure.MeasureUnitCode, "umol")==TRUE, DataValue*0.016128, DataValue)
                
                )


#Data conversion check
# ggplot(N_WQP_dat)+
#   geom_point(aes(DataValue, Result, colour = ResultMeasure.MeasureUnitCode))

#Summarize Data
df_dat_wqp <- N_WQP_dat %>%
  dplyr::select(site_id, site_nm, VariableName, DataValue, date, LatitudeMeasure, LongitudeMeasure)%>%
  mutate(DataValue = as.numeric(DataValue),
         decade = plyr::round_any(year(date), 10, floor),
         lat = LatitudeMeasure, 
         long = LongitudeMeasure)%>%
  na.omit()%>%
  group_by(site_id, site_nm, VariableName, decade, lat, long)%>%
  dplyr::summarise_all(list(mean = mean,  max = max, min = min, 
            std = sd, n = ~ n()), na.rm = TRUE) #%>% #woohoo n as tough to get


##make dataframe with all data amount
df_dat_wqp_all <- N_WQP_dat %>%
  dplyr::select(site_id, site_nm, VariableName, DataValue, date, LatitudeMeasure, LongitudeMeasure)%>%
  mutate(DataValue = as.numeric(DataValue),
         decade = plyr::round_any(year(date), 10, floor),
         lat = LatitudeMeasure, 
         long = LongitudeMeasure)%>%
  na.omit()%>%
  group_by(site_id, site_nm, VariableName, decade, lat, long)%>%
  dplyr::summarise_all(list(mean = mean,  max = max, min = min, 
                            std = sd, n = ~ n()), na.rm = TRUE)

  # left_join(., N_WQP_loc, by = c("site_id" = "MonitoringLocationIdentifier"))%>% #reestalsih lat/long
  # st_as_sf(crs = poly_crs)%>%
  # mutate(lat = unlist(map(geometry,1)),
  #        long = unlist(map(geometry,2)))
  
# df_dat_wqp <-  dplyr::select(df_dat_wqp, -17: -22)%>% #remove extra location variables
#                 st_drop_geometry(.)
saveRDS(df_dat_wqp, "./data/LISS_SW_NO3.rds")
write.csv(df_dat_wqp, "./data/LISS_SW_NO3.csv")

# df1 <- N_WQP_dat %>%
#   select(SiteName, VariableName, DataValue)%>%
#   mutate(DataValue = as.numeric(DataValue))%>%
#   na.omit()%>%
#   group_by(SiteName, VariableName)%>%
#   summarise_all(list(mean = mean,  max = max, min = min, 
#                      std = sd, n = ~ n()), na.rm = TRUE)%>%
#   left_join(., df_loc, by = c("SiteName" = "id"))%>%
#   st_as_sf(crs = poly_crs)%>%
#   mutate(lat = unlist(map(geometry,1)),
#          long = unlist(map(geometry,2)))



########## repeat with GW ####################
WQP_gw_N <- readRDS("data/N_WQP_Sites_gw.RDS")
dat <- WQP_gw_N%>%
  left_join(., N_WQP_loc_gw)%>%
  dplyr::filter(str_detect(CharacteristicName, "^Nitrate")| str_detect(CharacteristicName, "^Specific conductance"))%>%#starts with N
  #dplyr::filter(str_detect(CharacteristicName, "^Specific conductance"))%>%#starts with N
  dplyr::mutate(VariableName = CharacteristicName,
                SiteName = MonitoringLocationIdentifier,
                WellDepth_ft = WellDepthMeasure.MeasureValue,
                date = ActivityStartDate,
                DataValue = ResultMeasureValue)
WQP_gw_SiteName <- dplyr::select(N_WQP_loc_gw, MonitoringLocationIdentifier, geometry)
                                 
df_gw_wqp <- dat %>%
  dplyr::select(SiteName, date, VariableName, WellDepth_ft, DataValue, LatitudeMeasure, LongitudeMeasure)%>%
  dplyr::mutate(DataValue = as.numeric(DataValue),
         decade = plyr::round_any(year(date), 10, floor),
         lat = LatitudeMeasure, 
         long = LongitudeMeasure)%>%
  na.omit()%>%
  dplyr::group_by(SiteName, WellDepth_ft, VariableName, decade, lat, long)%>%
  dplyr::summarise_all(list(mean = mean,  max = max, min = min, 
                      std = sd, n = ~ n()), na.rm = TRUE) #%>%
  # left_join(., WQP_gw_SiteName, by = c("SiteName" = "MonitoringLocationIdentifier"))%>% 
  # st_as_sf(crs = poly_crs)%>%
  # mutate(lat = unlist(map(geometry,1)),
  #        long = unlist(map(geometry,2)))
# 
# df_gw_wqp <- st_as_sf(df_gw_wqp)%>%
#   na.omit()

rownames(df_gw_wqp) <- NULL

#save as RDS and csv
saveRDS(df_gw_wqp, "./data/LISS_GW_NO3.rds")
write.csv(df_gw_wqp, "./data/LISS_GW_NO3.csv")


#####################################
### Plot GW Sites over Deacdes #####
####################################

ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_gw_wqp, aes(colour = DataValue_max +0.01))+ #need to add to do log transform
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(0.001,0.1,1,10,100))+
  theme_minimal()+
  ggtitle("LISS Nitrate Samples")+
  guides(fill=guide_legend(title="Max Nitrate mg/L"))+
  xlim(-75,NA)+
  facet_wrap(~decade)


#####################################
### Plotting long term records ######
#####################################

#site that have a lot of data!
no3_long <- df_dat_wqp_all %>%
  dplyr::filter(VariableName == "Nitrate" & DataValue_n > 500)%>%
  pull(site_nm)

no3_long_df <- N_WQP_dat %>%
  dplyr::filter(CharacteristicName == "Nitrate" & site_nm %in% no3_long)%>%
  dplyr::select(station_nm, CharacteristicName, DataValue, date)%>%
  dplyr::mutate(DataValue = as.numeric(DataValue),
         decade = round_any(year(date), 10, floor))%>%
  na.omit() 

library(ggforce)

#Spilt Facets into multiple pages
for(i in 1:6){
  
    p1 <- ggplot(no3_long_df, aes(date, DataValue))+
      geom_point()+
      geom_smooth(colour = "yellow")+
      facet_grid()+
      theme_bw()+
      facet_wrap_paginate(station_nm ~ ., ncol = 1, nrow = 5, 
                          page = i, scales = "free_y")
    
    file = paste0("./figures/record_no3_p", i, ".pdf")
    ggsave(file, plot = p1)
    
}

df_wqp_no3_long <- no3_long_df %>%
  na.omit()%>%
  group_by(SiteName, VariableName, decade)%>%
  summarise_all(list(mean = mean,  max = max, min = min, 
                     std = sd, count = ~ n()), na.rm = TRUE) %>%
  left_join(., df_loc, by = c("SiteName" = "id"))%>% 
  st_as_sf(crs = poly_crs)%>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2)))

write.csv(df_wqp_no3_long, "LISS_decades_NO3_Spc_Data.csv")

####################
#Plotting Sites with Long Records
####################

ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_wqp_no3_long, aes(colour = DataValue_max))+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(0.001,0.1,1,10,100))+
  theme_minimal()+
  ggtitle("LISS Nitrate Samples")+
  guides(fill=guide_legend(title="Nitrate mg/L"))+
  xlim(-75,NA)+
  facet_grid(VariableName~decade)

  
  

#df <- dbReadTable(connection, "WQP_data")
df_loc <- N_WQP_dat%>%
  dplyr::select(site_no, dec_lat_va, dec_lon_va, HorizontalCoordinateReferenceSystemDatumName,  MonitoringLocationName, OrganizationFormalName, ActivityStartDate)%>%
  dplyr::rename("id" = 1, "lat" = 2, "lng" = 3, "datum" =4, "name" = 5, "agency" = 6, "date" = 7) %>%
  dplyr::bind_rows(., df_loc) %>%
  group_by(id) %>%
  summarise(lat = mean(lat),
            lng = mean(lng),
            #ele = mean(ele),
            datum = first(datum),
            name = first(name),
            agency = first(agency),
            sDate = min(date),
            eDate = max(date),
            n = n(),
            estN = interval(sDate,eDate) %/% days(1),
            days = estN + 1,
            estN = n/days)

df_loc <- df_loc %>% st_as_sf(coords = c("lng", "lat"), crs = poly_crs)
###########################
## Plot
###########################
LISS_poly <- read_sf("C:/Users/hared/Dropbox/UConn/Projects/800_LISS_LandUse/200_Data/230_SpatialData/LIS_watershed_GEO.shp")
LISS_WS <- read_sf("C:/Users/hared/Dropbox/UConn/Projects/800_LISS_LandUse/200_Data/230_SpatialData/LIS_SW_WSDelinMerge1.shp")
##Watersheds
ggplot()+
  #geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = LISS_WS, fill = "blue", alpha = 0.25, size= 0.25)+
  #scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(1,10,100,1000,3000))+
  theme_minimal()+
  xlim(-75,NA)



ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_loc, aes(colour = n))+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(1,10,100,1000,3000))+
  theme_minimal()+
  xlim(-75,NA)

library(hrbrthemes)
# Plot


p1 <- ggplot(df_loc) +
          geom_segment( aes(y=sDate, yend=eDate, x=id, xend=id), color="grey") +
          geom_point( aes(x=id, y=sDate), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
          geom_point( aes(x=id, y=eDate), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
          geom_text( aes(x=id, y=eDate, label = days), hjust = 0, nudge_x = 0.05)+
          coord_flip()+
          theme_ipsum() +
          theme(
            legend.position = "none",
          ) +
          xlab("Site_ID") +
          ylab("Date (Year)")


# #If you want to save your plot to a file and make it taller, you can specify the height and width in ggsave.
 ggsave(file="figures/plot.png", width=2, height=140, limitsize = FALSE)

#If you want your plot to be taller in your markdown and html file, you can specify them in the r chunk.
#{r fig.width=7, fig.height=4}

#for intermediate plotting
library(plyr)#for round_any
library(sf)
df_wqp <- left_join(df_dat_wqp, df_loc, by = c("SiteName" = "id"))%>% 
  st_as_sf(crs = poly_crs)#%>%
  #mutate(decadeE = round_any(year(eDate), 10, floor))

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)


ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_wqp, aes(colour = n.x))+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(1,10,100,1000,3000))+
  theme_minimal()+
  ggtitle("LISS Specific Conductance Samples")+
  guides(color=guide_legend(title="Specific Conductance mS/cm @25"))+
  xlim(-75,NA)


ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_wqp, aes(colour = DataValue_max))+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(0.001,0.1,1,10,100))+
  theme_minimal()+
  ggtitle("LISS Nitrate Samples")+
  guides(fill=guide_legend(title="Nitrate mg/L"))+
  xlim(-75,NA)+
  facet_grid(VariableName~decade)


#for specific conductance
ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = df_wqp, aes(colour = DataValue_max))+
  scale_colour_gradient(low = "blue", high = "red", na.value = NA, trans = "log10", breaks= c(1,10,100, 1000,10000))+
  theme_minimal()+
  ggtitle("Total LISS Specfic Conductance Samples")+
  xlim(-75,NA)+
  facet_grid(VariableName~decade)


## create C-Q plots
#df[!duplicated(df), ]
CQ_NO3 <- N_WQP_dat%>%
  distinct(.)%>%
  dplyr::select(station_nm, CharacteristicName, DataValue, date, LatitudeMeasure, LongitudeMeasure)%>%#datetime)%>%
  dplyr::mutate(DataValue = as.numeric(DataValue),
                #decade = round_any(year(datetime), 10, floor)
                )%>%
  group_by(station_nm, CharacteristicName, date, LatitudeMeasure, LongitudeMeasure)%>%
  dplyr::summarise_all(list(mean = mean,  max = max, min = min, 
                            std = sd, n = ~ n()), na.rm = TRUE)%>%
  ungroup()%>%
  dplyr::select(-mean, -min:-n)%>%
  pivot_wider(., names_from = CharacteristicName, values_from = max)%>%
  rename(Q_cfs = "Stream flow, instantaneous")%>%
  na.omit() %>%
  dplyr::mutate(decade = round_any(year(date), 10, floor)) %>%
  dplyr::filter(station_nm != "PAWCATUCK R AT KENYON INDUSTRIES AT KENYON, RI") #Very high nitrate - textile factry with a pong nearby?

CQ_NO3 <- st_as_sf(CQ_NO3, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = poly_crs)

ggplot(CQ_NO3)+
 uides(fill=guide_legend(title="Nitrate mg/L"))+
  xlim(-75,NA)+
  facet_grid(~decade)
 geom_point(aes(x = Q_cfs, y = Nitrate))+
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')


ggplot()+
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey", fill="white" )+
  geom_sf(data = LISS_poly, fill = NA)+
  geom_sf(data = CQ_NO3, aes(colour = Nitrate))+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, trans = "log10", breaks= c(0.001,0.1,1,10,100))+
  theme_minimal()+
  ggtitle("LISS Nitrate Samples")+
  g
#######################
#### NWIS notes
#######################


#            TS   parameter     Description
#        234669       99133     Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen, Nitrate sensor
#        243078       91058     Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, pounds of nitrogen per day, Nitrate sensor
#        248550       32295     Dissolved organic matter fluorescence (fDOM), water, in situ, concentration estimated from reference material, micrograms per liter as quinine sulfate equivalents (QSE)
#         66916       00010     Temperature, water, degrees Celsius
#         66918       00300     Dissolved oxygen, water, unfiltered, milligrams per liter
#         66919       00400     pH, water, unfiltered, field, standard units
#         66920       00095     Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
#         66921       00065     Gage height, feet
#         66930       00060     Discharge, cubic feet per second
#         66932       63680     Turbidity, water, unfiltered, monochrome near infra-red LED light, 780-900 nm, detection angle 90 +-2.5 degrees, formazin nephelometric units (FNU)
#         66934       72137     Discharge, tidally filtered, cubic feet per second

#TS   parameter     Description
#        256024       32295     Dissolved organic matter fluorescence (fDOM), water, in situ, concentration estimated from reference material, micrograms per liter as quinine sulfate equivalents (QSE)
#        256026       00301     Dissolved oxygen, water, unfiltered, percent of saturation
#        260496       99133     Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen, Nitrate Sensor
#         66838       00060     Discharge, cubic feet per second
#         66839       00065     Gage height, feet
#         66840       00300     Dissolved oxygen, water, unfiltered, milligrams per liter
#         66842       00010     Temperature, water, degrees Celsius
#         66843       00095     Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
#         66845       00400     pH, water, unfiltered, field, standard units
#         66846       63680     Turbidity, water, unfiltered, monochrome near infra-red LED light, 780-900 nm, detection angle 90 +-2.5 degrees, formazin nephelometric units (FNU)
#
df_loc_gw <- N_WQP_gw%>%
  dplyr::select(MonitoringLocationIdentifier, Measure, dec_lon_va, HorizontalCoordinateReferenceSystemDatumName,  MonitoringLocationName, OrganizationFormalName, ActivityStartDate)%>%
  dplyr::rename("id" = 1, "lat" = 2, "lng" = 3, "datum" =4, "name" = 5, "agency" = 6, "date" = 7) %>%
  dplyr::bind_rows(., df_loc) %>%
  group_by(id) %>%
  summarise(lat = mean(lat),
            lng = mean(lng),
            #ele = mean(ele),
            datum = first(datum),
            name = first(name),
            agency = first(agency),
            sDate = min(date),
            eDate = max(date),
            n = n(),
            estN = interval(sDate,eDate) %/% days(1),
            days = estN + 1,
            estN = n/days)

df_loc <- df_loc %>% st_as_sf(coords = c("lng", "lat"), crs = poly_crs)


