######
## Accessing SQL
######
# Built from ideas at:
#https://solutions.posit.co/connections/db/databases/sqlite/
library(RSQLite)
library(lubridate)
library(sf)
library(tidyverse)

connection <- dbConnect(RSQLite::SQLite(), "LISS_N.db")
# List all the tables available in the database
dbListTables(connection)

df <- dbReadTable(connection, "WQP_data_GW")%>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) #convert back to date necessary for sql

df_loc <- st_as_sf(dbReadTable(connection, "WQP_loc_GW"), coords = c("dec_lon_va", "dec_lat_va"), crs = 4326, agr = "constant")

#Find Sites within the Farmington Watershed
shp_farm <- read_sf("C:\\Users\\hared\\Dropbox\\UConn\\Projects\\800_LISS_LandUse\\200_Data\\230_SpatialData\\FarmingtonWatershed_simple\\FarmingtonWatershed_simple.shp")
shp_farm <- st_transform(shp_farm, crs = st_crs(df_loc))
df_farm <- st_intersection(df_loc, shp_farm)

sites_farm <- unique(df_farm$site_no)

output <- df %>%
  dplyr::filter(MonitoringLocationIdentifier %in% sites_farm) %>%
  dplyr::filter(str_detect(CharacteristicName, "^Nitrate"))

#Review Output
review <- output %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(count_values = n())

write.csv(output, "./data/FarmingtonWS_GW_nitrate.csv")

loc_ts <- review %>%
  dplyr::filter(count_values > 5)

loc_ts <- loc_ts$MonitoringLocationIdentifier

output_ts <- df %>%
  dplyr::filter(MonitoringLocationIdentifier %in% loc_ts) %>%
  dplyr::filter(str_detect(CharacteristicName, "^Nitrate"))

ggplot(output_ts)+
  geom_point(aes(x = date, y = as.numeric(ResultMeasureValue), color = MonitoringLocationIdentifier))





                