######
## Output Tables for SVIC 
### 4.14.2023
library(targets)
source("./packages.R")

#Read in Datafiles
# NO3_data <- tar_read(NO3_data)
N_sites <- tar_read(N_sites)
N_refsites <- tar_read(N_refsites)
baseflow_dates <- tar_read(baseflow_dates)
# BaseFlowSample_NO3 <- tar_read(BaseFlowSample_NO3)
BaseFlowSample_allWQ <- tar_read(BaseFlowSample_allWQ)
# All_data <- tar_read(WQP_df_N)
dat_WQ<- readRDS("data/WQ_SVIC_Display.RDS")# from SVIC_LISDatabase_CleanUp.R


### Delete unnecessary columns
Loc_File <- N_sites[, colSums(is.na(N_sites)) != nrow(N_sites)] %>%
  dplyr::select(-10, -12:-15, -18:-20)%>%
  mutate(site_id = MonitoringLocationIdentifier)%>%
  mutate(site_infl = ifelse(is.na(site_infl), "None", site_infl))

write.csv(st_drop_geometry(Loc_File), "data/Loc_SVIC.csv", row.names = FALSE)

Loc_WS_File <- Loc_File %>%
  mutate(CountyCode = as.character(CountyCode))%>%
  dplyr::select(-"StateCode", -"station_nm", -CountyCode)%>%
  inner_join(st_drop_geometry(WS_poly),.) %>%
  relocate(site_id)%>%
  dplyr::select(-"path", -REACH_meas, -OrganizationFormalName, -row_num, -shplen_num, -poly_area, -site_no_2, -hucCd_2, - UNNAM_or_S,
                -Organizat0, -Organizat1, -layer_2, -path_2, -Monitorin0, -Monitorin1, -Monitorin2, -Monitorin3, -StateCode, -layer, -HUCEightD0)
  
WQ_Display <- dat_WQ%>%
  inner_join(.,Loc_WS_File, by = "site_id")

WQ_Raw <- BaseFlowSample_allWQ%>%
  mutate(BaseflowConditions = ifelse(BaseflowConditions == 1, "Baseflow", "NonBaseflow"))%>%
  inner_join(.,Loc_WS_File, by = "site_id")

##Output CSV files

Full_WQ_File <- left_join(WQ_Display, Loc_WS_File)


write.csv(Loc_File, "C:\\Users\\hared\\Dropbox\\UConn\\Projects\\800_LISS_LandUse\\300_Analysis\\Loc_SVIC.csv", row.names = FALSE)
write.csv(Loc_WS_File, "C:\\Users\\hared\\Dropbox\\UConn\\Projects\\800_LISS_LandUse\\300_Analysis\\Loc_WS_SVIC.csv", row.names = FALSE)
write.csv(WQ_Display, "C:\\Users\\hared\\Dropbox\\UConn\\Projects\\800_LISS_LandUse\\300_Analysis\\Data_WQ_SVIC_Display.csv", row.names = FALSE)
write.csv(WQ_Raw, "C:\\Users\\hared\\Dropbox\\UConn\\Projects\\800_LISS_LandUse\\300_Analysis\\Data_WQ_SVIC_RawDB.csv", row.names = FALSE)


#######################
#check plots
#######################

df_WQ_BF <- Full_WQ_File %>%
  dplyr::filter(BaseflowConditions == 1 & 
                  is.na(site_infl) &
                  CharacteristicName == "Nitrate")

df_WQ_BF <- Full_WQ_File %>%
  dplyr::filter(BaseflowConditions == 1 & 
                  is.na(site_infl) &
                  CharacteristicName == "Orthophosphate")

ggplot(df_WQ_BF)+
  geom_point(aes(x = DateTime, y = as.numeric(Value)))

  


