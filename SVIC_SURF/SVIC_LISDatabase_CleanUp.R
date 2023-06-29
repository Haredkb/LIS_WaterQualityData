##Phosphate Clean UP
library(tidyverse)
library(targets)
library(sf)

BaseFlowSample_allWQ <- tar_read(BaseFlowSample_allWQ)
### Check that all site_ids are in the WS files
WS_Sites <- read.csv("data/Loc_WS_SVIC.csv")%>%
  pull(site_id)

Characteristic_remove <- c("Inorganic nitrogen (nitrate and nitrite) ***retired***use Nitrate + Nitrite","Phosphate-phosphorus***retired***use Total Phosphorus, mixed forms")
Characteristic_Display <- c("Ammonia and ammonium", 'Total dissolved solids', "pH", "Specific conductance", "Nitrate", "Orthophosphate", "Phosphate-phosphorus", "Phosphate-phosphorus as P","Soluble Reactive Phosphorus (SRP)")
## Evalulate Non-Detect Structure 
df_NDCount <- BaseFlowSample_allWQ %>%
  group_by(ResultDetectionConditionText)%>%
  summarise(count = n()) #(all have 1000+ count)

#remove unclear or contamination values and any sites without watersheds
BaseFlowSample_allWQ <- BaseFlowSample_allWQ %>%
  dplyr::filter(MonitoringLocationIdentifier %in% WS_Sites) %>% #Only sites within the list of sites with watershed attributes
  dplyr::filter(!CharacteristicName %in% Characteristic_remove) %>%
  mutate(ResultDetectionConditionText = ifelse(is.na(ResultDetectionConditionText), "Detect", ResultDetectionConditionText))%>%
  dplyr::filter(ResultDetectionConditionText != "Detected Not Quantified"|
                ResultDetectionConditionText != "Present Above Quantification Limit"|
                ResultDetectionConditionText != "Systematic Contamination")# Based  on NonDetect Review


#Check Results
df_CharCount <- BaseFlowSample_allWQ %>%
  group_by(CharacteristicName, ResultDetectionConditionText)%>%
  summarise(count = n())

#################################
## Process DataBase
#################################

##############
## Nitrate Clean Up
##############

dat_NO3 <- as.data.frame(BaseFlowSample_allWQ) %>% 
  dplyr::filter(str_detect(CharacteristicName, "Nitrate"))%>% 
  dplyr::filter(USGSPCode != 83353) %>% #remove atmospheric deposition values 
  mutate(#deal with non detects first to simply approach
    ResultMeasureValue = dplyr::coalesce(as.numeric(ResultMeasureValue),as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
    Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
    ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
    site_id = MonitoringLocationIdentifier,
    DateTime = ActivityStartDateTime,
    Date = ActivityStartDate,
    Value_Unit = tolower(ResultMeasure.MeasureUnitCode))%>% #units for both detect and nondetect
  dplyr::filter(ResultMeasureValue != 0 , !is.na(ResultMeasureValue))%>%
  
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, ResultMeasureValue, Value_Unit, Result_ND, ResultSampleFractionText,  ResultMeasure.MeasureUnitCode, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, ResultMeasureValue)%>%
  
  dplyr::mutate(#Convert ugL to mgL have to assume in NO3 - divide by 4.427 to convert to NO3-N
                DataValue = ifelse(USGSPCode == "91003", (as.numeric(ResultMeasureValue)/1000)/4.427, NA),#91003 - Nitrate, water, filtered, micrograms per liter as nitrate (ug/L)
                DataValue = ifelse(USGSPCode == "71851" | USGSPCode == "71850", as.numeric(ResultMeasureValue)/4.427, DataValue),
                DataValue = ifelse(USGSPCode == "00618" | USGSPCode == "00620", as.numeric(ResultMeasureValue), DataValue),
                DataValue = ifelse(as.numeric(ResultMeasureValue) == 0 , 0, DataValue),
                DataValue = ifelse(as.numeric(ResultMeasureValue) < 0 , NA, DataValue),
                DataValue = ifelse(is.na(DataValue) & Result_ND == "TRUE", 
                                   ifelse(str_detect(DetectionQuantitationLimitMeasure.MeasureUnitCode, "asNO3"), 
                                          as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)/4.427, 
                                          ifelse(str_detect(DetectionQuantitationLimitMeasure.MeasureUnitCode, "ug"),
                                                 as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)/1000,
                                                 as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)
                                          )
                                          
                                   ),DataValue)
                #DataValue = ifelse(str_detect(ResultMeasure.MeasureUnitCode, "ueq/L")==TRUE, ((DataValue*62)/1000)/4.427, DataValue), #https://www.knowyourh2o.com/outdoor-3/conversion-factors-for-water-quality
                #Convert NO3 to NO3-N
                #DataValue = ifelse(str_detect(ResultMeasure.MeasureUnitCode, "umol")==TRUE, DataValue*0.016128, DataValue)
                #https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx
  ) %>%
  mutate("Value" = DataValue,
         Unit = "mgLNO3-N" )%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText,Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()

#Save Cleaned Data
saveRDS(dat_NO3, "data/Data_NO3_clean.RDS")
write.csv(dat_NO3, "data/Data_NO3_clean.csv", row.names = FALSE)



##############
## Phosphate Clean Up
##############

phosphate_names = c("Orthophosphate", "Phosphate-phosphorus", "Phosphate-phosphorus as P","Soluble Reactive Phosphorus (SRP)")

df <- as.data.frame(BaseFlowSample_allWQ) %>%
  dplyr::filter(CharacteristicName %in% phosphate_names)%>%
  dplyr::select(USGSPCode, ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue,  ResultMeasure.MeasureUnitCode,  ActivityStartDateTime, BaseflowConditions)%>%
  mutate(site_id = MonitoringLocationIdentifier,
         Value = ResultMeasureValue,
         Value_Unit = ResultMeasure.MeasureUnitCode,
         Date= ActivityStartDate, 
         DateTime = ActivityStartDateTime) %>%
  relocate(site_id, Date, DateTime, Value, Value_Unit) %>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Value, Value_Unit, BaseflowConditions)%>%
  distinct()

#List of Units
units_PO4 <- unique(df$Value_Unit)
#determining data clean up methods
# Desired Units PO4-P
# Phosphate Unit Conversions
#orthophophate to PO4-P is multiply by 0.3261

#Counts of units 
df_units <- df %>%
  group_by(CharacteristicName, USGSPCode, Value_Unit)%>%
  summarise(count = n())

#Create Dataframe with one unit 
dat_PO4<- BaseFlowSample_allWQ %>% 
  dplyr::filter(CharacteristicName %in% phosphate_names)%>%
  dplyr::select(MonitoringLocationIdentifier, USGSPCode, CharacteristicName, ActivityStartDate, ActivityStartDateTime, ActivityStartDateTime, ResultSampleFractionText, ResultMeasureValue,ResultMeasure.MeasureUnitCode, DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText, BaseflowConditions) %>%
  mutate(#deal with non detects first to simply approach
         ResultMeasureValue = dplyr::coalesce(as.numeric(ResultMeasureValue),as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
         Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
         ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
         site_id = MonitoringLocationIdentifier,
         Date = ActivityStartDate,
         DateTime = ActivityStartDateTime,
         Value_Unit = tolower(ResultMeasure.MeasureUnitCode))%>% #units for both detect and nondetect
  dplyr::filter(ResultMeasureValue != 0 , !is.na(ResultMeasureValue))%>%

  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, ResultMeasureValue, Value_Unit, Result_ND, ResultSampleFractionText,  ResultMeasure.MeasureUnitCode, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, ResultMeasureValue)%>%
  
  dplyr::mutate(#Convert ugL to mgL orthophophate to PO4-P is multiply by 0.3261 (if just mg/L  (or ug/L) then assume as PO4)
                #use df_units to create this from PhosphateCleanUp.R
                DataValue = ifelse(str_detect(Value_Unit, "mg/l") == TRUE, as.numeric(ResultMeasureValue)* 0.3261, -99), #as PO4-P
                #DataValue = ifelse(USGSPCode == "00671" | USGSPCode == "70507" | str_detect(Value_Unit, "mg/l as p") == TRUE, as.numeric(ResultMeasureValue), DataValue), #as PO4-P
                DataValue = ifelse(str_detect(Value_Unit, "mg/l as p") == TRUE, as.numeric(ResultMeasureValue), DataValue), #as PO4-P
                DataValue = ifelse(as.numeric(ResultMeasureValue) * 0.3261, DataValue), #as PO4
                #DataValue = ifelse(USGSPCode == "00660" | USGSPCode == "00650", as.numeric(ResultMeasureValue) * 0.3261, DataValue), #as PO4
                DataValue = if_else(str_detect(Value_Unit, "ug/l") == TRUE, as.numeric(ResultMeasureValue)/1000* 0.3261, DataValue),#as PO4-P
                #DataValue = if_else(USGSPCode == "91004" | str_detect(Value_Unit, "ug/l") == TRUE, as.numeric(ResultMeasureValue)/1000* 0.3261, DataValue),#as PO4-P
                DataValue = ifelse(str_detect(Value_Unit, "ppm") == TRUE, as.numeric(ResultMeasureValue)* 0.3261, DataValue), #as PO4-P
                #DataValue = ifelse(ResultDetectionConditionText == "Not Detected",DetectionQuantitationLimitMeasure.MeasureValue, DataValue),
                DataValue = ifelse(as.numeric(ResultMeasureValue) == 0 , NA, DataValue),
                DataValue = ifelse(as.numeric(ResultMeasureValue) < 0 , NA, DataValue)
  ) %>%
  mutate("Value" = DataValue,
         Unit = "mgLPO4-P",
         CharacteristicName = "Orthophosphate")%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText,Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()


#Save Cleaned Data
saveRDS(dat_PO4, "data/Data_PO4_clean.RDS")
write.csv(dat_PO4, "data/Data_PO4_clean.csv", row.names = FALSE)



##############
## Ammonium and Ammonia 
##############

##### COnvert NH4 as well #https://www.ices.dk/data/dataset-collections/Pages/Unit-Conversions.aspx
#0.776490 

nh4_df <- BaseFlowSample_allWQ %>%
  dplyr::filter(grepl('ammon', CharacteristicName))

unique(nh4_df$CharacteristicName)
unique(nh4_df$ResultMeasure.MeasureUnitCode)

nh4_df <- as.data.frame(BaseFlowSample_allWQ) %>%
  dplyr::filter(grepl('ammon', CharacteristicName)) %>%
  dplyr::select(USGSPCode, ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue,  ResultMeasure.MeasureUnitCode, ResultSampleFractionText, ActivityStartDateTime, BaseflowConditions,
                DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText)%>%
  mutate(         ResultMeasureValue = as.numeric(dplyr::coalesce(ResultMeasureValue,DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
                  ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                  site_id = MonitoringLocationIdentifier,
                  Value_Unit = ResultMeasure.MeasureUnitCode,
                  DateTime = ActivityStartDateTime,
                  Date = ActivityStartDate,
                  Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
                )
  
nh4_df$Value =  base::ifelse(nh4_df$Value_Unit == "mg/l nh4", nh4_df$ResultMeasureValue * 0.776490, nh4_df$ResultMeasureValue)

dat_NH4 <- nh4_df %>%
  mutate("Value" = Value,
         Unit = "mgLNH4-N" )%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode,  Result_ND, ResultSampleFractionText, Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit) %>%
  distinct()


saveRDS(dat_NH4, "data/Data_NH4_clean.RDS")
write.csv(dat_NH4, "data/Data_NH4_clean.csv", row.names = FALSE)

########
## Total Dissolved Solids
########
TDS_df <- as.data.frame(BaseFlowSample_allWQ) %>%
  dplyr::filter(grepl('Total dissolved solids', CharacteristicName)) %>%
  dplyr::select(USGSPCode, ActivityStartDate, MonitoringLocationIdentifier, ResultSampleFractionText, CharacteristicName, ResultMeasureValue,  ResultMeasure.MeasureUnitCode,  ActivityStartDateTime, BaseflowConditions,
                DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText)%>%
  mutate(         ResultMeasureValue = as.numeric(dplyr::coalesce(ResultMeasureValue,DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
                  ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                  site_id = MonitoringLocationIdentifier,
                  Value_Unit = ResultMeasure.MeasureUnitCode,
                  DateTime = ActivityStartDateTime,
                  Date = ActivityStartDate,
                  ResultDetection = ResultDetectionConditionText,
                  Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
  )

df_units <- TDS_df %>%
  group_by(CharacteristicName, USGSPCode, Value_Unit)%>%
  summarise(count = n())
# 1000000g in 1 metric ton or 907185g in 1 imperial ton, as ton are unclear they are dropped

TDS_df <- TDS_df %>%
  dplyr::filter(Value_Unit == "mg/l")%>%
  mutate(Value = as.numeric(ResultMeasureValue),
         Unit = "mgLTDS")%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText, Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()

saveRDS(TDS_df, "data/Data_TDS_clean.RDS")
write.csv(TDS_df, "data/Data_TDS_clean.csv", row.names = FALSE)

########################
## pH
######################
pH_df <- as.data.frame(BaseFlowSample_allWQ) %>%
  dplyr::filter(grepl('pH', CharacteristicName)) %>%
  dplyr::select(USGSPCode, MonitoringLocationIdentifier, ActivityStartDate, ResultSampleFractionText, CharacteristicName, ResultMeasureValue,  ResultMeasure.MeasureUnitCode,  ActivityStartDateTime, BaseflowConditions,
                DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText)%>%
  mutate(         ResultMeasureValue = as.numeric(dplyr::coalesce(ResultMeasureValue,DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
                  ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                  site_id = MonitoringLocationIdentifier,
                  Value_Unit = ResultMeasure.MeasureUnitCode,
                  Date = ActivityStartDate,
                  DateTime = ActivityStartDateTime,
                  ResultDetection = ResultDetectionConditionText,
                  Result_ND = ifelse(is.na(ResultDetectionConditionText), FALSE, TRUE)
  )

#Check Units 
df_units <- pH_df %>%
  group_by(CharacteristicName, USGSPCode, Value_Unit)%>%
  summarise(count = n())
#No unit conversion nessecary
pH_df <- pH_df %>%
  mutate(Value = as.numeric(ResultMeasureValue),
         Unit = "pHunit")%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText, Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()

saveRDS(pH_df, "data/Data_pH_clean.RDS")
write.csv(pH_df, "data/Data_pH_clean.csv", row.names = FALSE)



####################
## Specfic Conductance
####################

df_SPC <- (BaseFlowSample_allWQ) %>%
  dplyr::filter(CharacteristicName == "Specific conductance")%>%
  dplyr::select(USGSPCode, MonitoringLocationIdentifier, ActivityStartDate,  ResultSampleFractionText, CharacteristicName, ResultMeasureValue,  ResultMeasure.MeasureUnitCode,  ActivityStartDateTime, BaseflowConditions,
                DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText)%>%
  mutate(         ResultMeasureValue = as.numeric(dplyr::coalesce(ResultMeasureValue,DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
                  ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
                  site_id = MonitoringLocationIdentifier,
                  Value_Unit = ResultMeasure.MeasureUnitCode,
                  DateTime = ActivityStartDateTime,
                  Date = ActivityStartDate,
                  ResultDetection = ResultDetectionConditionText,
                  Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
  )

#Check Units 
df_units <- df_SPC %>%
  group_by(CharacteristicName, USGSPCode, Value_Unit)%>%
  summarise(count = n())

#No unit conversion nessecary
df_SPC <- df_SPC %>%
  mutate(Value = as.numeric(ResultMeasureValue),
         Unit = "uS/cm@25C")%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText, Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()

saveRDS(df_SPC, "data/Data_SPC_clean.RDS")
write.csv(df_SPC, "data/Data_SPC_clean.csv", row.names = FALSE)




#######################
## Join to make single dataframe 
## for SVIC display
#######################

WQ_Display <- rbind(dat_NO3, dat_PO4, dat_NH4, df_SPC, pH_df, TDS_df)%>%
  mutate(BaseflowConditions = ifelse(BaseflowConditions == 1, "Baseflow", "NonBaseflow"))

write.csv(WQ_Display , "data/WQ_SVIC_Display.csv", row.names = FALSE)
saveRDS(WQ_Display , "data/WQ_SVIC_Display.RDS")


df_Display_CharCount <- WQ_Display %>%
  group_by(CharacteristicName)%>%
  summarise(count = n())

#determining data clean up methods
# Desired Units PO4-P
# Phosphate Unit Conversions
#orthophophate to PO4-P is multiply by 0.3261

#Counts of units 
df_CharCount <- BaseFlowSample_allWQ %>%
  group_by(CharacteristicName)%>%
  summarise(count = n()) #(all have 1000+ count)


#Create Dataframe with one unit 
dat_WQ <- BaseFlowSample_allWQ %>% 
  dplyr::select(MonitoringLocationIdentifier, USGSPCode, ResultMeasureValue, CharacteristicName, ActivityStartDate, ActivityStartDateTime, ActivityStartDateTime, ResultSampleFractionText, ResultMeasure.MeasureUnitCode, DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultDetectionConditionText, BaseflowConditions) %>%
  mutate(#deal with non detects first to simply approach
    Value = dplyr::coalesce(as.numeric(ResultMeasureValue),as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)), #nondetect and detects together 
    Result_ND = ifelse(ResultDetectionConditionText == "Detect", FALSE, TRUE),
    ResultMeasure.MeasureUnitCode = tolower(dplyr::coalesce(ResultMeasure.MeasureUnitCode,DetectionQuantitationLimitMeasure.MeasureUnitCode)),
    site_id = MonitoringLocationIdentifier,
    date = ActivityStartDate,
    DateTime = ActivityStartDateTime,
    Date = ActivityStartDate,
    Value_Unit = tolower(ResultMeasure.MeasureUnitCode))%>% #units for both detect and nondetect
  dplyr::filter(Value != 0 , !is.na(Value))%>% #remove all zeros and NAs
  dplyr::select(site_id, Date,  DateTime, CharacteristicName, USGSPCode, Value, Value_Unit, Result_ND, ResultSampleFractionText,  ResultMeasure.MeasureUnitCode, BaseflowConditions)%>%
  relocate(site_id, DateTime, Value) %>%
  mutate(BaseflowConditions = ifelse(BaseflowConditions == 1, "Baseflow", "NonBaseflow"),
        Unit = Value_Unit)%>%
  dplyr::select(site_id, Date, DateTime, CharacteristicName, USGSPCode, Result_ND, ResultSampleFractionText, Value, Unit, BaseflowConditions)%>%
  relocate(site_id, Date, DateTime, Value, Unit)%>%
  distinct()

## replace cleaned values with downloaded values
dat_WQ_file <- dat_WQ %>%
  dplyr::filter(!CharacteristicName %in% Characteristic_Display)%>%
  rbind(., WQ_Display)%>%
  distinct()

write.csv(dat_WQ_file , "data/WQ_SVIC_FinalDownloadFile.csv", row.names = FALSE)
saveRDS(dat_WQ_file , "data/WQ_SVIC_FinalDownloadFile.RDS")


