fix_flowlines <- function(flowline_shp){
  
  proper_NHDcol <- readRDS("data/NHD_column_names.RDS")
  x <- proper_NHDcol[! proper_NHDcol %in% c('Shape')]
  x <- c(x, "geometry")
  
  flowlines <- st_make_valid(read_sf(flowline_shp,crs = 4326))
  names(flowlines) <- x
  
  st_geometry(flowlines) <- "geometry"
  
  return(flowlines)
}

get_Nsites <- function(LIS_poly){
  #Create Bounding Box
  BB <- round(st_bbox(LIS_poly),3)
  poly_crs <- st_crs(LIS_poly)
  
  N.names <-  c("Nitrate")
  
  
  NitrateSites <- whatWQPsites(bBox=BB,
                               characteristicName = N.names)# "Nitrate")

  #trim the site extent to true LIS watershed
  WQP_sites <- #LISS_WQP %>%
    NitrateSites %>%
    #st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs =4269)%>%
    st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs =4269)%>%
    #dplyr::filter(grepl('Well', MonitoringLocationTypeName))%>%
    dplyr::filter(MonitoringLocationTypeName == "Stream" | MonitoringLocationTypeName == "River/Stream")%>%
    #river_names <- c("River/Stream", "Stream", "Stream: Canal", "Stream: Tidal stream", "Stream: Ditch", "Wetland Riverine-Emergent")
    st_intersection(., LIS_poly)#trim from boundingbox to true watershed shape
}

downloadWQP <- function(N_WQP_sf){
 readWQPdata(siteNumbers = unique(N_WQP_sf$MonitoringLocationIdentifier)) }


RefSampleSites <- function(Dam_infl, Diversion_infl, WTTP_infl, LIS_WQP_SW_NHD){
  
  Dam_10km = do.call("c", strsplit(as.character(Dam_infl$downstream10km), ", ")) #include space
  # Remove Sites that are Dam or Diversion Influenced
  NHD_sites <- LIS_WQP_SW_NHD %>%
    dplyr::filter(!as.character(COMID) %in% Dam_10km)%>%
    dplyr::filter(!as.character(COMID) %in% Diversion_infl$FEATURE)%>%
    dplyr::filter(!as.character(COMID) %in% WTTP_infl$COMID)%>%
    dplyr::filter(!is.na(COMID))%>%
    dplyr::filter(!str_detect(station_nm, '(?i)dam|reservoir'))%>%
    mutate(huc6 = substr(.$HUCEightDigitCode,1,6))
  
}

DefineSites <- function(Dam_infl, Diversion_infl, WTTP_infl, LIS_WQP_SW_NHD){
  
  Dam_10km = do.call("c", strsplit(as.character(Dam_infl$downstream10km), ", ")) #include space
  # Remove Sites that are Dam or Diversion Influenced
  NHD_sites <- LIS_WQP_SW_NHD %>%
    mutate(site_infl = ifelse(as.character(COMID) %in% Dam_10km, "Dam10k", NA),
           site_infl = ifelse(as.character(COMID) %in% Diversion_infl$FEATURE, "Diversion", site_infl),
           site_infl = ifelse(as.character(COMID) %in% WTTP_infl$COMID, "WWTP", site_infl),
           site_infl = ifelse(str_detect(station_nm, '(?i)dam|reservoir'), "Dam", site_infl))%>%
    dplyr::filter(!is.na(COMID))%>%
    mutate(huc6 = substr(.$HUCEightDigitCode,1,6))
  
}

cleanUnits_NO3 <- function(db){
  
  ## FIGURE OUT UNITS , using USGSPcode
  ## 91003 - Nitrate, water, filtered, micrograms per liter as nitrate (ug/L)
  ## 83353 - Nitrate, bulk atmospheric deposition, filtered, milligrams per liter as nitrate (mg/L) ***********REMOVE
  ## 00618 - Nitrate, water, filtered, milligrams per liter as nitrogen (mg/L NO3 as N)
  ## 71851 - Nitrate, water, filtered, milligrams per liter as nitrate (mg/L NO3)
  ## 00620 - Nitrate, water, unfiltered, milligrams per liter as nitrogen 
  ## 71850 - Nitrate, water, unfiltered, milligrams per liter as nitrate (mg/L NO3)
  ## NA - Use MethodDescriptionText:
  ##             - "Nitrate-Nitrite Nitrogen by Colorimetry" is mg/L as N - https://www.nemi.gov/methods/method_summary/4702/
  ##            - mg/L as N https://www.nemi.gov/methods/method_summary/4680/
  ###           - mg/L as N  https://www.nemi.gov/methods/method_summary/7416/
  #             - mg/L as N   https://www.nemi.gov/methods/method_summary/9888/
  ## NEED TO ADD NON DETECT - 
  WQP_dat_NO3clean <- db %>% #db %>%#N_WQP_loc)%>%
    dplyr::filter(str_detect(CharacteristicName, "Nitrate"))%>% 
    dplyr::filter(USGSPCode != 83353) %>% #remove atmospheric deposition values 
    dplyr::mutate(date = ActivityStartDate,
                  DateTime = as.POSIXct(paste(ActivityStartDate, ActivityStartDateTime), format="%Y-%m-%d %H:%M:%S"),
                  #Convert ugL to mgL have to assume in NO3 - divide by 4.427 to convert to NO3-N
                  DataValue = ifelse(USGSPCode == "91003", (as.numeric(ResultMeasureValue)/1000)/4.427, NA),#91003 - Nitrate, water, filtered, micrograms per liter as nitrate (ug/L)
                  DataValue = ifelse(USGSPCode == "71851" | USGSPCode == "71850", as.numeric(ResultMeasureValue)/4.427, DataValue),
                  DataValue = ifelse(USGSPCode == "00618" | USGSPCode == "00620", as.numeric(ResultMeasureValue), DataValue),
                  DataValue = ifelse(is.na(DataValue) & !is.na(MethodDescriptionText), as.numeric(ResultMeasureValue), DataValue),
                  DataValue = ifelse(as.numeric(ResultMeasureValue) == 0 , 0, DataValue),
                  DataValue = ifelse(as.numeric(ResultMeasureValue) < 0 , NA, DataValue),
                  DataValue = ifelse(is.na(DataValue) & ResultDetectionConditionText == "Not Detected", 
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
    dplyr::select(MonitoringLocationIdentifier, USGSPCode, ActivityStartDate, ActivityStartDateTime, ActivityStartDateTime, date, DateTime, DataValue, ResultMeasureValue,ResultMeasure.MeasureUnitCode, DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
    mutate("Value_mgLNO3-N" = DataValue)
  
}

filterBaseFlow <- function(loc, WQP_df_N, baseflow_dates){
  
  #Create summarized baseflow for times with no flow records   
  baseflow_avg <- baseflow_dates %>%
    ungroup()%>%
    na.omit()%>% #make sure clean 
    group_by(Date)%>%
    summarise(LIS_avg = min(BaseflowConditions)) #if any are not baseflow then it will be zero
  
  #Make sure all Date are present 
  lis_sample_seq <- seq.Date(min(baseflow_avg$Date), max(baseflow_avg$Date), by = 1)
  
  baseflow_avg <- left_join(data.frame(Date = lis_sample_seq), baseflow_avg)
  
  df_BF <- WQP_df_N %>%
    mutate(site_no = as.character(MonitoringLocationIdentifier),
           Date = as.Date(ActivityStartDate)
           ) %>%
    inner_join(., loc)%>%
    mutate(Date1 = as.Date(Date),
           huc6 = substr(HUCEightDigitCode,1,6)) #have date to evaluate at end of df
    
  #Find if Dates are Baseflow   
  output <- left_join(df_BF, baseflow_dates, by = c("Date", "huc6"))%>%
    left_join(., baseflow_avg)%>%
    mutate(BaseflowConditions = ifelse(is.na(BaseflowConditions), LIS_avg, BaseflowConditions)) #1 will be baseflow 0 will be everything else

}


filterParameter <- function(df, characteristicName){
  
  output <- df %>%
    dplyr::filter(CharacteristicName == characteristicName)
  
  message(paste("Units included are:", unique(output$ResultMeasure.MeasureUnitCode)))
  
  return(output)
}

clean_NWIS_table <- function(data){
  p_exclude <- c("Depth", "Count", "Stream flow, instantaneous")
  
  #evaluate what parameters exist
  LISS_nwis_parameters<- data %>%
    group_by(CharacteristicName)%>%
    summarise(n = n())%>%
    dplyr::arrange(desc(n))%>%
    dplyr::filter(n >= 1000)%>%
    dplyr::filter(!CharacteristicName %in% p_exclude)%>%
    pull(CharacteristicName)
  
  All_WQ <- data %>%
    dplyr::filter(CharacteristicName %in% LISS_nwis_parameters)%>%
    mutate(DateTime = ActivityStartDateTime,
           site_id = MonitoringLocationIdentifier)
  
  return(All_WQ)}





get_N_siteList <- function(startDate,endDate, modelBoundaryFile,
                           wwtp_downstream_reaches,
                           diversionReaches,
                           damReaches, 
                           gages3File, 
                           refGagesCTFile,
                           gageLocFile,
                           NHDPath, 
                           maxDamDrainage){
  
  modelBoundary = st_read(modelBoundaryFile)
  modelBoundary = st_transform(modelBoundary,"epsg:4269")
  bbox = st_bbox(modelBoundary)
  
  ### read in the available nitrogen data
  
  parameterCd=c("00631")
  
  #n measurements from CT / NY / RI
  siteDF = NA
  for (thisCD in parameterCd){
    tempDF = whatNWISsites(parameterCd=thisCD,bBox=paste(sprintf("%0.7f",bbox),sep="",collapse=","), siteType="ST",startDT=startDate,endDT=endDate)
    tempDF = tempDF[,-which(names(tempDF)=="queryTime")]
    if (is.na(siteDF)){
      siteDF = tempDF
    } else {
      siteDF = rbind(siteDF,tempDF)	
    }
  }
  
  siteDF = unique(siteDF)
  siteDF = readNWISsite(siteDF$site_no)
  
  #remove tidal sites
  siteDF = siteDF%>%dplyr::filter(site_tp_cd == "ST")
  
  
  #filter to the sites within the model domain
  siteSF <- st_as_sf(siteDF%>%data.frame(), coords = c("dec_long_va","dec_lat_va"), crs='epsg:4269')
  siteSF = st_filter(siteSF,modelBoundary)
  
  #remove non-reference sites
  nonRefDF = read.csv(gages3File,colClasses="character")%>%dplyr::filter(NATIONAL.REF.ALL.HIGH.LOW!="REF_ALL")
  siteSF = siteSF%>%dplyr::filter(!site_no %in% nonRefDF$GAGEID.txt)
  
  nonrefCT = read.xlsx(refGagesCTFile,sheet = "Sheet1",startRow=2)%>%dplyr::rename("site_no"= "STAID")%>%filter(!grepl("index",index))
  siteSF = siteSF%>%dplyr::filter(!site_no %in% nonrefCT$site_no)
  
  #join the gage sites to the nhdplus network
  #read in the gage location file
  gageSF = geojson_sf(gageLocFile)
  siteSF = siteSF %>%left_join(gageSF%>%dplyr::select(id,nhdpv2_COMID)%>%st_drop_geometry(),by=c("site_no"="id"))%>%dplyr::rename("COMID"="nhdpv2_COMID")
  
  #get the COMID's for sites that aren't in the gageLocFile
  testDF = get_NHDPlus_reaches(siteSF%>%filter(is.na(COMID)),NHDPath)
  siteSF[is.na(siteSF$COMID),"COMID"]=testDF$COMID
  
  
  #filter out sites downstream of WWTP's
  siteSF = siteSF%>%filter(!COMID %in% wwtp_downstream_reaches$COMID)
  
  #filter out sites downstream of diversions
  siteSF = siteSF%>%filter(!COMID %in% diversionReaches$FEATUREID)
  
  #filter out sites downstream of dams
  #first identify dams that control more than 20% of the drainage area for a downstream site OR are used for public water supply
  damReaches = damReaches%>%left_join(siteSF%>%st_drop_geometry()%>%group_by(COMID)%>%summarize(siteDrainage = max(drain_area_va,na.rm=TRUE))%>%dplyr::select(COMID,siteDrainage))%>%mutate(DamPer = DrainageArea/siteDrainage)%>%filter(DamPer>maxDamDrainage | grepl("Water Supply",DamPurpose))
  
  siteSF = siteSF%>%filter(!COMID %in% damReaches$COMID)
  
  #keep the dec lat / long
  siteSF = siteSF %>%left_join(siteDF%>%dplyr::select(site_no,dec_lat_va,dec_long_va))
  
  
  
  return (siteSF)
}
