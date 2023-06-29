


RefStreamSites <- function(Dam_infl, Diversion_infl, bbox_num, LIS_poly, start.date, end.date, flowlines){
  #find NWIS sites with discharge within a bounding box
  flowSiteDF <- st_as_sf(whatNWISsites(bbox = bbox_num,
                                       parameterCd = "00060", outputDataTypeCd ="dv", #only sites with daily data trims the random discharge measures
                                       startDate = start.date, endDate = end.date),
                         coords = c("dec_long_va", "dec_lat_va"),
                         crs=4326)

  
  #clip from bounding box to actual LIS polygon
  LIS_flowsites <- st_intersection(flowSiteDF, st_transform(LIS_poly, st_crs(flowSiteDF)))
  
  #Add RI as it is out of bounding box but need 0109 Q locations 
  flowSiteRI <- st_as_sf(whatNWISsites(stateCd = "RI",
                                       parameterCd = "00060", outputDataTypeCd ="dv", #only sites with daily data trims the random discharge measures
                                       startDate = start.date, endDate = end.date),
                         coords = c("dec_long_va", "dec_lat_va"),
                         crs=4326)%>%
                  mutate(LIS = NA)
  
  LIS_flowsites <- rbind(LIS_flowsites, flowSiteRI)%>%
                        distinct(site_no, .keep_all = TRUE)
  
  Dam_50km = do.call("c", strsplit(as.character(Dam_infl$downstream50km), ","))
  
  # Remove Sites that are Dam or Diversion Influenced
  NHD_sites <- NHDjoin(LIS_flowsites, flowlines) %>%
    dplyr::filter(!as.character(COMID) %in% Dam_50km)%>%
    dplyr::filter(!as.character(COMID) %in% Diversion_infl$FEATUREID)%>%
    dplyr::filter(!is.na(COMID))%>%
    dplyr::filter(!str_detect(station_nm, '(?i)dam|reservoir'))
  
}

baseflow_regression <- function(LIS_flowsites){
  
  #List of NWIS sites with Flow Data in LIS Study Area
  site_list <- LIS_flowsites %>%
    pull(site_no)
        #Conduct Data Download and Baseflow Analysis 
        baseflow_lst <- pblapply(site_list, function(thisSite){
          
          Sys.sleep(0.1)
          # Get Discharge Data for Sites with NWIS Data
          dischargeDF = renameNWISColumns(readNWISdv(thisSite, parameterCd ="00060", 
                                                     startDate = start.date, endDate = end.date))
          
          if (nrow(dischargeDF)==0) {
            print("No Daily Discharge")
            print(thisSite)
            return()
          }
          
          INFO = readNWISsite(thisSite)
          this_da = INFO$drain_area_va[1]
          
          # Clean Date Columns
          DateList = data.frame(Date = seq(from=min(dischargeDF$Date),to=max(dischargeDF$Date),by=1)) #add all dates
          dischargeDF = left_join(DateList,dischargeDF, by="Date")
          dischargeDF$Month = format(dischargeDF$Date,"%m")
          dischargeDF$MonYear = format(dischargeDF$Date,"%m-%Y")
          dischargeDF$WatYear = smwrBase::waterYear(dischargeDF$Date)
          dischargeDF$Date = as.Date(dischargeDF$Date)
          
          #fill into being dates up to 1 weeks 
          dischargeDF$flowFill <- smwrBase::fillMissing(dischargeDF$Flow, max.fill = 7)
          
          #Group Time Blocks with Continous Flow Records
          dischargeDF <- dischargeDF %>%
            dplyr::filter(!is.na(flowFill))%>%
            mutate(group = cumsum(c(TRUE, diff(Date) != 1)))
          
          
          baseflowDF = data.frame(site_no=NA,huc_cd = NA, Date=as.Date(NA),Baseflow_Frac=NA)
          
          for (g in unique(dischargeDF$group)){
            DF <- dischargeDF %>%
              dplyr::filter(group == g)
            
          #Baseflow Analysis for Continuous Time Blocks
            try({
              
              thisPART = part(DF$flowFill,DF$Date,da=this_da,STAID=thisSite)
              
              thisBFI = bfi(DF$flowFill, DF$Date, by="continuous",STAID=thisSite)
              
              thisPART$Flow[thisPART$Flow==0]=NA
              
              Q_df <- data.frame(site_no =thisSite,huc_cd=INFO$huc_cd,Date=as.Date(thisPART$Date),PART_index = thisPART$BaseQ/thisPART$Flow)%>%
                left_join(data.frame(site_no=thisSite,Date=thisBFI$Date,BFI_index = thisBFI$BaseQ/thisBFI$Flow),by=c("site_no","Date"))%>%
                dplyr::filter(complete.cases(BFI_index),complete.cases(PART_index))%>%
                group_by(site_no,huc_cd,Date)%>%
                summarize(Baseflow_Frac=mean(BFI_index,PART_index))%>%
                data.frame()
              
              baseflowDF = rbind(baseflowDF,Q_df)
              
            })#end try
          }#end groups
          #end duration loop
          baseflowDF$huc6 = substr(baseflowDF$huc_cd,1,6) 
          return(baseflowDF)
        })
        
        baseflow_output = do.call("rbind",baseflow_lst)
        #first 6 values (HUC6)
        
      return(baseflow_output)
} #end of Baseflow Function 

## Determine Times of Baseflow ##
get_daily_baseflow_stats <- function(baseflowDF, minBFSS, QFthresh){
  #get the fraction of sites at baseflow conditions for each day
  baseflowSummary = baseflowDF %>% dplyr::filter(!is.na(Baseflow_Frac))%>%
    group_by(Date, huc6)%>%
    summarize(nSites_Total = length(site_no))%>%
    left_join(baseflowDF%>%
                filter(Baseflow_Frac>=minBFSS,!is.na(Baseflow_Frac))
              %>%group_by(Date,huc6)
              %>%summarize(nSites_Baseflow = length(site_no)))%>%
    left_join(baseflowDF%>%filter(Baseflow_Frac<QFthresh,!is.na(Baseflow_Frac))%>% 
                group_by(Date,huc6)%>%summarize(nSites_Quickflow = length(site_no)))
  
  #days with NA for nSites_Baseflow should have 0
  baseflowSummary$nSites_Baseflow[is.na(baseflowSummary$nSites_Baseflow)]=0
  baseflowSummary$nSites_Quickflow[is.na(baseflowSummary$nSites_Quickflow)]=0
  baseflowSummary$PerBaseflow = baseflowSummary$nSites_Baseflow/baseflowSummary$nSites_Total*100
  baseflowSummary$BaseflowConditions = 0
  baseflowSummary$BaseflowConditions[(baseflowSummary$nSites_Quickflow==0)&(baseflowSummary$PerBaseflow>0.75)]=1
  
  return (baseflowSummary)
  
}









