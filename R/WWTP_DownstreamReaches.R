get_wwtp_downstream_reaches <- function(stateList, bbox_sf, flowlines, extraWWTPreaches=NA){

  #get the WWTP's (call format from: https://echo.epa.gov/tools/web-services/facility-search-water#/)
  result = GET(paste("https://echodata.epa.gov/echo/cwa_rest_services.get_facility_info?p_st=",paste(stateList,collapse=",",sep=""),"&p_sic=4952",sep=""))
  dat <- jsonlite::fromJSON(content(result, as="text"),  flatten=TRUE)
  
  result2 = GET(paste("https://echodata.epa.gov/echo/cwa_rest_services.get_download?qid=",dat$Result$QueryID,"&qcolumns=1%2C2%2C23%2C24%2C25%2C151%2C152%2C140",sep=""))
  epaFacilityDF = content(result2)%>%data.frame()
  
  facilitySF = st_as_sf(epaFacilityDF,coords=c("FacLong","FacLat"),crs=4326)
  
  #keep only the facilities within the model boundary
  facilitySF = st_filter(facilitySF,st_transform(bbox_sf,st_crs(facilitySF)))
  
  #remove facilities that discharge to coastal watershed
  facilitySF = facilitySF%>%filter(!CWPStateWaterBodyName %in% c("LONG ISLAND SOUND/NEW HAVEN HARBOR", "LONG ISLAND SOUND","N/A INCINERATOR", "NEW HAVEN HARBOR","STAMFORD HARBOR","RHODE ISLAND SOUND","STONINGTON HARBOR", "NARRAGANSETT BAY","BRANFORD HARBOR","CEDAR CREEK/LONG ISLAND SOUND"))
  
  facilitySF = st_transform(facilitySF,crs = st_crs(flowlines))%>%st_zm()
  
  joinDF = st_nn(st_zm(facilitySF),st_zm(flowlines),sparse=TRUE,returnDist=TRUE)
  joinDF = data.frame(nn = unlist(joinDF$nn),dist_m = unlist(joinDF$dist))
  facilitySF['COMID']=flowlines$COMID[joinDF$nn]
  facilitySF['dist_m']=joinDF$dist
  
  facilitySF = facilitySF%>%
    left_join(st_drop_geometry(flowlines[,c("COMID","StreamOrde",'FTYPE')]),by=c("COMID"="COMID"))%>%
    dplyr::filter(FTYPE!="Coastline")
  
  #return all downstream COMID from WWTP
  #resultsDF = data.frame(COMID=NA,UpstreamCOMID=NA)
  
  results_lst <- lapply(na.omit(facilitySF$COMID), function(thisCOMID){ #,extraWWTPreaches
    reachList = try(get_DD(st_drop_geometry(flowlines),thisCOMID), silent = TRUE)
    df = try(data.frame(COMID=reachList, WWTP_COMID=thisCOMID))
  })
  
  resultsDF <- do.call(rbind, na.omit(results_lst))
  return(resultsDF)
  
}
