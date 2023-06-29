# get_dam_reaches <- function (NID_df, LIS_poly, catchment , flowlines ,minStorageAcreFt){
# 
#     #keep all Water Supply Dams and dams with normal storage > 100 acre-Ft
#   damSF = st_as_sf(NID_df%>%
#     dplyr::filter(Primary.Purpose=="Water Supply" | Normal.Storage..Acre.Ft.>minStorageAcreFt),
#     coords = c("Longitude", "Latitude"),
#     crs=4326)
#   
#   #filter dams to the model area
#   #clip from bounding box to actual LIS polygon
#   LIS_damSites <- st_intersection(damSF, st_transform(LIS_poly, st_crs(damSF)))
#   
#   #read in the NHDcatchments
#   
#   #get the catchments with dams
#   damSF = st_transform(LIS_damSites,st_crs(catchment))
#   catchmentSF = st_filter(catchment,damSF)
#   
#   #read in the NHDFlowlines
#   flowlines = flowlines %>% 
#     dplyr::filter((FCODE %in% c(55800, 46000, 46003, 46006, 46007))) #flowlines associated with Stream/River or Artifical Path
#   #dplyr::filter(FNAME %in% c("ArtificialPath","StreamRiver"))
#   
#   # add comids to reaches
#   #fline <- nhdplusTools::get_tocomid(flowlines, add = TRUE)
#   #fl <- dplyr::select(fline, ID = comid, toID = tocomid, lengthkm)
#   
#   #empty dataframe
#   
#   #resultsDF = data.frame(COMID=NA,UpstreamCOMID=NA, DrainageArea=NA, DamPurpose = NA,mostDownstream = NA)
#   
#   resultsDF <- lapply(na.omit(catchmentSF$FEATURE), function(thisCOMID){
#     
#           tryCatch({
#           #which dams are within a catchment
#           thisDamSF = st_intersection(damSF,catchmentSF%>%
#                                         dplyr::filter(FEATURE==thisCOMID))
#           
#           #get the list of reaches below this dam
#           reachList = get_DM(st_drop_geometry(flowlines),thisCOMID, sort=TRUE)
#               
#           
#           #get find out if any of the downstream reaches are also dam reaches, 
#           #1 = no additional dams between this one and the given reach, 0 = additional dam below this one
#           damReach = ifelse(!(reachList %in% catchmentSF$FEATURE[catchmentSF$FEATURE!=thisCOMID]),1,0) #this ctachmentSF is the list of catchment with dams
#           
#           # #find distance from dam to 
#           # dam_dist <- get_path_lengths(reachList, fl) %>%
#           #                 dplyr::filter(ID_1 == thisCOMID)%>%
#           #                 .[1:(nrow(.)/2)+1,]
#           
#           #make any values after a 0 equal to 0 (b/c there is another downstream dam before that reach)
#           if (0 %in% damReach) damReach[min(which(damReach==0)):length(damReach)]=0
#           
#           
#           if (length(reachList)>0) resultsDF = na.omit(rbind(resultsDF,
#                                                              data.frame(UpstreamCOMID=thisCOMID,
#                                                                         COMID=reachList,
#                                                                         DrainageArea=thisDamSF%>%
#                                                                                     st_drop_geometry()%>%
#                                                                                     summarize(maxArea = max(Drainage.Area..Sq.Miles.))%>%
#                                                                                     dplyr::pull(maxArea),DamPurpose=paste(unique(thisDamSF$Primary.Purpose),collapse=", ",sep=""), 
#                                                                                    mostDownstream = damReach)))
#           
#         }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})#try catch
#     
#     df <- try(data.frame(UpstreamCOMID=thisCOMID,
#                      COMID=reachList,
#                      DrainageArea=thisDamSF%>%
#                        st_drop_geometry()%>%
#                        summarize(maxArea = max(Drainage.Area..Sq.Miles.))%>%
#                        dplyr::pull(maxArea),DamPurpose=paste(unique(thisDamSF$Primary.Purpose),collapse=", ",sep=""), 
#                      mostDownstream = damReach))
#     
#   })
#   
# 
#   
#   
#   #aggregate the dam data by downstream COMID
#   resultsDF = resultsDF%>%
#     group_by(COMID)%>%
#     summarize(DrainageArea = sum(DrainageArea*mostDownstream),
#               UpstreamCOMID = paste(UpstreamCOMID,sep="",collapse=", "),
#               DamPurpose=paste(DamPurpose,sep="",collapse=", "))
#   
#   
#   return (resultsDF)
#   
# }

get_dam_reaches <- function(NID_df, LIS_poly, flowlines, minStorageAcreFt){
  
  #keep all Water Supply Dams and dams with normal storage > 100 acre-Ft
  damSF = st_as_sf(NID_df%>%
                     dplyr::filter(Primary.Purpose=="Water Supply" | Normal.Storage..Acre.Ft.>minStorageAcreFt),
                   coords = c("Longitude", "Latitude"),
                   crs=4326)
  
  #filter dams to the model area
  #clip from bounding box to actual LIS polygon
  LIS_damSites <- st_intersection(damSF, st_transform(LIS_poly, st_crs(damSF)))
  
  # snap points to closest flowline
  #flowlines <- nhdplusTools::get_tocomid(flowlines, add = TRUE)
  NHD_dam <- sf::st_nearest_feature(st_geometry(LIS_damSites), st_geometry(st_transform(flowlines, st_crs(LIS_damSites))))
  
  #dis2flow <- st_distance(LIS_damSites, flowlines)
  LIS_damSites <- LIS_damSites %>%
    mutate(COMID = flowlines$ToNode[NHD_dam])%>%
    dplyr::filter(!is.na(COMID))
  
  resultsDF <- lapply(LIS_damSites$COMID, function(thisCOMID){
    
    tryCatch({
      #get the list of reaches below this dam
      reach50 = get_DD(st_drop_geometry(flowlines),thisCOMID, distance = 50)
      reach10 = get_DD(st_drop_geometry(flowlines),thisCOMID, distance = 10)
      reachList50=paste(unique(reach50),collapse=", ",sep="")  
      reachList10=paste(unique(reach10),collapse=", ",sep="")  
    #unlist(strsplit(reachList10, split=", ")) #how to undo
    if (length(reachList10)>0) resultsDF = na.omit(data.frame(COMID=thisCOMID,
                                                                  downstream50km=reachList50,
                                                                  downstream10km=reachList10
                                                            ))
    },
    error=function(e) {
      e
      print(paste("Oops! --> Error for COMID ",thisCOMID,sep = ""))
    }
    )
    
  })
  
  LIS_damSites <- left_join(LIS_damSites, do.call("rbind", resultsDF))

  
}#end funcion



    
# Reaches_BelowDam <-  function(Dam_infl, flowlines, distance){
#   
#   reachList <-  lapply(unique(Dam_infl$COMID), function(dam_comid){
#     
#     tryCatch(get_DD(st_drop_geometry(flowlines),dam_comid, distance), error=function(err) NA)
#     
#   })
#   
#   output <- c(do.call(c, reachList), unique(Dam_infl$COMID))
#   

get_diversion_reaches <- function(ct_deep_hydrography, flowlines, catchment){
  #########################3
  ### get the diversions from the CT DEEP hydrography layer
  #filter ctHydro
  ctHydro = ct_deep_hydrography %>%
    dplyr::filter(HYDRO_ARC %in% c("Aqueduct","Canal, Lock, or Sluice Gate","Underground Aqueduct","Spillway","Ditch or Canal"))      
  
  #get the endpoints
  endPts = st_cast(ctHydro,"POINT")
  
  #keep the first and last points for each line
  ptDF = data.frame(ptInd = rownames(endPts),ptGrp = floor(as.numeric(rownames(endPts))))
  ptDF = ptDF%>%group_by(ptGrp)%>%filter(ptInd==min(ptInd) | ptInd==max(ptInd))
  endPts = endPts[rownames(endPts) %in% ptDF$ptInd,]
  
  #########################
  # ### get the diversions from NHDPlus
  # #read in the flowlines
  # pipelines = flowlines %>% 
  #   dplyr::filter(str_detect(FCODE, "428$")) #FTYPE=="Pipeline")
  # #get the endpoints
  # endPts2 = st_cast(pipelines,"POINT")
  # 
  # if(nrow(endPts2) >= 1 ){
  #   #keep the first and last points for each line
  #   ptDF2 = data.frame(ptInd = rownames(endPts2),ptGrp = floor(as.numeric(rownames(endPts2))))
  #   ptDF2 = ptDF2%>%group_by(ptGrp)%>%filter(ptInd==min(ptInd) | ptInd==max(ptInd))
  #   endPts2 = endPts2[rownames(endPts2) %in% ptDF2$ptInd,]
  #   
  #   #combine the 2 sets of endpoints
  #   endPts = try(st_transform(endPts2,st_crs(endPts)))
  #   endPts = try(rbind(st_zm(endPts)%>%dplyr::select(geometry),st_zm(endPts2)%>%dplyr::select(geometry)))
  # }
  
  
  #get the catchments with diversions
  endPts = st_transform(endPts,st_crs(catchment))
  catchmentSF = st_filter(catchment,endPts)
  
  return (catchmentSF)
  
}

