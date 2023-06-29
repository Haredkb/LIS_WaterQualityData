

#########

NHDjoin <- function(site_loc, flowlines){
  
  #find COMID ID
  st_crs(site_loc) <- 4326
  LIS_geom <- st_geometry(site_loc)
  site_loc$comid <- NA
  
  # flowpath <- st_as_sf(flowlines, crs = 4326) %>%
  #                       dplyr::select(.,
  #                             comid = COMID,
  #                             totda = TotDASqKM,
  #                             nameid = GNIS_Name,
  #                             REACHCODE,
  #                             ToMeas,
  #                             FromMeas)
  # #sf::st_point(LIS_geom)
  
  
  indexes <- get_flowline_index(flowlines,
                                site_loc,
                                max_matches = 1)
  
  site_loc$row_num = as.integer(row.names(site_loc))
  
  output <- left_join(site_loc, indexes, by = c("row_num" = "id") )
  
  # for (i in 1:length(LIS_geom)){
  #   LIS_dbLoc[i,]$comid <- tryCatch(
  #       #discover_nhdplus_id(LIS_geom[i]), #3/27/2023 Gives same value multiple points that are clearly unique
  #     error = function(e) return(NA)
  #   )
  # }

  
}

#index <- nhdplusTools::get_flowline_index(LIS_geom[i])
##########







