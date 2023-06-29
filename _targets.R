## Load your packages, e.g. library(targets).
source("./packages.R")


## To Run Target, use tar_make() in the console - nothing in brackets

# target = function_to_make(arg), ## drake style
# tar_target(target2, function_to_make2(arg)) ## targets style

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  ######################
  #--- INPUT INFO -----#
  ######################
  
  #For Dam Locations
  tar_target(NID_file,
             "data/NID_NE.csv",
             format = "file"),

  tar_target(LIS_DB_sf_file,
             "data/LIS_Database_Points.shp",
             format = "file"),
  
  LIS_poly = st_read("data/LIS_watershed_GEO.shp"),
  LIS_dbLoc = st_read("data/LIS_DBpoint_snapNHD.shp"),
  flowlines = fix_flowlines("data/LIS_flowlines.shp"),#from LISS_nhd_plus_.R #readRDS("..//NHD_flowlinesHR_0110_0108.RDS"),
  catchment = st_make_valid(st_read("data/LIS_catchments.shp",crs = 4326)),#readRDS("..//NHD_catchment_0110_0108.RDS"),
  NID_df = readr::read_csv(NID_file),
  modelBoundaryFile = read_sf(LIS_DB_sf_file),
  stateList=c("VT","CT","RI", "NH", "MA"),
  CT_hydro = st_read("data/Connecticut_Hydrography_Set.shp"),
  #SET UP BASE INPUT FILES 
  model_points = st_transform(modelBoundaryFile,"epsg:4326"),
  bbox_num = round(st_bbox(model_points),2), #needs to be rounded for many DVStats Functions
  bbox_sf = st_as_sfc(st_bbox(model_points)), #creates polygon bounding box
  
  # db_stream = dbReadTable(connection, "WQP_data")%>%
  #   mutate(date = as.Date(date, format = "%Y-%m-%d")) 
  #   
  start.date <- "1950-10-01",
  end.date <- "2023-04-05", #today(),
  
  ######################################
  ## - Database Upload & NHD Connect - #
  ######################################
  
  # tar_target(LIS_DB_SW,
  #            dbReadTable(connection, "WQP_data_SW")),
  # 
  # tar_target(LIS_DB_SW_NHD,
  #            NHDjoin(LIS_dbLoc, flowlines)),
  
  tar_target(WQP_df_N, downloadWQP(get_Nsites(LIS_poly))),
  
  tar_target(WQP_loc_N, st_as_sf(attr(tar_read(WQP_df_N), "siteInfo"), 
                                 coords = c("dec_lon_va", "dec_lat_va"), crs = 4326)),
  
  tar_target(LIS_WQP_SW_NHD,
             NHDjoin(WQP_loc_N, flowlines)),
  
  
  ######################################
  ## --- Locate Dams and NHD Loc ---- ##
  ##                                  ##
  ##                                  ##
  ######################################
  minStorageAcreFt = 100,
  
  tar_target(Dam_infl, get_dam_reaches(NID_df, LIS_poly, flowlines, minStorageAcreFt)),
  
  tar_target(Diversion_infl, get_diversion_reaches(CT_hydro, flowlines, catchment)),
  
  ## Remove Dams from Baseflow 
  ######################################
  ## --- Set up Baseflow Analysis --- ##
  ## Determine times of year Baseflow ##
  ## occurs within a given HUC6       ##
  ######################################
  
  tar_target(Q_refsites, RefStreamSites(Dam_infl, Diversion_infl, bbox_num, LIS_poly, start.date, end.date, flowlines)),
  
  tar_target(baseflow_df, baseflow_regression(Q_refsites)),
  
  minBFSS = 0.75, #Minimum BFI that defines Baseflow Time of Year
  QFthresh = 0.5, #Quickflow BFI threshold
  
  tar_target(baseflow_dates, get_daily_baseflow_stats(na.omit(baseflow_df), minBFSS, QFthresh)),
  
  
  ######################################
  ## --- Locate Diversions (onlyCT)-- ##
  ## Determine times of year Baseflow ##
  ##                                  ##
  ######################################
  
  WTTP_infl = get_wwtp_downstream_reaches(stateList, bbox_sf, flowlines, extraWWTPreaches=NA), ## drake style
  #WTTP_distance to each may be helpful but long
  
  ######################################
  ## -- Filter DB data for baseflow -- #
  ##          times of year            #
  ######################################
  
  tar_target(N_refsites, RefSampleSites(Dam_infl, Diversion_infl, WTTP_infl, LIS_WQP_SW_NHD)),
  tar_target(N_sites, DefineSites(Dam_infl, Diversion_infl, WTTP_infl, LIS_WQP_SW_NHD)),
  tar_target(NO3_data, cleanUnits_NO3(WQP_df_N)),
  
  tar_target(BaseFlowSample_NO3, filterBaseFlow(LIS_WQP_SW_NHD, NO3_data, baseflow_dates)),
  
  tar_target(BaseFlowSample_allWQ, filterBaseFlow(LIS_WQP_SW_NHD, clean_NWIS_table(WQP_df_N), baseflow_dates))
  
)
