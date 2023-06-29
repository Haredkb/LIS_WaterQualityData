## library() calls go here
suppressPackageStartupMessages({
  ### Targets Required
  library(conflicted)
  library(dotenv)
  library(targets)
  library(tarchetypes)
  ### Analysis Required
  library(dataRetrieval)
  library(smwrBase) #water year
  library(tidyverse)
  library(ggplot2)
  library(lubridate)
  library(sp)
  library(sf)
  library(rgdal)
  library(reshape)
  library(DVstats)
  library(nngeo)
  library(httr)
  library(nhdplusTools)
  library(utils) #for progress bar
  library(pbapply)
  library(RSQLite)
  sf_use_s2(FALSE)}
)

#Global Variable
connection <- dbConnect(RSQLite::SQLite(), "..\\LISS_N.db")
dbListTables(connection)

conflicts_prefer(dplyr::filter)
options(dplyr.summarise.inform = FALSE) #remove grouping messages

