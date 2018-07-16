# Author: Bastiaen Boekelo 
# Goal: Extract zonal statistics from the datasets

# LIBRARIES
rm(list=ls())
library(raster)
library(maptools)
library(rgdal)
library(rgeos)
source('R/02a_zonal_statistics.R') # load function that calculates the zonal statistics

# Give "run" a unique name and determine the number of percelen with "selectie"
run <- "run7"
selectie <- 5:10


# FILE AND FOLDER LOCATIONS
SAT_dir <- "C:/Data/SBIR/data/Landsat_Sentinel_RapidEye/"
SHAPE_loc <- "data/percelen/01_gebied1/02_omgevormde_data"
SHAPE_filename <- "Omgevormde_data_pilotgebied_SBIR_MAAN_fase2"
STATS_dir <- "C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/"

#####################################
######## Loading raster data ########
#####################################


#####################################
###### Loading shapefile data #######
#####################################

percelen_raw <- readOGR(dsn = SHAPE_loc, layer = SHAPE_filename)

################################### select percelen ######################################

percelen <- percelen_raw
#percelen <- percelen[percelen$SHAPE_AREA > 15000,]
percelen <- percelen[,(names(percelen) %in% c("OBJECTID", "categorie", "periode"))] #remove columns

sel1 <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode != "2009_2015",]
sel2 <- percelen[percelen$categorie == "Nieuw ingetekend vlak - voorheen gemuteerd",]
#sel2 <- sel2[1:100,]
sel3 <- percelen[percelen$categorie == "Nooit gemuteerd geweest",]
#sel3 <- sel3[1:800,]
sel4 <- percelen[percelen$categorie == "Nieuw ingetekende percelen",]

sel_percelen <- rbind(sel1, sel2, sel3, sel4)
sel_percelen <- sel_percelen[selectie,]

###################################
######## Zonal statistics #########
###################################

ZONAL_STATS(SAT_dir, sel_percelen, STATS_dir, 50)

result_df <- as.data.frame(result, row.names=F)
names(result_df) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
write.csv(result_df,paste(STATS_dir, "Perceelstats_", run, ".csv", sep=""), row.names=F)







