# Author: Bastiaen Boekelo 
# Date: July 2018
# Goal: Extract zonal statistics from the datasets

# LIBRARIES
rm(list=ls())
library(raster)
library(maptools)
library(rgdal)
library(rgeos)
library(foreach)
library(doParallel)
library(doSNOW)
source('R/02a_zonal_statistics.R') # load function ZONAL_STATS that calculates the zonal statistics 

##################################
#### BEPAAL NUMMER VAN JE RUN #### HANDMATIG!   ...   de rest gaat vanzelf.
##################################

#cl<-makeCluster(15) #change the 2 to your number of CPU cores
#registerDoSNOW(cl)

runners <- c(6,7,8,9,10)
selecties <- list(list(6:8), list(9:11), list(12:15), list(15:17), list(18:20), list(6:8), list(9:11), list(12:15), list(15:17), list(18:20), list(26:30), list(31:35), list(36:40))


# FILE AND FOLDER LOCATIONS
SAT_dir        <- "C:/Data/SBIR/data/Landsat_Sentinel_RapidEye/"
SHAPE_loc      <- "data/percelen/01_gebied1/02_omgevormde_data"
SHAPE_filename <- "Omgevormde_data_pilotgebied_SBIR_MAAN_fase2"
STATS_dir      <- "C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/"

##################################################
######## Loading raster and shapefiledata ########
##################################################

# Percelen (deze moeten voorbewerkt zijn (dit gebeurt nu nog met een FME workbench))
percelen_raw <- readOGR(dsn = SHAPE_loc, layer = SHAPE_filename)

# Rasters (lijst van, deze worden later een voor een ingelezen binnen de functie die de zonal statistics berekent)
images <- list.files(path = SAT_dir, pattern = "tct.img$", full.names = T)

###################################
######## Selectie percelen ########
###################################

percelen <- percelen_raw # percelen <- percelen[percelen$SHAPE_AREA > 15000,]
percelen <- percelen[,(names(percelen) %in% c("OBJECTID", "categorie", "periode"))] #remove columns

#sel1 <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode != "2009_2015",]
sel1a <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode == "2015_2016",][1:5,]
sel1b <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode == "2016_2017",][1:5,]
sel2 <- percelen[percelen$categorie == "Nieuw ingetekend vlak - voorheen gemuteerd",]
#sel3 <- percelen[percelen$categorie == "Nooit gemuteerd geweest",]
sel3 <- percelen[percelen$categorie == "Nooit gemuteerd geweest",][1:10,]

sel4 <- percelen[percelen$categorie == "Nieuw ingetekende percelen",]

#all_percelen <- rbind(sel1, sel2, sel3, sel4)
all_percelen <- rbind(sel1a, sel1b, sel3)


RUNNER <- 6


for(RUNNER in runners){
  NUMMER_RUN <- RUNNER
  selectie <- unlist(selecties[NUMMER_RUN])
  sel_percelen <- all_percelen[selectie,]
  

  ##################################
  ##################################
  ##################################
  
  
  if(nchar(as.character(NUMMER_RUN)) == 1){
    run <- paste("run0", as.character(NUMMER_RUN), sep="")
  } else {
    run <- paste("run", as.character(NUMMER_RUN), sep="")
  }
  
  ###################################
  ######## Zonal statistics #########
  ###################################
  
  # Berekent zonal statistics (raster locations, percelen, opslaglocatie statistieken, aantal-pixels treshold)
  # In de functie wordt een schatting gemaakt van het aantal pixels dat binnen het perceel valt. De laatste term bepaalt hoeveel pixels er minimaal in moeten zitten om het perceel mee te nemen in de analyse
  
  ZONAL_STATS(images, sel_percelen, STATS_dir, "temp/zonal_stats_",  20) # returns the variable 'result' -> LET OP, definieer aantal pixels
  
  ###################################
  ######## Schrijf data weg #########
  ###################################
  
  
  result_df <- as.data.frame(na.omit(result), row.names=F) # Hier worden ook NA's weggegooid van percelen alleen overlap hadden met NA pixels
  names(result_df) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
  write.csv(result_df,paste(STATS_dir, "01_chunks/", "zonal_stats_", run, ".csv", sep=""), row.names=F)
  
}
#stopCluster(cl)






