# Author: Bastiaen Boekelo 
# Goal: Extract zonal statistics from the datasets

# LIBRARIES
rm(list=ls())
library(raster)
library(maptools)
library(rgdal)
library(rgeos)


# FILE AND FOLDER LOCATIONS
SAT_dir <- "C:/Data/SBIR/data/Landsat_Sentinel_RapidEye/"
SHAPE_loc <- "C:/Data/SBIR/data/Pilotdata_omgevormd/Omgevormde_data_pilotgebied_SBIR_MAAN_fase2.shp"
STATS_dir <- "C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/"

# Give "run" a unique name and determine the number of percelen with "selectie"
run <- "run7"
selectie <- 1321:1540

# Loading shapefile & make selection (2015-2016)

#####################################
######## Loading raster data ########
#####################################

images <- list.files(path = SAT_dir, pattern = "tct.img$", full.names = T)

#####################################
###### Loading shapefile data #######
#####################################

percelen_raw <- readShapePoly(SHAPE_loc)

################################### select percelen ######################################


# NOTE
# Meest geïnteresseerd in de vlakken die nooit gemuteerd zijn geweest. Daar hebben we 800 percelen van geselecteerd
# Daarnaast het meest geïnteresseerd in de percelen waarvan we met redelijke zekerheid kunnen zeggen dat deze
# in een bepaald jaar zijn gemuteerd. Dat zijn percelen met begindatum JAAR en einddatum JAAR + 1. Deze worden later geselecteerd,
# hier worden alle gemuteerde percelen meegenomen (=sel2)
# Ook is hier een voorselectie gedaan op percelen die kleiner zijn dan 15000 m2. Dit is om in eerste instantie een hogere betrouwbaarheid van de
# statistieken te genereren. Later kunnen we deze ook meenemen en bijvoorbeeld strengere thresholds maken voor deze percelen.

percelen <- percelen_raw[percelen_raw$SHAPE_AREA > 15000,]
percelen <- percelen[,(names(percelen) %in% c("OBJECTID","categorie", "periode"))] #remove columns

sel1 <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode != "2009_2015",]
sel2 <- percelen[percelen$categorie == "Nieuw ingetekend vlak - voorheen gemuteerd",]
sel2 <- sel2[1:100,]
sel3 <- percelen[percelen$categorie == "Nooit gemuteerd geweest",]
sel3 <- sel3[1:800,]
sel4 <- percelen[percelen$categorie == "Nieuw ingetekende percelen",]

sel_percelen <- rbind(sel1, sel2, sel3, sel4)
sel_percelen <- sel_percelen[selectie,]
sel_percelen_df <- as.data.frame(sel_percelen)

###################################
######## Zonal statistics #########
###################################

# Note
# Hier worden verschillende statistieken berekend per IMAGE per PERCEEL en per TCT BAND. Deze worden later gebruikt voor het maken van variabelen in script 2.
# Om niet per pixel te kijken was vooral een pragmatische reden: het kost uiteindelijk een stuk minder rekentijd en het is een intuitieve en makkelijke
# manier van het vergelijkbaar maken van statistieken van meerder satellietbronnen (makkelijker dan kleine pixel by pixelwaardes)

i <- 0
result <- matrix(, nrow = 0, ncol = 13)
for(IMAGE in 1:length(images)){ # calculate for every raster
  img <- stack(images[IMAGE])  # pick one raster
  checkmax <- mean(maxValue(img)) # if 0 --> do not make zonal stats
  beeld <- substr(names(img[[1]]), 1, (nchar(names(img[[1]]))-2))
  if(checkmax > 0){
    for(PERCEEL in 1:length(sel_percelen)){
      perceel <- sel_percelen[PERCEEL,]
      objectid <- perceel@data$OBJECTID
      #area <- area_percelen[PERCEEL]
      nonoverlap <- is.null(intersect(perceel, extent(img)))
      if(nonoverlap == FALSE){
        img_crop  <- crop(img, perceel)
        img_mask  <- mask(img_crop, perceel)
        for(BAND in 1:3){
          i <- i + 1
          pixels <- as.matrix(img_mask[[BAND]])
          b_max <- max(pixels, na.rm=T)
          b_min <- min(pixels, na.rm=T)
          b_q05 <- as.numeric(quantile(pixels, 0.05, na.rm=T))
          b_q10 <- as.numeric(quantile(pixels, 0.10, na.rm=T))
          b_q90 <- as.numeric(quantile(pixels, 0.90, na.rm=T))
          b_q95 <- as.numeric(quantile(pixels, 0.95, na.rm=T))
          b_mean <- mean(pixels, na.rm=T)
          b_median <- median(pixels, na.rm=T)
          b_sd <- sd(pixels, na.rm=T)
          npixels <- length(pixels) - sum(is.na(pixels))        
          
          newrow <- c(objectid, beeld, BAND, npixels, b_mean, b_sd, b_median, b_max, b_min, b_q05, b_q10, b_q90, b_q95)#, area)
          result <- rbind(result, newrow)
        }
      }
      print(paste(Sys.time(), i, beeld, " ", objectid))
    }
  }
  write.csv(result,paste(STATS_dir, run, "/perceelstats.csv", sep=""), row.names=F)
}


result_df <- as.data.frame(result, row.names=F)
names(result_df) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
write.csv(result_df,paste(STATS_dir, "Perceelstats_", run, ".csv", sep=""), row.names=F)







