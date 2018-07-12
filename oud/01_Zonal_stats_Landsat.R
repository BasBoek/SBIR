# Author: Bastiaen Boekelo 
# Goal: Extract zonal statistics from the datasets

# LIBRARIES
rm(list=ls())
library(raster)
library(maptools)
library(rJava)

# FILE AND FOLDER LOCATIONS
LS_dir <- "C:/BASTIAEN/SBIR/data/Landsat8/landsat 8 OLI_TIRS C1 TCT/"


# Loading shapefile & make selection (2015-2016)

#####################################
######## Loading raster data ########
#####################################

images <- list.files(path = LS_dir, pattern = "tct.img$", full.names = T)

#####################################
###### Loading shapefile data #######
#####################################


percelen_raw <- readShapePoly("C:/BASTIAEN/SBIR/data/Pilotdata_omgevormd/Omgevormde_data_pilotgebied_SBIR_MAAN_fase2.shp")
percelen <- percelen_raw[percelen_raw$SHAPE_AREA > 15000,]
hist(percelen$SHAPE_AREA, breaks=500)
percelen <- percelen[,(names(percelen) %in% c("OBJECTID","categorie", "periode"))] #remove columns

sel1 <- percelen[percelen$categorie == "Gemuteerd" & percelen$periode != "2009_2015",]
sel2 <- percelen[percelen$categorie == "Nieuw ingetekende percelen",]

sel3 <- percelen[percelen$categorie == "Nieuw ingetekend vlak - voorheen gemuteerd",]
sel3 <- sel3[1:400,]
  
sel3 <- percelen[sample(nrow(percelen[percelen$categorie == "Nooit gemuteerd geweest",]),400),] # sample 400 percelen van nooit gemuteerd geweest
sel4 <- percelen[sample(nrow(percelen[percelen$categorie == "Nieuw ingetekend vlak - voorheen gemuteerd",]),400),] # sample 400 percelen van nooit gemuteerd geweest

test <- as.data.frame(sel3)
hip <- as.data.frame(table(sel_percelen_df$OBJECTID))
hip

#sel_percelen <- rbind(sel1, sel2, sel3, sel4)
sel_percelen <- rbind(sel1, sel2, sel3, sel4)

test <- as.data.frame(percelen_raw)
test2 <- unique(as.data.frame(table(test$OBJECTID))[,2])

#sel_percelen <- sel_percelen[sample(nrow(sel_percelen),5),]
#sel_percelen <- percelen[percelen$categorie == 'Nooit gemuteerd geweest',]
#sel_percelen <- sel_percelen[1:155,]
#sel_percelen <- sel_percelen[1,] ## CHECK CHECK CHECK

sel_percelen_df <- as.data.frame(sel_percelen)

###################################
######## Zonal statistics #########
###################################

i <- 0
result <- matrix(, nrow = 0, ncol = 13)
for(IMAGE in 1:length(images)){ # calculate for every raster
  img <- stack(images[IMAGE])  # pick one raster
  checkmax <- mean(maxValue(img)) # if 0 --> do not make zonal stats
  beeld <- substr(names(img[[1]]), 1, (nchar(names(img[[1]]))-2))
  print(paste(Sys.time(), i, beeld))
  if(checkmax > 0){
    for(PERCEEL in 1:length(sel_percelen)){
      perceel <- sel_percelen[PERCEEL,]
      objectid <- perceel@data$OBJECTID
      
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
        
        newrow <- c(objectid, beeld, BAND, npixels, b_mean, b_sd, b_median, b_max, b_min, b_q05, b_q10, b_q90, b_q95)
        result <- rbind(result, newrow)
      }
    }
  }
}

result_df <- as.data.frame(result, row.names=F)
names(result_df) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
write.csv(result_df, "C:/BASTIAEN/SBIR/data/Statistics/FINAL_stats_result_selectie_percelen_alle_beelden_PART.csv", row.names=F)

test <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/FINAL_stats_result_selectie_percelen_alle_beelden_PART1.csv")
test2 <- rbind(test, result_df)
write.csv(test2, "C:/BASTIAEN/SBIR/data/Statistics/FINAL_stats_result_selectie_percelen_alle_beelden_ALL.csv", row.names=F)




