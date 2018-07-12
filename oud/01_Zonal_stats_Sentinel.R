# Author: Bastiaen Boekelo 
# Goal: Extract zonal statistics from the datasets

# LIBRARIES
rm(list=ls())
library(raster)
library(maptools)
library(rgdal)
library(rgeos)


# FILE AND FOLDER LOCATIONS
LS_dir <- "C:/BASTIAEN/SBIR/data/Sentinel2/"


# Loading shapefile & make selection (2015-2016)

#####################################
######## Loading raster data ########
#####################################

images <- list.files(path = LS_dir, pattern = "tct.img$", full.names = T)

#####################################
###### Loading shapefile data #######
#####################################

# inlezen selectie objectid's van shap
selectie <- unique(read.csv("C:/BASTIAEN/SBIR/data/Landsat8/FINAL_stats_result_selectie_percelen_alle_beelden_Landsat.csv")$objectid)

# Inlezen shapefile
percelen_raw <- readShapePoly("C:/BASTIAEN/SBIR/data/Pilotdata_omgevormd/Omgevormde_data_pilotgebied_SBIR_MAAN_fase2.shp")
percelen <- percelen_raw[,(names(percelen_raw) %in% c("OBJECTID","categorie", "periode"))] #remove columns

################################### select the right percelen ######################################
percelen2 <- percelen
percelen2@data$counter <- 1:nrow(percelen2@data)
for(i in 1:length(selectie)){
  object <- selectie[i]
  row <- percelen2@data[percelen@data$OBJECTID==object,]
  rownumber <- as.numeric(row[4])
  if(i == 1){
    poi <- percelen2[rownumber,]
  } else {
    poi2 <- percelen2[rownumber,]
    poi <- rbind(poi, poi2)
  }
  print(i)
}
sel_percelen <- poi
sel_percelen <- sel_percelen[,(names(sel_percelen) %in% c("OBJECTID","categorie", "periode"))]
rm(percelen_raw, percelen2, percelen, poi, poi2, row)

################################### select the right percelen ######################################

sel_percelen <- sel_percelen[1768:1873,]
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
  #sel_percelen_2 <- crop(sel_percelen, extent(img))   # crop percelen based on extent img
  #area_percelen <- as.numeric(area(sel_percelen_2))
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
  write.csv(result,paste("C:/BASTIAEN/SBIR/data/Sentinel2/stats/s4/d/FINAL_stats_result_Sentinel2_", beeld, ".csv", sep=""), row.names=F)
}


result_df <- as.data.frame(result, row.names=F)
names(result_df) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
write.csv(result_df,"D:/rapideye/stats/FINAL_stats_result_RapidEye4.csv", row.names=F)







