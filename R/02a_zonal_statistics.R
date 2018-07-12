# Author: Bastiaen Boekelo
# Date: June 2018
# Goal: write function that calculates zonal statistics 

input_satdata <- "C:/Data/SBIR/data/Landsat_Sentinel_RapidEye/"
input_perceeldata <- sel_percelen
IMAGE <- 1
PERCEEL <- 1

plot(img_crop)
plot(img_mask)
plot(perceel)




ZONAL_STATS <- function(input_satdata, input_perceeldata, STATS_dir, minimum_nr_of_pixels){

  images <- list.files(path = input_satdata, pattern = "tct.img$", full.names = T)
  i <- 0
  result <- matrix(, nrow = 0, ncol = 13)
  sel_percelen <- input_perceeldata
  
  for(IMAGE in 1:length(images)){ # calculate for every raster
    resolution <- min(res(img))
    img <- stack(images[IMAGE])  # pick one raster
    checkmax <- mean(maxValue(img)) # if 0 --> do not make zonal stats
    beeld <- substr(names(img[[1]]), 1, (nchar(names(img[[1]]))-2))
    if(checkmax > 0){
      for(PERCEEL in 1:length(sel_percelen)){
        perceel <- sel_percelen[PERCEEL,]
        objectid <- perceel@data$OBJECTID
        #area <- area_percelen[PERCEEL]
        maxpixels <- area(perceel) / resolution^2
        if(maxpixels > minimum_nr_of_pixels){
          nonoverlap <- is.null(intersect(perceel, extent(img)))
          if(nonoverlap == FALSE){
            img_crop  <- crop(img, perceel)
            img_mask  <- mask(img_crop, perceel)
            plot(img_mask[[1]])
            for(BAND in 1:3){
              pixels <- as.matrix(img_mask[[1]])
              i <- i + 1
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
    }
    write.csv(result,paste(STATS_dir, run, "/perceelstatsblieb.csv", sep=""), row.names=F)
  }
}
  
  
