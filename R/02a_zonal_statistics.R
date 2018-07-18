# Author: Bastiaen Boekelo
# Date: June 2018
# Goal: write function that calculates zonal statistics 

#images[1]
#img <- stack("C:/Data/SBIR/data/Landsat_Sentinel_RapidEye/re1x_l3ax_2017241_1115_0000000_tct.img")
#plot(img)
IMAGE <- 1
PERCEEL <- 1
BAND <- 1

#plot(img_crop)
#plot(img_mask)
#plot(perceel)
#input_perceeldata <- sel_percelen

ZONAL_STATS <- function(images, input_perceeldata, STATS_dir, filebasename, minimum_nr_of_pixels){

  i <- 0
  result <- matrix(, nrow = 0, ncol = 13)
  sel_percelen <- input_perceeldata
  
  for(IMAGE in 1:length(images)){ # calculate for every raster
    img <- stack(images[IMAGE])  # pick one raster
    resolution <- min(res(img))
    checkmax <- mean(maxValue(img)) # if 0 --> do not make zonal stats
    beeld <- substr(names(img[[1]]), 1, (nchar(names(img[[1]]))-2))
    if(checkmax > 0){
      for(PERCEEL in 1:length(sel_percelen)){
        perceel <- sel_percelen[PERCEEL,] 
        objectid <- as.numeric(as.character(perceel@data$OBJECTID)) # naam perceel
        
        ## Testing variables for overlap or pixels and size
        maxpixels <- area(perceel) / resolution^2 # Schat het aantal pixels die binnen een perceel kunnen liggen.
        nonoverlap <- is.null(intersect(perceel, extent(img))) # Overlap?
        if(maxpixels > minimum_nr_of_pixels & nonoverlap == FALSE){ 
          img_crop  <- crop(img, perceel)
          img_mask  <- mask(img_crop, perceel)
          for(BAND in 1:3){
            pixels <- as.matrix(img_mask[[BAND]])
            i <- i + 1
            b_max    <- max(pixels, na.rm=T)
            b_min    <- min(pixels, na.rm=T)
            b_q05    <- as.numeric(quantile(pixels, 0.05, na.rm=T))
            b_q10    <- as.numeric(quantile(pixels, 0.10, na.rm=T))
            b_q90    <- as.numeric(quantile(pixels, 0.90, na.rm=T))
            b_q95    <- as.numeric(quantile(pixels, 0.95, na.rm=T))
            b_mean   <- mean(pixels, na.rm=T)
            b_median <- median(pixels, na.rm=T)
            b_sd     <- sd(pixels, na.rm=T)
            npixels  <- length(pixels) - sum(is.na(pixels))
            newrow   <- c(objectid, beeld, BAND, npixels, b_mean, b_sd, b_median, b_max, b_min, b_q05, b_q10, b_q90, b_q95)#, area)
            result   <- rbind(result, newrow)
          }
        }
      }
      print(paste(Sys.time(), i/3, beeld, " ", objectid))
    }
    write.csv(result,paste(STATS_dir, filebasename, run, ".csv", sep=""), row.names=F)
  }
  assign("result", result, envir = .GlobalEnv) 
  return(result)
}

