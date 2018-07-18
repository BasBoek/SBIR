# Author: Bastiaen Boekelo 
# Date: June 2018
# Goal: Creating linear models per parcel

rm(list=ls())
library(Publish) 
library(calibrate)
#library(bfast)
library(data.table)  
source("R/03a_mutation_determinator.R") # Laad een functie die bepaald of iets een outlier is of niet.

DIR_outliers         <- "C:/Data/SBIR/data/Statistics/all_sats/02_outliers/"
DIR_yearstats        <- "C:/Data/SBIR/data/Statistics/all_sats/03_yearstats/01_separate/"
DIR_yearstats_1_file <- "C:/Data/SBIR/data/Statistics/all_sats/03_yearstats/02_all/"

############################################
##############  READING DATA ###############
############################################

files <- list.files(path = "C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/01_chunks",pattern = "zonal_stats", recursive=F, full.names=T)
temp  <- lapply(files, fread, sep=",")
data1  <- rbindlist( temp )
data2  <- data1[!duplicated(data1)] # if during parallel processing same data is analysed
data  <- na.omit(data2) # mogelijk door zelfde pixel
names(data) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
## writing data to 1 file
write.csv(data,"C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/02_all/perceelstats_all.csv", row.names=F)

rm(temp, files, data1, data2)

                ############################################
                ##############  Creating variables #########
                ############################################

data$year     <- as.numeric(substr(data$beeld, 11,14))
data$day      <- as.numeric(substr(data$beeld, 15,17))/366 # convert Julian day to year
data$date     <- data$year + data$day # newdate
data$difmax   <- (data$max-data$mean)/data$sd
data$difmin   <- (data$mean-data$min)/data$sd
data$max_90   <- (data$max - data$q90)/data$mean*100
data$min_10   <- (data$q10 - data$min)/data$mean*100
data$mut_high <- data$difmax * sqrt(data$max_90)
data$mut_low  <- data$difmin * sqrt(data$min_10)
data$cov      <- data$sd / data$mean

                ############################################
                ##############  Inspecting dataset #########
                ############################################ [OPTIONAL & DELETABLE]

hist(data$cov[data$cov < 0.2], na.rm=T, breaks=300)
hist(data$cov[data$cov > 3 & data$cov < 80], na.rm=T, breaks=300)
quantile(data$cov, seq(0,1,0.001), na.rm=T)

hist(data$mut_low[data$mut_low < 25], na.rm=T, breaks=300)
quantile(data$difmax, seq(0,1,0.05), na.rm=T)


############################################
##############  mean cov en difmax #########
############################################

agg_difmax <- aggregate(data$difmax, by=list(data$objectid, data$year), FUN = mean, na.rm=T)
names(agg_difmax) <- c("perceel", "jaar", "mean_difmax")

agg_cov    <- as.data.frame(aggregate(data$cov, by=list(data$year, data$objectid), FUN = mean, na.rm=T)$x)
names(agg_cov) <- c("mean_cov")
agg_thresholds <- cbind(agg_difmax, agg_cov)
write.csv(agg_thresholds, "C:/Data/SBIR/data/Statistics/all_sats/03_yearstats/03_mean_cov_difmax/COV_and_difmax.csv", row.names=F)
plot(agg_thresholds$mean_cov, agg_thresholds$mean_difmax)

#########################################
##############  Mutatie of niet #########
#########################################

## Voor testen
PERCEEL <- 1317071
SD <- 2 
BAND <- 1 

## Input voor de for loop
perceelnamen <- unique(data$objectid)
sds <- c(1.5, 2.0, 2.5, 3.0) # Na eerste inspectie data besloten het aantal SD's tussen 1.5 en 3.0 te houden
i <- 0 
for(PERCEEL in perceelnamen){
  for(BAND in 1:3){
    for(SD in sds){
      i <- i + 1
      date            <-  data$date[data$objectid     == PERCEEL & data$band == BAND]
      input_mut_high  <-  data$mut_high[data$objectid == PERCEEL & data$band == BAND]
      input_mut_low   <-  data$mut_low[data$objectid  == PERCEEL & data$band == BAND]
      input_difmax    <-  data$difmax[data$objectid   == PERCEEL & data$band == BAND]
      input_difmin    <-  data$difmin[data$objectid   == PERCEEL & data$band == BAND]
      input_max90     <-  data$max_90[data$objectid   == PERCEEL & data$band == BAND]
      input_min10     <-  data$min_10[data$objectid   == PERCEEL & data$band == BAND]

      muts1 <- nr_mutations(input_mut_high, date, SD)
      muts1$value <- "mut_high"
      
      muts2 <- nr_mutations(input_mut_low,  date, SD)
      muts2$value <- "mut_low"
      
      muts3 <- nr_mutations(input_difmax,   date, SD)
      muts3$value <- "difmax"
      
      muts4 <- nr_mutations(input_difmin,   date, SD)
      muts4$value <- "difmin"
      
      muts5 <- nr_mutations(input_max90,    date, SD)
      muts5$value <- "max90"
      
      muts6 <- nr_mutations(input_min10,    date, SD)
      muts6$value <- "min10"
      
      muts <- rbind(muts1, muts2, muts3, muts4, muts5, muts6)
      #muts$mean_cov <- mean_cov
      #muts$mean_difmax <- mean_difmax
      muts$perceel <- PERCEEL
      muts$band <- BAND
      muts$sd <- SD
      muts$nr_beelden <- nrow(muts1)
      if(i == 1){
        muts_all <- muts # create variable muts_all if first run
      } else {
        muts_all <- rbind(muts_all, muts) # append rows to muts_all
      }
    }
  }
  write.csv(muts_all, paste(DIR_outliers, "outliers_", PERCEEL, ".csv", sep = ""), row.names=F)
  muts_all <- muts_all[0,]
  print(i/(length(sds)*3))
}

rm(muts, muts_all,muts1, muts2, muts3, muts4, muts5, muts6, result, input_difmax, input, input_difmin, input_max90, input_min10, input_mut_high, input_mut_low, i, hip, h_list, h)

                #########################################
                ##############  Alles in 1 file #########
                #########################################

files <- list.files(path = DIR_outliers ,pattern = "outliers", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_all <- rbindlist( temp )
muts_all$year <- substr( muts_all$date, 1,4)

                #######################################################
                ############  Aggregeer naar info per perceel #########
                #######################################################

## Voor testen
YEAR <- 2015
BAND <- 1
VARIABLE <- "difmax"

variables <- unique(muts_all$value)
years <- c(2015, 2016, 2017) # Welk jaar / Welke jaren van de sat.beelden moeten er meegenomen worden?
for(VARIABLE in variables){
  muts_sell <- muts_all[muts_all$value == VARIABLE,]
  for(YEAR in years){
    for(BAND in 1:3){
      
      ## Get number of mutations in a certain year for a certain band for all the thresholds
      muts_sel15 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 1.5,]
      muts_sel20 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 2,]
      muts_sel25 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 2.5,]
      muts_sel30 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 3,]
      agg15 <- aggregate(muts_sel15$outlier, by=list(muts_sel15$perceel, muts_sel15$sd), FUN = sum, na.rm=T)$x
      agg20 <- aggregate(muts_sel20$outlier, by=list(muts_sel20$perceel, muts_sel20$sd), FUN = sum, na.rm=T)$x
      agg25 <- aggregate(muts_sel25$outlier, by=list(muts_sel25$perceel, muts_sel25$sd), FUN = sum, na.rm=T)$x
      agg30 <- aggregate(muts_sel30$outlier, by=list(muts_sel30$perceel, muts_sel30$sd), FUN = sum, na.rm=T)$x
      
      mean_cov_year    <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 1.5,]
      mean_difmax_year <- 

      stats <- as.data.frame(cbind(agg15,agg20,agg25,agg30))
      
      # Extra informatie
      temp <- stats
      temp[temp > 0] <- 1
      cutoff <- rowSums(temp)/2+0.5
      stats$cutoff <- cutoff
      stats$year <- YEAR
      stats$band <- BAND
      stats$variable <- VARIABLE
      
      # nr of beelden
      nr_beelden <- aggregate(muts_sel15$nr_beelden, by=list(muts_sel15$perceel), FUN = mean)$x
      stats$nr_beelden <- nr_beelden
      
      # perceelnaam
      percelen <- aggregate(muts_sel15$outlier, by=list(muts_sel15$perceel), FUN = sum)$Group.1
      stats$perceel <- percelen
      
      write.csv(stats, paste(DIR_yearstats, VARIABLE, "_", YEAR, "_b", BAND, ".csv", sep=""), row.names=F)
      print(VARIABLE)
    }
  }
}

rm(agg15,agg20,agg25,agg30,muts_sel15,muts_sel20,muts_sel25,muts_sel30, muts_sell) # Verwijder tijdelijke variabelen

## Lees alle csv's in, plak ze onder elkaar (= rbindlist) en schrijf als 1 file weg
files <- list.files(path = DIR_yearstats, pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_sum <- rbindlist( temp )
muts_sum$mutlike <- muts_sum$agg20 * muts_sum$cutoff

##
write.csv(muts_sum, paste(DIR_yearstats_1_file, "Yearstats_all.csv", sep=""), row.names=F)





