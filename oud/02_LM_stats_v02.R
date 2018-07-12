# Author: Bastiaen Boekelo 
# Date
# Goal: Creating linear models per parcel

library(Publish) 
library(calibrate)
library(bfast)
library(data.table)  

rm(list=ls())

############################################
##############  READING DATA ###############
############################################

# Inlezen stats data rapideye en landsat
data_LS <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/landsat/FINAL_stats_result_selectie_percelen_alle_beelden_ALL.csv")
data_RE <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/rapideye/FINAL_stats_result_RapidEye_ALL.csv")
data_SE <- read.csv("C:/BASTIAEN/SBIR/data/Sentinel2/stats/all_stats/FINAL_stats_result_Sentinel2_ALL.csv")
data <- rbind(data_LS, data_RE)
data <- na.omit(data)
rm(data_LS, data_RE, data_SE)

data$year <- as.numeric(substr(data$beeld, 11,14))
data$day <- as.numeric(substr(data$beeld, 15,17))/366
data$date <- data$year + data$day
data$difmax <- (data$max-data$mean)/data$sd
data$difmin <- (data$mean-data$min)/data$sd
data$max_90 <- (data$max - data$q90)/data$mean*100
data$min_10 <- (data$q10 - data$min)/data$mean*100
data$mut_high <- data$difmax * sqrt(data$max_90)
data$mut_low <- data$difmin * sqrt(data$min_10)


################################################################################
# Determine if value is exceptional (=1)
################################################################################

# Function to use when calculating everyting

#input <- data$mut_low[data$objectid == 1836325 & data$band == 1]
#date <- data$date[data$objectid == 1836325 & data$band == 1]
#i <- 1
#sd_val <- 2

nr_mutations <- function(input, date, sd_val){ # SET SD
  result <- matrix(, nrow = 0, ncol = 2050)
  outlier <- c()
  for(i in 1:length(input)){
    h <- 0
    val <- input[i] - (mean(input, na.rm=T) + sd_val*sd(input, na.rm=T))
    if(is.na(val)==F){
      if(val > 0){
        h <- 1
      }
      outlier[i] <- h
    }
  }

  muts <- cbind(as.data.frame(outlier), date)
  return(muts)
}

#test <- nr_mutations(input, date, 1.5)
# SELECTION DATA
#result <- matrix(, nrow = 0, ncol = 4)
#print(result)
#test <- as.character(concatenate(result))
#test <- perceelnamen[1666:length(perceelnamen)]

perceelnamen <- unique(data$objectid)
sds <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
i <- 0
for(PERCEEL in perceelnamen){
  for(BAND in 1:3){
    for(SD in sds){
      i <- i + 1
      date <- data$date[data$objectid == PERCEEL & data$band == BAND]
      input_mut_high <- data$mut_high[data$objectid == PERCEEL & data$band == BAND]
      input_mut_low  <- data$mut_low[data$objectid == PERCEEL & data$band == BAND]
      input_difmax   <- data$difmax[data$objectid == PERCEEL & data$band == BAND]
      input_difmin   <- data$difmin[data$objectid == PERCEEL & data$band == BAND]
      input_max90    <- data$max_90[data$objectid == PERCEEL & data$band == BAND]
      input_min10    <- data$min_10[data$objectid == PERCEEL & data$band == BAND]
      
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
      muts$perceel <- PERCEEL
      muts$band <- BAND
      muts$sd <- SD
      muts$nr_beelden <- nrow(muts1)
      if(i == 1){
        muts_all <- muts
      } else {
        muts_all <- rbind(muts_all, muts)
      }
    }
  }
  write.csv(muts_all, paste("C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/00_perceelstats/perceelstats_", PERCEEL, ".csv", sep = ""), row.names=F)
  muts_all <- muts_all[0,]
  print(i/21)
}

rm(muts, muts_all,muts1, muts2, muts3, muts4, muts5, muts6, result, input_difmax, input, input_difmin, input_max90, input_min10, input_mut_high, input_mut_low, i, hip, h_list, h)

# COMBINE ALL WRITTEN DATA IN ONE VARIABLE
files <- list.files(path = "C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/00_perceelstats/",pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_all <- rbindlist( temp )
muts_all$year <- substr( muts_all$date, 1,4)

tail(muts_all)

# MAKE A DATA SELECTION
YEAR <- 2015
BAND <- 1
VARIABLE <- "difmax"

variables <- unique(muts_all$value)
years <- c(2015,2016,2017)
for(VARIABLE in variables){
  muts_sell <- muts_all[muts_all$value == VARIABLE,]
  for(YEAR in years){
    for(BAND in 1:3){
      muts_sel10 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 1,]
      muts_sel15 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 1.5,]
      muts_sel20 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 2,]
      muts_sel25 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 2.5,]
      muts_sel30 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 3,]
      muts_sel35 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 3.5,]
      muts_sel40 <- muts_sell[muts_sell$band == BAND & muts_sell$year == YEAR & muts_sell$sd == 4,]
      
      agg10 <- aggregate(muts_sel10$outlier, by=list(muts_sel10$perceel, muts_sel10$sd), FUN = sum)$x
      agg15 <- aggregate(muts_sel15$outlier, by=list(muts_sel15$perceel, muts_sel15$sd), FUN = sum)$x
      agg20 <- aggregate(muts_sel20$outlier, by=list(muts_sel20$perceel, muts_sel20$sd), FUN = sum)$x
      agg25 <- aggregate(muts_sel25$outlier, by=list(muts_sel25$perceel, muts_sel25$sd), FUN = sum)$x
      agg30 <- aggregate(muts_sel30$outlier, by=list(muts_sel30$perceel, muts_sel30$sd), FUN = sum)$x
      agg35 <- aggregate(muts_sel35$outlier, by=list(muts_sel35$perceel, muts_sel35$sd), FUN = sum)$x
      agg40 <- aggregate(muts_sel40$outlier, by=list(muts_sel40$perceel, muts_sel40$sd), FUN = sum)$x
      
      stats <- as.data.frame(cbind(agg10,agg15,agg20,agg25,agg30,agg35,agg40))
      
      # derivates
      temp <- stats
      temp[temp > 0] <- 1
      cutoff <- rowSums(temp)/2+0.5
      stats$cutoff <- cutoff
      stats$year <- YEAR
      stats$band <- BAND
      stats$variable <- VARIABLE
      
      # nr of beelden
      nr_beelden <- aggregate(muts_sel10$nr_beelden, by=list(muts_sel10$perceel), FUN = mean)$x
      stats$nr_beelden <- nr_beelden
      
      # perceelnaam
      percelen <- aggregate(muts_sel10$outlier, by=list(muts_sel10$perceel), FUN = sum)$Group.1
      stats$perceel <- percelen
      
      write.csv(stats, paste("C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/02_summary_stats_var_year_band/01_stats_per_combi/Stats_", VARIABLE, "_", YEAR, "_b", BAND, ".csv", sep=""), row.names=F)
      
    }
  }
}


rm(agg10,agg15,agg20,agg25,agg30,agg35,agg40,muts_sel10,muts_sel15,muts_sel20,muts_sel25,muts_sel30,muts_sel35,muts_sel40, muts_sell)

files <- list.files(path = "C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/02_summary_stats_var_year_band/01_stats_per_combi",pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_sum <- rbindlist( temp )


muts_sum$mutlike <- muts_sum$agg20 * muts_sum$cutoff

write.csv(muts_sum, "C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/02_summary_stats_var_year_band/02_all_in_1/Mutations_all.csv", row.names=F)



#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
################################################################   THE END   ########################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################

lm <- lm(date~mut_high+difmax, data=data)
pred <- predict(lm, newdata=data, interval="prediction", level=0.90)


plot(data$date, data$difmax)

plot(data$date, data$max)

cor(selectie)
selectie <- data[,16:25]
plot(selectie)

ci.mean(data$difmax, 0.0001)
plot(data$date, data$mut_low)
bfast(data)


par(mfrow=c(2,3))

##################################################################
# plot(data$date, data$difmax)
# text(data$date, data$difmax, labels = round(data$day*366))
# 
# plot(data$date, data$max_90)
# text(data$date, data$max_90, labels = round(data$day*366))
# 
# plot(data$date, data$mut_high)
# text(data$date, data$mut_high, labels = round(data$day*366))
# 
# plot(data$date, data$difmin)
# text(data$date, data$difmin, labels = round(data$day*366))
# 
# plot(data$date, data$min_10)
# text(data$date, data$min_10, labels = round(data$day*366))
# 
# plot(data$date, data$mut_low)
# text(data$date, data$mut_low, labels = round(data$day*366))
##################################################################




# SELECTION DATA
data <- data[data$band ==1,]
data <- data[data$objectid == perceelnamen[3],]

plot(data$date, data$difmin)
lm <- lm(difmax~date, data=data)

#pred <- predict(lm, newdata=data, interval="prediction", level=0.90)
#data <- cbind(data, pred)












stats <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/FINAL_stats_result_selectie_percelen_alle_beelden_ALL.csv")

objectids <- unique(stats$objectid)

write.csv(objectids, "C:/BASTIAEN/SBIR/data/Statistics/gebruikte_percelen.csv", row.names=F)


