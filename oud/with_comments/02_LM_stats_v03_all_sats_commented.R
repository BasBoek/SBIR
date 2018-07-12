# Author: Bastiaen Boekelo 
# Date
# Goal: Creating linear models per parcel

rm(list=ls())
library(Publish) 
library(calibrate)
#library(bfast)
library(data.table)  

# Note
# In functions staat de volgende formule:

#nr_mutations <- function(input, date, sd_val){ # SET SD
# result <- matrix(, nrow = 0, ncol = 2050)
# outlier <- c()
# for(i in 1:length(input)){
#   h <- 0
#   val <- input[i] - (mean(input, na.rm=T) + sd_val*sd(input, na.rm=T))
#   if(is.na(val)==F){
#     if(val > 0){
#       h <- 1
#     }
#     outlier[i] <- h
#   }
# }
# 
# muts <- cbind(as.data.frame(outlier), date)
# return(muts)
# }

# Deze bepaalt of de waarde gezien moet worden als een outlier of niet. Dit wordt bepaald met verschillende SD's voor elke variabele.
# Op deze manier kunnen we later het model en het algoritme nog afstellen
# Als alternatief van deze functie kan er ook worden gekeken of er 'breaks' in voorkomen met een model zoals BFAST.
# Dit willen we waarschijnlijk ook nog wel bekijken en eventueel noemen. 


source("C:/Data/SBIR/modellen/R_functions/functions.R")

DIR_outliers  <- "C:/Data/SBIR/data/Statistics/all_sats/02_outliers/"
DIR_yearstats <- "C:/Data/SBIR/data/Statistics/all_sats/03_yearstats/"
DIR_yearstats_1_file <- "C:/Data/SBIR/data/Statistics/all_sats/03_yearstats_1_file/"

############################################
##############  READING DATA ###############
############################################

# Inlezen stats data rapideye en landsat

files <- list.files(path = "C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/",pattern = ".csv", recursive=F, full.names=T)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
names(data) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")
data <- data[!duplicated(data)] # Uitzoeken waar duplicates vandaan komen
data <- na.omit(data)

write.csv(data,"C:/Data/SBIR/data/Statistics/all_sats/01_perceelstats/perceelstats_all.csv", row.names=F)

rm(temp, files)

###### Adding extra info ######

# Note
# Hier worden de variabelen berekend. De betekenis van de variabelen in het kort:
# difmax zegt iets over de mate van uitschietendheid van de hoogste pixelwaarde. Bijvoorbeeld een trekker of een schuur zal daardoor vermoedelijk een stuk hoger uitvallen dan een koe.
# difmin is tegengestelde van difmax --> zegt iets over de mate van laagste pixelwaarde (bijvoorbeeld een donkere poel middenin een perceel)
# max_90 zal snel hoog zijn als de uitschietende pixelwaardes een klein gedeelte van het perceel beslaan (aanname dat dat waarschijnlijker een muatie is)
# min_10 is tegengestelde max_90.
# mut_high combineert de waardes van difmax en max_90, dus over de mate van hogere reflectie en relatieve grootte van de mutatie t.o.v. perceel
# mut_low --> idem, maar voor lage waardes


data$year <- as.numeric(substr(data$beeld, 11,14))
data$day <- as.numeric(substr(data$beeld, 15,17))/366
data$date <- data$year + data$day
data$difmax <- (data$max-data$mean)/data$sd
data$difmin <- (data$mean-data$min)/data$sd
data$max_90 <- (data$max - data$q90)/data$mean*100
data$min_10 <- (data$q10 - data$min)/data$mean*100
data$mut_high <- data$difmax * sqrt(data$max_90)
data$mut_low <- data$difmin * sqrt(data$min_10)

#data <- data[selectie,]

################################################################################
# Determine if value is exceptional (=1)
################################################################################
plot(date, input_mut_high)
perceelnamen <- unique(data$objectid)
sds <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
i <- 0
PERCEEL <- perceelnamen[7]
BAND <- 1
SD <- 1.5
for(PERCEEL in perceelnamen){
  for(BAND in 1:3){
    for(SD in sds){
      i <- i + 1
      date <- data$date[data$objectid == PERCEEL & data$band == BAND]
      input_mut_high <- data$mut_high[data$objectid == PERCEEL & data$band == BAND]
      input_mut_low  <-  data$mut_low[data$objectid == PERCEEL & data$band == BAND]
      input_difmax   <-   data$difmax[data$objectid == PERCEEL & data$band == BAND]
      input_difmin   <-   data$difmin[data$objectid == PERCEEL & data$band == BAND]
      input_max90    <-   data$max_90[data$objectid == PERCEEL & data$band == BAND]
      input_min10    <-   data$min_10[data$objectid == PERCEEL & data$band == BAND]
      
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
  write.csv(muts_all, paste(DIR_outliers, "perceelstats_", PERCEEL, ".csv", sep = ""), row.names=F)
  muts_all <- muts_all[0,]
  print(i/21)
}

rm(muts, muts_all,muts1, muts2, muts3, muts4, muts5, muts6, result, input_difmax, input, input_difmin, input_max90, input_min10, input_mut_high, input_mut_low, i, hip, h_list, h)

# COMBINE ALL WRITTEN DATA IN ONE VARIABLE
files <- list.files(path = DIR_outliers ,pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_all <- rbindlist( temp )
muts_all$year <- substr( muts_all$date, 1,4)

tail(muts_all)

# MAKE A DATA SELECTION
YEAR <- 2015
BAND <- 1
VARIABLE <- "difmax"

# NOTE
# Hier wordt een aggregatie gemaakt hoeveel mutaties er per variabele gevonden zijn per perceel (per band). 
# Ook op basis van de verschillende standaarddeviaties . 
# 


variables <- unique(muts_all$value)
years <- c(2016)
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
      
      agg10 <- aggregate(muts_sel10$outlier, by=list(muts_sel10$perceel, muts_sel10$sd), FUN = sum, na.rm=T)$x
      agg15 <- aggregate(muts_sel15$outlier, by=list(muts_sel15$perceel, muts_sel15$sd), FUN = sum, na.rm=T)$x
      agg20 <- aggregate(muts_sel20$outlier, by=list(muts_sel20$perceel, muts_sel20$sd), FUN = sum, na.rm=T)$x
      agg25 <- aggregate(muts_sel25$outlier, by=list(muts_sel25$perceel, muts_sel25$sd), FUN = sum, na.rm=T)$x
      agg30 <- aggregate(muts_sel30$outlier, by=list(muts_sel30$perceel, muts_sel30$sd), FUN = sum, na.rm=T)$x
      agg35 <- aggregate(muts_sel35$outlier, by=list(muts_sel35$perceel, muts_sel35$sd), FUN = sum, na.rm=T)$x
      agg40 <- aggregate(muts_sel40$outlier, by=list(muts_sel40$perceel, muts_sel40$sd), FUN = sum, na.rm=T)$x
      
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
      
      write.csv(stats, paste(DIR_yearstats, VARIABLE, "_", YEAR, "_b", BAND, ".csv", sep=""), row.names=F)
      
    }
  }
}

rm(agg10,agg15,agg20,agg25,agg30,agg35,agg40,muts_sel10,muts_sel15,muts_sel20,muts_sel25,muts_sel30,muts_sel35,muts_sel40, muts_sell)

files <- list.files(path = DIR_yearstats, pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
muts_sum <- rbindlist( temp )
muts_sum$mutlike <- muts_sum$agg20 * muts_sum$cutoff

write.csv(muts_sum, paste(DIR_yearstats_1_file, "Yearstats_all.csv", sep=""), row.names=F)





