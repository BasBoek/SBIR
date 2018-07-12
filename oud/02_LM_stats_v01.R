# Author: Bastiaen Boekelo 
# Date
# Goal: Creating linear models per parcel

library(Publish) 
library(calibrate)
library(bfast)

rm(list=ls())

############################################
##############  READING DATA ###############
############################################

data_LS <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/landsat/FINAL_stats_result_selectie_percelen_alle_beelden_ALL.csv")
data_RE <- read.csv("C:/BASTIAEN/SBIR/data/Statistics/rapideye/FINAL_stats_result_RapidEye_ALL.csv")
data <- rbind(data_LS, data_RE)
data <- na.omit(data)
perceelnamen <- unique(data$objectid)

data$year <- as.numeric(substr(data$beeld, 11,14))
data$day <- as.numeric(substr(data$beeld, 15,17))/366
data$date <- data$year + data$day
data$difmax <- (data$max-data$mean)/data$sd
data$difmin <- (data$mean-data$min)/data$sd
data$max_90 <- (data$max - data$q90)/data$mean*100
data$min_10 <- (data$q10 - data$min)/data$mean*100
data$mut_high <- data$difmax * sqrt(data$max_90)
data$mut_low <- data$difmin * sqrt(data$min_10)

par(mfrow=c(2,3))

plot(data$date, data$difmax)
text(data$date, data$difmax, labels = round(data$day*366))

plot(data$date, data$max_90)
text(data$date, data$max_90, labels = round(data$day*366))

plot(data$date, data$mut_high)
text(data$date, data$mut_high, labels = round(data$day*366))

plot(data$date, data$difmin)
text(data$date, data$difmin, labels = round(data$day*366))

plot(data$date, data$min_10)
text(data$date, data$min_10, labels = round(data$day*366))

plot(data$date, data$mut_low)
text(data$date, data$mut_low, labels = round(data$day*366))

################################################################################
# Determine if value is exceptional (=1)
################################################################################

# Function to use when calculating everyting

nr_mutations <- function(mut_high, mut_low, sd_val){ # SET SD
  h_list <- c()
  l_list <- c()
  for(i in 1:length(mut_high)){
    h <- 0
    l <- 0
    val_high <- mut_high[i] - (mean(mut_high, na.rm=T) + sd_val*sd(mut_high, na.rm=T)) 
    val_low  <-  mut_low[i] - (mean(mut_low, na.rm=T) +  sd_val*sd(mut_low, na.rm=T))  
    
    if(val_high > 0){
      h <- 1
    }
    
    if(val_low > 0){
      l <- 1
    }
    
    h_list[i] <- h
    l_list[i] <- l
  }
  h_sum <- sum(h_list)
  l_sum <- sum(l_list)
  return(c(h_sum, l_sum))
}

test <- nr_mutations(data$mut_high, data$mut_low, 1.5)


# SELECTION DATA

for(PERCEEL in perceelnamen){
  for(BAND in 1:3){
    #agg <- aggregate(data, by=list(objectid,band), FUN=sum, na.rm=TRUE)
    hip <- nr_mutations(data$mut_high[data$objectid == PERCEEL & data$band == BAND], data$mut_low[data$objectid == PERCEEL & data$band == BAND], 2)
    print(paste(PERCEEL, BAND,hip))
  }
}

data <- data[data$band ==1,]
data <- data[data$objectid == perceelnamen[3],]

plot(data$date, data$difmin)
lm <- lm(difmax~date, data=data)

#pred <- predict(lm, newdata=data, interval="prediction", level=0.90)
#data <- cbind(data, pred)



mean(data$mut_low)
mean(data$mut_low) + 1.5* sd(data$mut_low)


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


