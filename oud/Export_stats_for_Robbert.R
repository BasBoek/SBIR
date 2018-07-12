
files <- list.files(path = "C:/Data/SBIR/data/Statistics/all_sats/02_outliers/", pattern = ".csv", full.names=T)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )

data <- data[data$outlier == 1,]

data$year <- substr(data$date, 1,4)
data$day <- (as.numeric(data$date) - as.numeric(data$year))*366
data$yearday <- round(as.numeric(paste(data$year, data$day, sep="")))
head(data)


head(data)

write.csv(data, "C:/Data/SBIR/data/Statistics/all_sats/02_outliers_all.csv", row.names=F)
