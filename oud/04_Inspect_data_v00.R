# Author: Bastiaen Boekelo
# Date: June 2018
# Goal: Inspect correlations data with calculated statistics

library(raster)
library(maptools)
library(rgdal)
rm(list=ls())

p <- readShapePoly("C:/BASTIAEN/SBIR/data/Statistics/mutations_all_images/02_summary_stats_var_year_band/03_stats_in_shape/Statistieken_percelen.shp")
p <- as.data.frame(p)

##############################################################################
#################################  2016  #####################################
##############################################################################
YEAR <- 2016

unique(p$categorie)

var <- "difmax"
p1 <- p[p$band == 1 & p$year == YEAR & p$variable == var,]
p2 <- p[p$band == 2 & p$year == YEAR & p$variable == var,]
p3 <- p[p$band == 3 & p$year == YEAR & p$variable == var,]
aggregate(p1$mutlike, by=list(p1$categorie), FUN = mean, na.rm=T) # band 1 stats
aggregate(p2$mutlike, by=list(p2$categorie), FUN = mean, na.rm=T) # band 2 stats
aggregate(p3$mutlike, by=list(p3$categorie), FUN = mean, na.rm=T) # band 3 stats



p30 <- p[p$band == 1 & p$year == 2016,]
p35 <- p[p$band == 1 & p$year == 2016,]
p40 <- p[p$band == 1 & p$year == 2016,]

agg10 <- aggregate(p10$outlier, by=list(p10$perceel, p10$sd), FUN = sum)$x
agg15 <- aggregate(p15$outlier, by=list(p15$perceel, p15$sd), FUN = sum)$x
agg20 <- aggregate(p20$outlier, by=list(p20$perceel, p20$sd), FUN = sum)$x
agg25 <- aggregate(p25$outlier, by=list(p25$perceel, p25$sd), FUN = sum)$x
agg30 <- aggregate(p30$outlier, by=list(p30$perceel, p30$sd), FUN = sum)$x
agg35 <- aggregate(p35$outlier, by=list(p35$perceel, p35$sd), FUN = sum)$x
agg40 <- aggregate(p40$outlier, by=list(p40$perceel, p40$sd), FUN = sum)$x

unique(p$categorie)

test <- 

p_high <- p[p$]

b1_2015 <- percelen[percelen$year == 2015]
nrow(b1_2015@data)



