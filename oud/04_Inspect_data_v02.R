# Author: Bastiaen Boekelo
# Date: June 2018
# Goal: Inspect correlations data with calculated statistics

library(raster)
library(maptools)
library(rgdal)
rm(list=ls())

p_raw <- readShapePoly("C:/Data/SBIR/data/Statistics/all_sats/04_yearstats_shape/Statistieken_percelen.shp")
pdf <- as.data.frame(p_raw)
p <- na.omit(pdf)
rm(pdf)
#p <- read.csv("C:/Data/SBIR/data/Statistics/all_sats/03_yearstats_1_file/Yearstats_all.csv")
#p <- na.omit(p)
##############################################################################
#################################  2016  #####################################
##############################################################################
YEAR <- 2016
BAND <- 3

categories <- unique(p$perceel, p$categorie)

unique(p$variable)
p <- p[order(p$OBJECTID, p$band, p$year ),]


# band 1
a10_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg10
a10_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg10
a10_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg10
a10_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg10
a10_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg10
a10_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg10

a15_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg15
a15_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg15
a15_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg15
a15_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg15
a15_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg15
a15_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg15

a20_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg20
a20_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg20
a20_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg20
a20_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg20
a20_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg20
a20_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg20

a25_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg25
a25_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg25
a25_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg25
a25_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg25
a25_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg25
a25_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg25

a30_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg30
a30_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg30
a30_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg30
a30_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg30
a30_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg30
a30_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg30

a35_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg35
a35_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg35
a35_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg35
a35_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg35
a35_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg35
a35_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg35

a40_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$agg40
a40_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$agg40
a40_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$agg40
a40_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$agg40
a40_max90 <-       p[p$year == YEAR & p$variable == "max90",]$agg40
a40_min10 <-       p[p$year == YEAR & p$variable == "min10",]$agg40

cut_mut_high <- p[p$year == YEAR & p$variable == "mut_high",]$cutoff
cut_mut_low <-   p[p$year == YEAR & p$variable == "mut_low",]$cutoff
cut_difmax <-     p[p$year == YEAR & p$variable == "difmax",]$cutoff
cut_difmin <-     p[p$year == YEAR & p$variable == "difmin",]$cutoff
cut_max90 <-       p[p$year == YEAR & p$variable == "max90",]$cutoff
cut_min10 <-       p[p$year == YEAR & p$variable == "min10",]$cutoff
perceel_ID <-       p[p$year == YEAR & p$variable == "min10",]$OBJECTID
categories <-       as.character(p[p$year == YEAR & p$variable == "min10",]$categorie)


test <- cbind(
      perceel_ID, 
      a10_mut_high,
      a10_mut_low,
      a10_difmax,
      a10_difmin,
      a10_max90,
      a10_min10,
      a15_mut_high,
      a15_mut_low,
      a15_difmax,
      a15_difmin,
      a15_max90,
      a15_min10,
      a20_mut_high,
      a20_mut_low,
      a20_difmax,
      a20_difmin,
      a20_max90,
      a20_min10,
      a25_mut_high,
      a25_mut_low,
      a25_difmax,
      a25_difmin,
      a25_max90,
      a25_min10,
      a30_mut_high,
      a30_mut_low,
      a30_difmax,
      a30_difmin,
      a30_max90,
      a30_min10,
      a35_mut_high,
      a35_mut_low,
      a35_difmax,
      a35_difmin,
      a35_max90,
      a35_min10,
      a40_mut_high,
      a40_mut_low,
      a40_difmax,
      a40_difmin,
      a40_max90,
      a40_min10,
      cut_mut_high,
      cut_mut_low,
      cut_difmax,
      cut_difmin,
      cut_max90,
      cut_min10,
      categories
      )

names(test)

d <- as.data.frame(cbind(p1,p2,p3,p4,p5,p6))
d <- cbind(d,pc,pp)
names(d) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "categorie")

a1 <- aggregate(d$mut_high, by=list(d$categorie), FUN = mean, na.rm=T) # band 1 stats
a1

unique(d$categorie)

cor <- round(cor(d[,1:6])*100)/100
################################################################################
##############################################################################

#### AANNAME: ALS PERCEEL NOOIT OUTLIER HOGER DAN 1 SD HEEFT GEHAD -> ONGEMUTEERD
BAND <- 1
p_sel <- p[p$band == BAND,]

index <- p_sel$agg10 > 0
p_sel$mutatie <- 0
p_sel$mutatie[index] <- 1 
  
p_not_mutated <- p_sel[p_sel$categorie == "Nooit gemuteerd geweest",]
p_mutated2015 <- p_sel[p_sel$periode == "2015_2016" & p_sel$year == 2015,]
p_mutated2016 <- p_sel[p_sel$periode == "2016_2017" & p_sel$year == 2016,]

sum(p_not_mutated$mutatie) # [liefst   0%] 49,1% van 14392 aangemerkt als gemuteerd
sum(p_mutated2015$mutatie) # [liefst 100%] 49,7% van 732 aangemerkt als gemuteerd 
sum(p_mutated2016$mutatie) # [liefst 100%] 11,3% van 1278 aangemerkt als gemuteerd

#### AANNAME: ALS PERCEEL NOOIT OUTLIER HOGER DAN 1 SD HEEFT GEHAD -> ONGEMUTEERD

####################################################################################################
####################################################################################################
####################################################################################################
################################ Per Variabele per band ############################################
####################################################################################################
####################################################################################################
####################################################################################################

## selectie criteria
PERIODE <- "2016_2017"
YEAR <- "2016"
BAND <- 2
VARIABLE <- "mut_high"

p_sel <- p[p$variable ==VARIABLE,]

for(VARIABLE in unique(p$variable)){
  for(BAND in 1:3){
    
    p_sel <- p[p$band == BAND & p$variable ==VARIABLE,]
    
    ## classificatie
    index <- p_sel$agg15 > 0
    p_sel$mutatie <- 0
    p_sel$mutatie[index] <- 1 

    ## selectie 2
    p_not_mutated <- p_sel[p_sel$categorie == "Nooit gemuteerd geweest" & p_sel$year == YEAR,]
    p_mutated <- p_sel[(p_sel$periode == PERIODE & p_sel$year == YEAR),]
    
    TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
    FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
    FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
    TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
    TOT <- c(TP, FP, FN, TN)
    print(paste(YEAR, VARIABLE, BAND, TP, FP, FN, TN))
  }
}

####################################################################################################
####################################################################################################
####################################################################################################
#################################### Per band #######################################################
####################################################################################################
####################################################################################################
####################################################################################################

## selectie criteria
PERIODE <- "2016_2017"
YEAR <- "2016"
BAND <- 1
blieb <- as.character(unique(p$periode))
blieb
jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")

for(MOMENT in 1:(length(jaren)/2)){
  for(AGG in 1:7){
    for(BAND in 1:3){
      p1m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
      p1o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
      p2m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
      p2o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
      p3m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
      p3o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
      p4m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
      p4o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
      p5m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
      p5o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
      p6m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
      p6o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
      
      ppo <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
      ppm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$OBJECTID
      pco <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
      pcm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$categorie
      
      
      dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
      dm <- cbind(dm, ppm, pcm)
      names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
      
      do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
      do <- cbind(do, ppo, pco)
      names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10",  "perceel", "categorie")
      
      for(i in 1:ncol(combi4)){
        
        i1 <- do$mut_high == 0
        i2 <- do$mut_low == 0
        i3 <- do$difmax == 0
        i4 <- do$difmin == 0
        i5 <- do$max90 == 0
        i6 <- do$min10 == 0
        
        muts <- cbind(i1,i2,i3,i4,i5,i6)
        
        combi1 <- combn(1:6, 1)
        combi2 <- combn(1:6, 2)
        combi3 <- combn(1:6, 3)
        combi4 <- combn(1:6, 4)
        combi5 <- combn(1:6, 5)
        #index <-  !(muts[,combi1[1,i]])
        #index <-  !(muts[,combi2[1,i]] * muts[,combi2[2,i]])
        #index <-  !(muts[,combi3[1,i]] * muts[,combi3[2,i]] * muts[,combi3[3,i]])
        index <-  !(muts[,combi4[1,i]] * muts[,combi4[2,i]] * muts[,combi4[3,i]] * muts[,combi4[4,i]])
        #index <-  !(muts[,combi5[1,i]] * muts[,combi5[2,i]] * muts[,combi5[3,i]] * muts[,combi5[4,i]] * muts[,combi5[5,i]])

        do$mutatie <- 0
        do$mutatie[index] <- 1 
        
        ###############################################################################
        
        i1 <- dm$mut_high == 0
        i2 <- dm$mut_low == 0
        i3 <- dm$difmax == 0
        i4 <- dm$difmin == 0
        i5 <- dm$max90 == 0
        i6 <- dm$min10 == 0

        muts <- cbind(i1,i2,i3,i4,i5,i6)

        combi1 <- combn(1:6, 1)
        combi2 <- combn(1:6, 2)
        combi3 <- combn(1:6, 3)
        combi4 <- combn(1:6, 4)
        combi5 <- combn(1:6, 5)
        #index <-  !(muts[,combi1[1,i]])
        #index <-  !(muts[,combi2[1,i]] * muts[,combi2[2,i]])
        #index <-  !(muts[,combi3[1,i]] * muts[,combi3[2,i]] * muts[,combi3[3,i]])
        index <-  !(muts[,combi4[1,i]] * muts[,combi4[2,i]] * muts[,combi4[3,i]] * muts[,combi4[4,i]])
        #index <-  !(muts[,combi5[1,i]] * muts[,combi5[2,i]] * muts[,combi5[3,i]] * muts[,combi5[4,i]] * muts[,combi5[5,i]])
        
        
        dm$mutatie <- 0
        dm$mutatie[index] <- 1 
        
        
        ## selectie 2
        p_not_mutated <- do
        p_mutated <- dm
        
        TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
        FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
        FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
        TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
        TOT <- c(TP, FP, FN, TN)
        print(paste("kolom", AGG, "jaar",  jaren[MOMENT*2-1], "var", "band", BAND, "stats", TP, FP, FN, TN))
        
      }
    }
  }
}











## selectie criteria
PERIODE <- "2016_2017"
YEAR <- "2016"
BAND <- 1
blieb <- as.character(unique(p$periode))
blieb
jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")

for(MOMENT in 1:(length(jaren)/2)){
  for(AGG in 1:8){
    for(BAND in 1:3){
      p1m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
      p1o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
      p2m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
      p2o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
      p3m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
      p3o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
      p4m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
      p4o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
      p5m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
      p5o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
      p6m <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
      p6o <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
      
      ppo <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
      ppm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$OBJECTID
      pco <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
      pcm <- p[p$band == BAND & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$categorie
      
      
      dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
      dm <- cbind(dm, ppm, pcm)
      names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
      
      do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
      do <- cbind(do, ppo, pco)
      names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10",  "perceel", "categorie")
      
      
      i1 <- do$mut_high == 0
      i2 <- do$mut_low == 0
      i3 <- do$difmax == 0
      i4 <- do$difmin == 0
      i5 <- do$max90 == 0
      i6 <- do$min10 == 0
      index <-  !(i1*i2*i3*i4*i5*i6)
      do$mutatie <- 0
      do$mutatie[index] <- 1 
      
      i1 <- dm$mut_high == 0
      i2 <- dm$mut_low == 0
      i3 <- dm$difmax == 0
      i4 <- dm$difmin == 0
      i5 <- dm$max90 == 0
      i6 <- dm$min10 == 0
      index <-  !(i1*i2*i3*i4*i5*i6)
      dm$mutatie <- 0
      dm$mutatie[index] <- 1 
      
      
      ## selectie 2
      p_not_mutated <- do
      p_mutated <- dm
      
      TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
      FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
      FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
      TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
      TOT <- c(TP, FP, FN, TN)
      print(paste("kolom", AGG, "jaar", jaren[MOMENT*2-1], "band", BAND, "stats", TP, FP, FN, TN))
    }
  }
}
####################################################################################################




####################################################################################################
####################################################################################################
##################################    1 waarde    ##################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

## selectie criteria
PERIODE <- "2015_2016"
YEAR <- "2015"

p1m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable == "mut_high",]$agg15
p1o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",]$agg15
p2m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable  == "mut_low",]$agg15
p2o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",]$agg15
p3m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmax",]$agg15
p3o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",]$agg15
p4m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmin",]$agg15
p4o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",]$agg15
p5m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "max90",]$agg15
p5o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",]$agg15
p6m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "min10",]$agg15
p6o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$agg15

ppo <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
ppm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$OBJECTID
pco <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
pcm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$categorie
pbo <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$band
pbm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$band

dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
dm <- cbind(dm, ppm, pcm, pbm)
names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie","band")

do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
do <- cbind(do, ppo, pco, pbo)
names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10",  "perceel", "categorie", "band")

do1 <- aggregate(do$mut_high, by=list(do$perceel), FUN=sum)$x
do2 <- aggregate(do$mut_low, by=list(do$perceel), FUN=sum)$x
do3 <- aggregate(do$difmax, by=list(do$perceel), FUN=sum)$x
do4 <- aggregate(do$difmin, by=list(do$perceel), FUN=sum)$x
do5 <- aggregate(do$max90, by=list(do$perceel), FUN=sum)$x
do6 <- aggregate(do$min10, by=list(do$perceel), FUN=sum)$x
dop <- aggregate(do$min10, by=list(do$perceel), FUN=sum)$Group.1
  
dm1 <- aggregate(dm$mut_high, by=list(dm$perceel), FUN=sum)$x
dm2 <- aggregate(dm$mut_low, by=list(dm$perceel), FUN=sum)$x
dm3 <- aggregate(dm$difmax, by=list(dm$perceel), FUN=sum)$x
dm4 <- aggregate(dm$difmin, by=list(dm$perceel), FUN=sum)$x
dm5 <- aggregate(dm$max90, by=list(dm$perceel), FUN=sum)$x
dm6 <- aggregate(dm$min10, by=list(dm$perceel), FUN=sum)$x
dmp <- aggregate(dm$min10, by=list(dm$perceel), FUN=sum)$Group.1

dm <- as.data.frame(cbind(dm1, dm2, dm3, dm4, dm5, dm6))

do <- as.data.frame(cbind(do1, do2, do3, do4, do5, do6))
names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10")
names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10")

i1 <- do$mut_high == 0
i2 <- do$mut_low == 0
i3 <- do$difmax == 0
i4 <- do$difmin == 0
i5 <- do$max90 == 0
i6 <- do$min10 == 0
index <-  !(i1*i2*i3*i4*i5*i6)
do$mutatie <- 0
do$mutatie[index] <- 1 
index
i1 <- dm$mut_high == 0
i2 <- dm$mut_low == 0
i3 <- dm$difmax == 0
i4 <- dm$difmin == 0
i5 <- dm$max90 == 0
i6 <- dm$min10 == 0
index <-  !(i1*i2*i3*i4*i5*i6)
dm$mutatie <- 0
dm$mutatie[index] <- 1 

rm(p1o,p1m,p2o,p2m,p3o,p3m,p4o,p4m,p5o,p5m,p6o,p6m,ppo,ppm,pco,pcm,pbo,pbm,do1,do2,do3,do4,do5,do6,i1,i2,i3,i4,i5,i6)

## selectie 2
p_not_mutated <- do
p_mutated <- dm

TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
TOT <- c(TP, FP, FN, TN)
print(paste(YEAR, TP, FP, FN, TN))



#sum(p_not_mutated$mutatie)/nrow(p_not_mutated)*100 # [liefst   0%] 89,0% van 14392 aangemerkt als gemuteerd
#sum(p_mutated$mutatie)/nrow(p_mutated)*100 # [liefst hoog] 96,2% van 732 aangemerkt als gemuteerd 
#sum(p_mutated2016$mutatie)/nrow(p_mutated2016)*100 # [liefst hoog] 90,8% van 1278 aangemerkt als gemuteerd
#p_mutated2015 <- p_sel[p_sel$periode == "2015_2016" & p_sel$year == 2015,]
#p_mutated2016 <- p_sel[p_sel$periode == "2016_2017" & p_sel$year == 2016,]


####################################################################################################
####################################################################################################
####################################################################################################
#################################### Met variabelen spelen #########################################
####################################################################################################
####################################################################################################
####################################################################################################

## selectie criteria
PERIODE <- "2015_2016"
YEAR <- "2015"
BAND <- 3

for(BAND in 1:3){
  
  p1m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable == "mut_high",]$agg10
  p1o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",]$agg10
  p2m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable  == "mut_low",]$agg10
  p2o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",]$agg10
  p3m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmax",]$agg10
  p3o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",]$agg10
  p4m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmin",]$agg10
  p4o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",]$agg10
  p5m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "max90",]$agg10
  p5o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",]$agg10
  p6m <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "min10",]$agg10
  p6o <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$agg10
  
  ppo <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
  ppm <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$OBJECTID
  pco <- p[p$band == BAND & p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
  pcm <- p[p$band == BAND & p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$categorie
  
  
  dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
  dm <- cbind(dm, ppm, pcm)
  names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
  
  do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
  do <- cbind(do, ppo, pco)
  names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10",  "perceel", "categorie")
  
  
  i1 <- do$mut_high == 0
  i2 <- do$mut_low == 0
  i3 <- do$difmax == 0
  i4 <- do$difmin == 0
  i5 <- do$max90 == 0
  i6 <- do$min10 == 0
  
  index <-  !(i1*i2*i3*100)
  do$mutatie <- 0
  do$mutatie[index] <- 1 
  
  FP <- nrow(do[do$mutatie == 1,]) # FP
  TN <- nrow(do[do$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
  
  i1 <- dm$mut_high == 0
  i2 <- dm$mut_low == 0
  i3 <- dm$difmax == 0
  i4 <- dm$difmin == 0
  i5 <- dm$max90 == 0
  i6 <- dm$min10 == 0
  index <-  !(i1*i2*i3*100)
  dm$mutatie <- 0
  dm$mutatie[index] <- 1 
  
  TP <- nrow(dm[dm$mutatie == 1,])         # TP
  FN <- nrow(dm[dm$mutatie == 0,])         # FN (0 !!!!)
  
  print(paste(YEAR, BAND, TP, FP, FN, TN))
  
}
plot(do)


####################################################################################################
####################################################################################################
##################################    1 waarde variabele weglaten    ##################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

## selectie criteria
PERIODE <- "2015_2016"
YEAR <- "2015"

p1m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable == "mut_high",]$agg15
p1o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",]$agg15
p2m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable  == "mut_low",]$agg15
p2o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",]$agg15
p3m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmax",]$agg15
p3o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",]$agg15
p4m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable   == "difmin",]$agg15
p4o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",]$agg15
p5m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "max90",]$agg15
p5o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",]$agg15
p6m <- p[p$year == YEAR & p$periode   == PERIODE                   & p$variable    == "min10",]$agg15
p6o <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$agg15

ppo <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
ppm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$OBJECTID
pco <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
pcm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$categorie
pbo <- p[p$year == YEAR & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$band
pbm <- p[p$year == YEAR & p$periode   == PERIODE & p$variable    == "min10",]$band

dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
dm <- cbind(dm, ppm, pcm, pbm)
names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie","band")

do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
do <- cbind(do, ppo, pco, pbo)
names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10",  "perceel", "categorie", "band")

do1 <- aggregate(do$mut_high, by=list(do$perceel), FUN=sum)$x
do2 <- aggregate(do$mut_low, by=list(do$perceel), FUN=sum)$x
do3 <- aggregate(do$difmax, by=list(do$perceel), FUN=sum)$x
do4 <- aggregate(do$difmin, by=list(do$perceel), FUN=sum)$x
do5 <- aggregate(do$max90, by=list(do$perceel), FUN=sum)$x
do6 <- aggregate(do$min10, by=list(do$perceel), FUN=sum)$x

dm1 <- aggregate(dm$mut_high, by=list(dm$perceel), FUN=sum)$x
dm2 <- aggregate(dm$mut_low, by=list(dm$perceel), FUN=sum)$x
dm3 <- aggregate(dm$difmax, by=list(dm$perceel), FUN=sum)$x
dm4 <- aggregate(dm$difmin, by=list(dm$perceel), FUN=sum)$x
dm5 <- aggregate(dm$max90, by=list(dm$perceel), FUN=sum)$x
dm6 <- aggregate(dm$min10, by=list(dm$perceel), FUN=sum)$x

dm <- as.data.frame(cbind(dm1, dm2, dm3, dm4, dm5, dm6))
do <- as.data.frame(cbind(do1, do2, do3, do4, do5, do6))
names(dm) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10")
names(do) <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10")

i1 <- do$mut_high == 0
i2 <- do$mut_low == 0
i3 <- do$difmax == 0
i4 <- do$difmin == 0
i5 <- do$max90 == 0
i6 <- do$min10 == 0
index <-  !(i2*i4*i5*100)
do$mutatie <- 0
do$mutatie[index] <- 1 

i1 <- dm$mut_high == 0
i2 <- dm$mut_low == 0
i3 <- dm$difmax == 0
i4 <- dm$difmin == 0
i5 <- dm$max90 == 0
i6 <- dm$min10 == 0
index <-  !(i2*i4*i5*100)
dm$mutatie <- 0
dm$mutatie[index] <- 1 

rm(p1o,p1m,p2o,p2m,p3o,p3m,p4o,p4m,p5o,p5m,p6o,p6m,ppo,ppm,pco,pcm,pbo,pbm,do1,do2,do3,do4,do5,do6,i1,i2,i3,i4,i5,i6)
plot(do)
## selectie 2
p_not_mutated <- do
p_mutated <- dm

TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
TOT <- c(TP, FP, FN, TN)
print(paste(YEAR, TP, FP, FN, TN))







