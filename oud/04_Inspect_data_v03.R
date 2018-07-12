# Author: Bastiaen Boekelo
# Date: June 2018
# Goal: Inspect correlations data with calculated statistics

library(raster)
library(maptools)
library(rgdal)
library(plyr)
rm(list=ls())

p_raw <- readShapePoly("C:/Data/SBIR/data/Statistics/all_sats/04_yearstats_shape/Statistieken_percelen.shp")
pdf <- as.data.frame(p_raw)
p <- na.omit(pdf)
blob <- pdf[rowSums(is.na(pdf)) > 0,]

rm(pdf)
#p <- read.csv("C:/Data/SBIR/data/Statistics/all_sats/03_yearstats_1_file/Yearstats_all.csv")
#p <- na.omit(p)
##############################################################################
#################################  2016  #####################################
##############################################################################


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

MOMENT <- jaren[2]
BAND <- 3
AGG <- 2
## selectie criteria

jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")
run <- 0
GET_STATS <- function(p, nr_sel_var, jaren){
  run <- 0
  # variable combinations
  nr_var <- nr_sel_var
  combi <- combn(1:6, nr_var)
  
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
        
        varnames <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
        dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
        dm <- cbind(dm, ppm, pcm)
        names(dm) <- varnames
        
        do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
        do <- cbind(do, ppo, pco)
        names(do) <- varnames
        
        for(i in 1:ncol(combi)){
          run <- run + 1

          ################## for unmutated part ####################################
          
          i1 <- do$mut_high == 0
          i2 <- do$mut_low == 0
          i3 <- do$difmax == 0
          i4 <- do$difmin == 0
          i5 <- do$max90 == 0
          i6 <- do$min10 == 0
          
          muts <- cbind(i1,i2,i3,i4,i5,i6)
          
          if(nr_var == 1){
            index <-  !(muts[,combi[1,i]])
          } else if(nr_var == 2) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
          } else if(nr_var == 3) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
          } else if(nr_var == 4) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
          } else if(nr_var == 5) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
          } else if(nr_var == 6) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
          } else { nr_var = NULL}

          do$mutatie <- 0
          do$mutatie[index] <- 1 
          
          ################## for mutated part ####################################
          
          i1 <- dm$mut_high == 0
          i2 <- dm$mut_low == 0
          i3 <- dm$difmax == 0
          i4 <- dm$difmin == 0
          i5 <- dm$max90 == 0
          i6 <- dm$min10 == 0
          
          muts <- cbind(i1,i2,i3,i4,i5,i6)
          
          if(nr_var == 1){
            index <-  !(muts[,combi[1,i]])
          } else if(nr_var == 2) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
          } else if(nr_var == 3) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
          } else if(nr_var == 4) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
          } else if(nr_var == 5) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
          } else if(nr_var == 6) {
            index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
          } else { nr_var = NULL}
          
          
          dm$mutatie <- 0
          dm$mutatie[index] <- 1 
          
          
          ## Make the stats
          p_not_mutated <- do
          p_mutated <- dm
          
          TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
          FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
          FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
          TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
          
          #newline <- paste("AGG", AGG/2+0.5, "jaar",  jaren[MOMENT*2-1], "var", paste((combi[,i]), collapse = '_'), "band", BAND, "stats", TP, FP, FN, TN, sep=", ")
          newline <- c(AGG/2+0.5,jaren[MOMENT*2-1], paste((combi[,i]), collapse= '_'), paste(varnames, collapse= ' '), BAND, TP, FP, FN, TN)
          print(newline)
          if(run == 1){
            allstats <- data.frame( threshold=numeric(), jaar=integer(), variabelen=character(), allvar=character(), band=integer(), TP=integer(),FP=integer(),FN=integer(),TN=integer(),stringsAsFactors=FALSE)

            allstats[run,] <- newline
          } else {
            allstats <- rbind(allstats, newline)
            assign('allstats',allstats,envir=.GlobalEnv)
          }
        }
      }
    }
  }
}



GET_STATS(p, 1, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 1, ".csv", sep=""), row.names=F)
GET_STATS(p, 2, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 2, ".csv", sep=""), row.names=F)
GET_STATS(p, 3, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 3, ".csv", sep=""), row.names=F)
GET_STATS(p, 4, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 4, ".csv", sep=""), row.names=F)
GET_STATS(p, 5, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 5, ".csv", sep=""), row.names=F)
GET_STATS(p, 6, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_", 6, ".csv", sep=""), row.names=F)


####################################################################################################
####################################################################################################
####################################################################################################
################################      Per Variabele     ############################################
####################################################################################################
####################################################################################################
####################################################################################################

MOMENT <- 1
BAND <- 3
AGG <- 2
## selectie criteria

jaren <- c("2015", "2015_2016", "2016",  "2016_2017", "2017", "2017_2018")
run <- 0
GET_STATS_NOBAND <- function(p, nr_sel_var, jaren){
  run <- 0
  # variable combinations
  nr_var <- nr_sel_var
  combi <- combn(1:6, nr_var)
  
  for(MOMENT in 1:(length(jaren)/2)){
    for(AGG in 1:7){
      for(BAND in 1:3){
        p1m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o1 <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        p1m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o2 <- p[p$band == 2 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        p1m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable == "mut_high",][,AGG]
        p1o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable == "mut_high",][,AGG]
        p2m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable  == "mut_low",][,AGG]
        p2o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable  == "mut_low",][,AGG]
        p3m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmax",][,AGG]
        p3o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmax",][,AGG]
        p4m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable   == "difmin",][,AGG]
        p4o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable   == "difmin",][,AGG]
        p5m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "max90",][,AGG]
        p5o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "max90",][,AGG]
        p6m3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",][,AGG]
        p6o3 <- p[p$band == 3 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",][,AGG]
        
        
        if(length(p1m1) == length(p1m2) & length(p1m1) == length(p1m3)){
          p1m <- p1m1 + p1m2 + p1m3
          p1o <- p1o1 + p1o2 + p1o3
          p2m <- p2m1 + p2m2 + p2m3
          p2o <- p2o1 + p2o2 + p2o3
          p3m <- p3m1 + p3m2 + p3m3
          p3o <- p3o1 + p3o2 + p3o3
          p4m <- p4m1 + p4m2 + p4m3
          p4o <- p4o1 + p4o2 + p4o3
          p5m <- p5m1 + p5m2 + p5m3
          p5o <- p5o1 + p5o2 + p5o3
          p6m <- p6m1 + p6m2 + p6m3
          p6o <- p6o1 + p6o2 + p6o3
          
          ppo <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$OBJECTID
          ppm <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$OBJECTID
          pco <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$categorie == "Nooit gemuteerd geweest" & p$variable    == "min10",]$categorie
          pcm <- p[p$band == 1 & p$year == jaren[MOMENT*2-1] & p$periode   == jaren[MOMENT*2]           & p$variable    == "min10",]$categorie
          
          varnames <- c("mut_high", "mut_low", "difmax", "difmin", "max90", "min10", "perceel",  "categorie")
          dm <- as.data.frame(cbind(p1m,p2m,p3m,p4m,p5m,p6m))
          dm <- cbind(dm, ppm, pcm)
          names(dm) <- varnames
          
          do <- as.data.frame(cbind(p1o,p2o,p3o,p4o,p5o,p6o))
          do <- cbind(do, ppo, pco)
          names(do) <- varnames
          
          for(i in 1:ncol(combi)){
            run <- run + 1
            
            ################## for unmutated part ####################################
            
            i1 <- do$mut_high == 0
            i2 <- do$mut_low == 0
            i3 <- do$difmax == 0
            i4 <- do$difmin == 0
            i5 <- do$max90 == 0
            i6 <- do$min10 == 0
            
            muts <- cbind(i1,i2,i3,i4,i5,i6)
            
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else { nr_var = NULL}
            
            do$mutatie <- 0
            do$mutatie[index] <- 1 
            
            ################## for mutated part ####################################
            
            i1 <- dm$mut_high == 0
            i2 <- dm$mut_low == 0
            i3 <- dm$difmax == 0
            i4 <- dm$difmin == 0
            i5 <- dm$max90 == 0
            i6 <- dm$min10 == 0
            
            muts <- cbind(i1,i2,i3,i4,i5,i6)
            
            if(nr_var == 1){
              index <-  !(muts[,combi[1,i]])
            } else if(nr_var == 2) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]])
            } else if(nr_var == 3) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]])
            } else if(nr_var == 4) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]])
            } else if(nr_var == 5) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]])
            } else if(nr_var == 6) {
              index <-  !(muts[,combi[1,i]] * muts[,combi[2,i]] * muts[,combi[3,i]] * muts[,combi[4,i]] * muts[,combi[5,i]] * muts[,combi[6,i]])
            } else { nr_var = NULL}
            
            
            dm$mutatie <- 0
            dm$mutatie[index] <- 1 
            
            
            ## Make the stats
            p_not_mutated <- do
            p_mutated <- dm
            
            TP <- nrow(p_mutated[p_mutated$mutatie == 1,])         # TP
            FP <- nrow(p_not_mutated[p_not_mutated$mutatie == 1,]) # FP
            FN <- nrow(p_mutated[p_mutated$mutatie == 0,])         # FN (0 !!!!)
            TN <- nrow(p_not_mutated[p_not_mutated$mutatie == 0,]) # TN (Mits FN=0 --> zo hoog mogelijk!)
            
            #newline <- paste("AGG", AGG/2+0.5, "jaar",  jaren[MOMENT*2-1], "var", paste((combi[,i]), collapse = '_'), "band", BAND, "stats", TP, FP, FN, TN, sep=", ")
            newline <- c(AGG/2+0.5,jaren[MOMENT*2-1], paste((combi[,i]), collapse= '_'), paste(varnames, collapse= ' '), BAND, TP, FP, FN, TN)
            print(newline)
            if(run == 1){
              allstats <- data.frame( threshold=numeric(), jaar=integer(), variabelen=character(), allvar=character(), band=integer(), TP=integer(),FP=integer(),FN=integer(),TN=integer(),stringsAsFactors=FALSE)
              
              allstats[run,] <- newline
            } else {
              allstats <- rbind(allstats, newline)
              assign('allstats',allstats,envir=.GlobalEnv)
            }
          }
        }
      }
    }
  }
}



GET_STATS_NOBAND(p, 1, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 1, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 2, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 2, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 3, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 3, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 4, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 4, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 5, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 5, ".csv", sep=""), row.names=F)
GET_STATS_NOBAND(p, 6, jaren)
write.csv(allstats, paste("C:/Data/SBIR/data/Statistics/all_sats/05_variables/varstats_bandsamen_", 6, ".csv", sep=""), row.names=F)








