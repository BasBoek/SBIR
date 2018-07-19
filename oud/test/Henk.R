library(raster)

rm(list=ls())

henk <- 4

lijst <- c(1,5,238746,3)

timed <- 0

## Hoi, dit is een functie voor de cursus. Mooi. Sad...
tamdamdam <- function(inputlijst, factor){
  for(i in inputlijst){
    timed <- i*factor  # tamdamdam
    #assign("timed", timed, envir = .GlobalEnv) 
  }
  return(timed)
}

lijst[3]

tamdamdam(lijst,55)
