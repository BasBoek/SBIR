# Bastiaen Boekelo
# Date: June 2018
# Goal: Creating library for functions

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




