# Bastiaen Boekelo
# Date: June 2018
# Goal: Mutation or not (0 or 1)

nr_mutations <- function(input, date, sd_val){ # SET SD
  #result <- matrix(, nrow = 0, ncol = 2050)
  outlier <- c()
  for(i in 1:length(input)){
    h <- 0 
    val <- input[i] - (mean(input, na.rm=T) + sd_val*sd(input, na.rm=T)) 
    if(is.na(val)==F){
      if(val > 0){
        h <- 1 # Alleen als het punt boven de [mean + factor*sd] uitkomt --> 0 wordt 1 
      }
      outlier[i] <- h # 0 of 1 wordt toegevoegd aan de reeks
    }
  }
  
  muts <- cbind(as.data.frame(outlier), date) # de datum en de 'outlier of niet' kolom worden gemerged
  return(muts)
}




