
files <- list.files(path = "C:/BASTIAEN/SBIR/data/Sentinel2/stats/",pattern = ".csv", recursive=T, full.names=T)
temp <- lapply(files, fread, sep=",")
muts_all <- rbindlist( temp )
names(muts_all) <- c("objectid", "beeld", "band", "npixels", "mean", "sd", "median", "max", "min", "q05", "q10", "q90", "q95")


write.csv(muts_all,"C:/BASTIAEN/SBIR/data/Sentinel2/stats/all_stats/FINAL_stats_result_Sentinel2_ALL.csv", row.names=F)







