rm(list = ls())
gc(verbose = FALSE)

# Define library folder
.libPaths("/ui/ncsa/nogueir2/NewRPackages")
.libPaths(.libPaths()[1])
setwd("/ui/ncsa/nogueir2/")

# loading required packages
library(data.table)

print("READING THE DATA:")
start_time <- Sys.time()

### loading in the datasets
ihwap_full = read.csv('/ui/ncsa/nogueir2/Data/ihwap_full.csv')

end_time <- Sys.time()
print("ELAPSED TIME TO READ THE DATA:")
end_time - start_time

### setting up bootstrap samples
ihwap_full = data.table(ihwap_full)
setkey(ihwap_full, "Household")


### defining bootstrap samples
set.seed(5)
boots = 200
small_size = 1
boot_samples = replicate(boots, sample(unique(ihwap_full$Household), 
                                    round(small_size*length(unique(ihwap_full$Household))), replace=TRUE))

for(i in 1:200) {
  ihwap_boots = subset(ihwap_full[J(boot_samples[,i]),])
  write.csv(ihwap_boots, paste0("/ui/ncsa/nogueir2/Data/ihwap_boots",i,".csv"), row.names = FALSE)
  print(paste0("BOOTSTRAP SAMPLE ", i, " GENERATED."))
}

print("ALL TASKS FINISHED")
