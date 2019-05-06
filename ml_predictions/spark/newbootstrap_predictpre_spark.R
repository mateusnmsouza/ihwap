rm(list = ls())
gc(verbose = FALSE)

# Define library folder
.libPaths("/ui/ncsa/nogueir2/NewRPackages")
.libPaths(.libPaths()[1])
setwd("/ui/ncsa/nogueir2/")

# loading required packages
library(sparklyr)
library(dplyr)
#spark_install(version = "2.2.0")

# establish spark connection
conf <- spark_config()
conf$spark.driver.memory = "8G"
conf$spark.driver.maxResultSize = "2G"
conf$spark.rpc.message.maxSize = 2046
conf$spark.executor.memory <- "64G"
conf$spark.executor.cores <- 15
conf$spark.executor.instances <- 3
# multiply instances by cores -> total cores
# max cores = 100
# max memory (theory) = 1TB

conf$spark.dynamicAllocation.enabled <- "false"
conf <- spark_config()

sc <- spark_connect(master = "yarn-cluster", 
                    config = conf)

print("READING THE DATA:")
start_time <- Sys.time()

filelist = readRDS('/ui/ncsa/nogueir2/Data/filenames.rds')
print(paste0(filelist[1]))

### loading in the datasets
spark_read_csv(sc, 'ihwap_full', paste0('file:///ui/ncsa/nogueir2/Data/', filelist[1], ".csv"))
ihwap_full <- tbl(sc, 'ihwap_full')

end_time <- Sys.time()
print("ELAPSED TIME TO READ THE DATA:")
end_time - start_time

### selecting variables to be used in regressions
ihwap_clean = filter(ihwap_full, treated == 0)
ihwap_clean = dplyr::select(ihwap_clean, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

print("TRAINING ML MODEL:")
start_time <- Sys.time()

## running machine learning model
ml_model = ml_gbt_regressor(ihwap_clean, formula = total_mmbtu ~ ., min_info_gain = 0, loss_type="squared", step_size = 0.2, max_depth=20, min_instances_per_node=60, max_iter=70, prediction_col="prediction")

end_time <- Sys.time()
print("ELAPSED TIME FOR TRAINING NL MODEL:")
end_time - start_time

print("GETTING PREDICTIONS AND EXPORTING RESULTS:")
start_time <- Sys.time()

## model predictions
ihwap_X = dplyr::select(ihwap_full, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2")))
predict_energy = ml_predict(ml_model, ihwap_X)
predict_energy = dplyr::select(predict_energy, c("rowid", "prediction"))
predict_energy = predict_energy %>% collect

## exporting predictions
write.csv(predict_energy, paste0('/ui/ncsa/nogueir2/ModelOutputs/predpre_',  filelist[1], '.csv'), row.names = FALSE)

end_time <- Sys.time()
print("ELAPSED TIME FOR PREDICTION AND EXPORTING RESULTS:")
end_time - start_time

filelist = filelist[-1]
saveRDS(filelist, '/ui/ncsa/nogueir2/Data/filenames.rds')

print("ALL TASKS FINISHED")
