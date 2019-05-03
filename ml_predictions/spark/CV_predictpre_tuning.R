rm(list = ls())
gc(verbose = FALSE)

fullstart <- Sys.time()

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

### loading in the datasets
spark_read_csv(sc, 'ihwap_full', 'file:///ui/ncsa/nogueir2/Data/ihwap_full.csv')
ihwap_full <- tbl(sc, 'ihwap_full')

end_time <- Sys.time()
print("ELAPSED TIME TO READ THE DATA:")
end_time - start_time

### selecting variables to be used in regressions
ihwap_results = ihwap_full
ihwap_cleanX = dplyr::select(ihwap_full, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2")))

ihwap_clean1 = filter(ihwap_full, treated == 0)
ihwap_clean1 = filter(ihwap_clean1, kfold2 != 1)
ihwap_clean1 = dplyr::select(ihwap_clean1, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

ihwap_clean2 = filter(ihwap_full, treated == 0)
ihwap_clean2 = filter(ihwap_clean2, kfold2 != 2)
ihwap_clean2 = dplyr::select(ihwap_clean2, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

ihwap_clean3 = filter(ihwap_full, treated == 0)
ihwap_clean3 = filter(ihwap_clean3, kfold2 != 3)
ihwap_clean3 = dplyr::select(ihwap_clean3, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

ihwap_clean4 = filter(ihwap_full, treated == 0)
ihwap_clean4 = filter(ihwap_clean4, kfold2 != 4)
ihwap_clean4 = dplyr::select(ihwap_clean4, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

ihwap_clean5 = filter(ihwap_full, treated == 0)
ihwap_clean5 = filter(ihwap_clean5, kfold2 != 5)
ihwap_clean5 = dplyr::select(ihwap_clean5, -one_of(c("Household", "ExistingConsumption", "ProjectedConsumption", "treated", "end_day", "kfold", "kfold2", "rowid")))

print("TRAINING CROSS-VALIDATED ML MODEL:")
start_time <- Sys.time()

############## MANUALLY CROSS VALIDATING
cvlist = list(ihwap_clean1, ihwap_clean2, ihwap_clean3, ihwap_clean4, ihwap_clean5)

## running machine learning models
start_time55 <- Sys.time()
for(i in 1:5) {
ml_model = ml_gbt_regressor(cvlist[[i]], formula = total_mmbtu ~ ., min_info_gain = 0, loss_type="squared", step_size = 0.2, max_depth=20, min_instances_per_node=60, max_iter=50, prediction_col=paste0("pred1_", i))
ml_predictions = ml_predict(ml_model, ihwap_cleanX)
ml_predictions = ml_predictions %>% arrange(rowid)
ml_predictions = dplyr::select(ml_predictions, c(paste0("pred1_", i)))
ihwap_results = ihwap_results %>% arrange(rowid)
ihwap_results = sdf_bind_cols(ihwap_results, ml_predictions)
}
end_time55 <- Sys.time()
print("Time elapsed for models with max 50 iterations:")
end_time55 - start_time55

start_time60 <- Sys.time()
for(i in 1:5) {
ml_model = ml_gbt_regressor(cvlist[[i]], formula = total_mmbtu ~ ., min_info_gain = 0, loss_type="squared", step_size = 0.2, max_depth=20, min_instances_per_node=60, max_iter=60, prediction_col=paste0("pred3_", i))
ml_predictions = ml_predict(ml_model, ihwap_cleanX)
ml_predictions = ml_predictions %>% arrange(rowid)
ml_predictions = dplyr::select(ml_predictions, c(paste0("pred3_", i)))
ihwap_results = ihwap_results %>% arrange(rowid)
ihwap_results = sdf_bind_cols(ihwap_results, ml_predictions)
}
end_time60 <- Sys.time()
print("Time elapsed for models with max 60 iterations:")
end_time60 - start_time60

start_time65 <- Sys.time()
for(i in 1:5) {
ml_model = ml_gbt_regressor(cvlist[[i]], formula = total_mmbtu ~ ., min_info_gain = 0, loss_type="squared", step_size = 0.2, max_depth=20, min_instances_per_node=60, max_iter=70, prediction_col=paste0("pred5_", i))
ml_predictions = ml_predict(ml_model, ihwap_cleanX)
ml_predictions = ml_predictions %>% arrange(rowid)
ml_predictions = dplyr::select(ml_predictions, c(paste0("pred5_", i)))
ihwap_results = ihwap_results %>% arrange(rowid)
ihwap_results = sdf_bind_cols(ihwap_results, ml_predictions)
}
end_time65 <- Sys.time()
print("Time elapsed for models with max 70 iterations:")
end_time65 - start_time65

start_time70 <- Sys.time()
for(i in 1:5) {
ml_model = ml_gbt_regressor(cvlist[[i]], formula = total_mmbtu ~ ., min_info_gain = 0, loss_type="squared", step_size = 0.2, max_depth=20, min_instances_per_node=60, max_iter=80, prediction_col=paste0("pred7_", i))
ml_predictions = ml_predict(ml_model, ihwap_cleanX)
ml_predictions = ml_predictions %>% arrange(rowid)
ml_predictions = dplyr::select(ml_predictions, c(paste0("pred7_", i)))
ihwap_results = ihwap_results %>% arrange(rowid)
ihwap_results = sdf_bind_cols(ihwap_results, ml_predictions)
}
end_time70 <- Sys.time()
print("Time elapsed for models with max 80 iterations:")
end_time70 - start_time70

ihwap_results = ihwap_results %>% collect

end_time <- Sys.time()
print("TIME ELAPSED FOR ALL CV MODELS:")
end_time - start_time

saveRDS(ihwap_results, '/ui/ncsa/nogueir2/ModelOutputs/CV_predcitpre_gbtTune2.rds')
write.csv(ihwap_results, '/ui/ncsa/nogueir2/ModelOutputs/CV_predcitpre_gbtTune2.csv', row.names = FALSE)

fullend <- Sys.time()
print("TOTAL TIME FOR WHOLE SCRIPT:")
fullend - fullstart

print("ALL TASKS FINISHED")
