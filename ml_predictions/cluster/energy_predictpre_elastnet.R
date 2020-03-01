rm(list = ls())
gc(verbose = FALSE)

# Define library folder
.libPaths("/projects/aces/nogueir2/NewRPackages")
.libPaths(.libPaths()[1])

### Function to check if all required packages are installed and loaded
packageCheck = function(x) {
  if (!require(x, character.only = T)) {
    install.packages(x, dep = T, repos="https://ftp.ussg.iu.edu/CRAN/")
    library(x, character.only = T, lib.loc = "/projects/aces/nogueir2/NewRPackages")
  }
}

## Type in the packages you need below
pkg <- c("SuperLearner", "ggplot2", "RhpcBLASctl", "snow", "parallel", "DescTools", "data.table")
suppressPackageStartupMessages(lapply(pkg, packageCheck))

## set up parallellization, detecting number of cores:
#num_cores = detectCores()
cluster = parallel::makeForkCluster(nnodes=20, outfile="/projects/aces/nogueir2/ClusterOutputs/clust.out.predictpre_elastnet")
print(cluster)

## Need to load all libraries in all cores
parallel::parLapply(cluster, pkg, packageCheck)

# read the data
ihwap_complete = read.csv("/projects/aces/nogueir2/Data/ihwap_full_flex.csv")

# drop some variables not to be used
ihwap_full = subset(ihwap_complete, select = -c(Household, ExistingConsumption, ProjectedConsumption, end_day, kfold, kfold2, rowid))
ihwap_completeX = subset(ihwap_complete, select = -c(Household, ExistingConsumption, ProjectedConsumption, end_day, kfold, kfold2, rowid, total_mmbtu, treated))

# will train model only with untreated homes
ihwap_full = ihwap_full[which(ihwap_full$treated==0), ]

### Data Frames for the models
# dependent variable
train_y = as.matrix(subset(ihwap_full, select = c("total_mmbtu")))
# independent variables
train_x = subset(ihwap_full, select = -c(total_mmbtu, treated))

######## Setup Parallel SuperLearner
# Load the SuperLearner package on all workers so they can find
# SuperLearner::All(), the default screening function which keeps all variables.
parallel::clusterEvalQ(cluster, library(SuperLearner))

# Function to create a list of model names
expandingList <- function(capacity = 10) {
    buffer <- vector('list', capacity)
    length <- 0
    
    methods <- list()
    
    methods$double.size <- function() {
        buffer <<- c(buffer, vector('list', capacity))
        capacity <<- capacity * 2
    }
    
    methods$add <- function(val) {
        if(length == capacity) {
            methods$double.size()
        }
        
        length <<- length + 1
        buffer[[length]] <<- val
    }
    
    methods$as.list <- function() {
        b <- buffer[0:length]
        return(b)
    }
    
    methods
}
SL.library<-expandingList()


### different models to try
# glmnet
glmnet.tune <- list(alpha = c(0, 0.25, 0.5, 0.75, 1),
			nfolds = c(5),
			dfmax = c(50, 100, 200, 300))

# Set detailed names = T so we can see the configuration for each function.
# Also shorten the name prefix.
glmnet <- create.Learner("SL.glmnet", tune = glmnet.tune, detailed_names = T, name_prefix = "glmnet")
for(i in 1:length(glmnet$names)){
    SL.library$add(c(glmnet$names[i]))
}

# We need to explictly export our custom learner functions to the workers.
parallel::clusterExport(cluster, glmnet$names)

# We need to set a different type of seed that works across cores.
# This version is for SNOW parallelization.
# Otherwise the other cores will go rogue and we won't get repeatable results.
parallel::clusterSetRNGStream(cluster, 1)

fit.gaselec.SL = snowSuperLearner(Y=train_y, X=train_x, cluster=cluster,
                            SL.library=SL.library$as.list(), family=gaussian(),
                            method="method.NNLS", verbose=TRUE, cvControl = list(V = 5))
saveRDS(fit.gaselec.SL, "/projects/aces/nogueir2/ModelOutputs/predictpre_model_elastnet")


# predictions for pre-data
in_preds = as.data.frame(fit.gaselec.SL$library.predict)
cv_preds = as.data.frame(fit.gaselec.SL$Z)
colnames(cv_preds) =  sprintf('cv_%s', colnames(in_preds)) 
colnames(in_preds) =  sprintf('in_%s', colnames(in_preds))

data_export = subset(ihwap_complete[which(ihwap_complete$treated==0), ], select = c(Household, end_day, end_month, end_year))
data_export = as.data.frame(cbind(data_export, in_preds, cv_preds))

saveRDS(data_export, '/projects/aces/nogueir2/ModelOutputs/predictpre_results_elastnet.rds')
write.csv(data_export, '/projects/aces/nogueir2/ModelOutputs/predictpre_results_elastnet.csv', row.names = FALSE)


# predictions for post-data
predictions = predict(fit.gaselec.SL, ihwap_completeX, onlySL = T)
data_export2 = subset(ihwap_complete, select = c(Household, end_day, end_month, end_year))
data_export2$pred_elastnet = unlist(as.numeric(predictions$pred))

saveRDS(data_export2, '/projects/aces/nogueir2/ModelOutputs/predictpost_results_elastnet.rds')
write.csv(data_export2, '/projects/aces/nogueir2/ModelOutputs/predictpost_results_elastnet.csv', row.names = FALSE)


