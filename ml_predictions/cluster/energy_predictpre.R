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
cluster = parallel::makeForkCluster(nnodes=20, outfile="/projects/aces/nogueir2/ClusterOutputs/clust.out.bootstrap_predictpre")
print(cluster)

## Need to load all libraries in all cores
parallel::parLapply(cluster, pkg, packageCheck)

# read the data
ihwap_complete = readRDS("/projects/aces/nogueir2/Data/ihwap_merged.rds")

# drop some variables not to be used
ihwap_complete = subset(ihwap_complete, select = c(Household, treated, total_mmbtu, HDD60, HDD65, CDD75,
                                               ExistingConsumption, ProjectedConsumption, sqfeet, noccupants, nwindows,
                                               nstories, builddate, CountyID, ShieldingClass, Blower_Pre, tbesetting,
                                               MainHeatFuel, HeatTypeMain, MainHeatBTU, Attic_RValue, Real_income,
                                               white, black, hispanic, asian, nativeamerican, otherrace, renter, female,
                                               Disabled, haselderly, hasminor, nbedrooms, audit_day, audit_month, audit_year,
                                               start_day, start_month, start_year, end_day, end_month, end_year, meterdays,
                                               priority, ProgramYear, tmin, tmax, precip))
ihwap_full = subset(ihwap_complete, select = -c(audit_day, audit_month, audit_year, start_day, start_month, start_year, end_day))

# keeping only homes that heat with Gas
ihwap_full = ihwap_full[which(ihwap_full$MainHeatFuel=="1 - Natural Gas"), ]
ihwap_full = subset(ihwap_full, select = -c(MainHeatFuel))

# preprocessing the control variables
ihwap_full$MainHeatBTU_wins = Winsorize(ihwap_full$MainHeatBTU, na.rm=TRUE)
ihwap_full = subset(ihwap_full, select = -c(MainHeatBTU))

ihwap_full$hasattic = ifelse(ihwap_full$Attic_RValue>0, 1, 0)
ihwap_full$hasattic = ifelse(is.na(ihwap_full$hasattic)==TRUE, 0, ihwap_full$hasattic)
ihwap_full$hasattic = as.numeric(ihwap_full$hasattic)

categorical_vars = which(colnames(ihwap_full) %in% 
                           c("buildingdate", "noccupants", "ShieldingClass", "CountyID",
                             "nstories",  "Disabled", "HeatTypeMain", "tbesetting"))

ihwap_full[, categorical_vars] = lapply(ihwap_full[, categorical_vars], as.factor)
ihwap_full[, categorical_vars] = lapply(ihwap_full[, categorical_vars], as.numeric)

# droppping non-processed variables that have too many NAs, or are not relevant for ML
ihwap_full = subset(ihwap_full, select = -c(Attic_RValue))

# convert every variable to numeric
ihwap_full[,c(1:length(ihwap_full))] = lapply(ihwap_full[,c(1:length(ihwap_full))], as.numeric)

# keep only complete cases (dropping NAs from all variables)
ihwap_full = ihwap_full[complete.cases(ihwap_full), ]

# will train model only with untreated homes
ihwap_full = ihwap_full[which(ihwap_full$treated==0), ]

# selecting outer cross validation rows
ihwap_notreat = data.table(ihwap_full)

### defining bootstrap samples
boots = 500
small_size = 1
boot_samples = replicate(boots, sample(unique(ihwap_full$Household), 
                                    round(small_size*length(unique(ihwap_full$Household))), replace=TRUE))

ihwap_full = data.table(ihwap_full)
setkey(ihwap_full, "Household")

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
# xgboost
xgboost.tune <- list(ntrees = c(1000),
                     max_depth = c(20, 30),
                     shrinkage = c(0.05),
                     minobspernode = c(30))

# Set detailed names = T so we can see the configuration for each function.
# Also shorten the name prefix.
xgboost <- create.Learner("SL.xgboost", tune = xgboost.tune, detailed_names = T, name_prefix = "xgb")
for(i in 1:length(xgboost$names)){
    SL.library$add(c(xgboost$names[i]))
}

# We need to explictly export our custom learner functions to the workers.
parallel::clusterExport(cluster, xgboost$names)

# We need to set a different type of seed that works across cores.
# This version is for SNOW parallelization.
# Otherwise the other cores will go rogue and we won't get repeatable results.
parallel::clusterSetRNGStream(cluster, 1)


### running the model with 100 bootstrap samples
for(i in 1:boots) {
fit.gaselec.SL = snowSuperLearner(Y=as.matrix(subset(ihwap_full[J(boot_samples[,i]),], select = c("total_mmbtu"))),
			    X=subset(ihwap_full[J(boot_samples[,i]),], select = -c(total_mmbtu, Household, ExistingConsumption, ProjectedConsumption, treated)),
			    cluster=cluster,
                            SL.library=SL.library$as.list(), family=gaussian(), id=as.matrix(subset(ihwap_full[J(boot_samples[,i]),], select = c("Household"))),
                            method="method.NNLS", verbose=TRUE, cvControl = list(V = 5))
saveRDS(fit.gaselec.SL, paste0("/projects/aces/nogueir2/ModelOutputs/bootstrap_predictpre",i))
}
