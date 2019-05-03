#!/bin/bash

### Change the default package library for R
export R_LIBS=/ui/ncsa/nogueir2/NewRPackages

### make sure you use Spark version 2
export SPARK_MAJOR_VERSION=2

### load R, sparkR, and Java
module load R/3.4.3
module load hdp/spark2
module load java/java-1.8.171

### run your code
spark-submit --master yarn --deploy-mode client --num-executors 3 --executor-cores 15 --executor-memory 64G --driver-memory 8G --verbose --conf spark.rpc.message.maxSize=2046 --conf spark.driver.maxResultSize=2G /ui/ncsa/nogueir2/preprocess_bootstrap.R
