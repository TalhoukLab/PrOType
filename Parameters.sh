#!/bin/bash

# data information
trainSet="ov.afc1_xpn"
trainSet2="ov.afc1_cbt"
testSet="ov.afc2_xpn"

shouldCompute=FALSE

# Unsupervised parameters
reps=1000
k=4
c=50 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=500 # 5
normalizeBy="None"
threshold=0.0
minVar=0
normType="conventional"

# IV Summary
top=5

# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/"
outputDir="/outputs/"
inputDir="/PrOType/raw_data/"
logDir="/logs/"

. ./assets/dev_params.sh
