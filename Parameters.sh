#!/bin/bash

# data information
#dataSets=(ov.afc1_xpn ov.afc1_cbt ov.afc2_xpn ov.afc2_cbt)
dataSets=(ov.afc1_cbt ov.afc1_xpn)
shouldCompute=TRUE

# Unsupervised parameters
reps=1000
k=4
c=50 # use for determining splitting criterion

# Supervised parameters
supervised_reps=500
normalizeBy="None"
threshold=0.0
minVar=0
normType="conventional"

# IV Summary
top=5

# Post Processing
trainSet="ov.afc1_xpn"
trainSet2="ov.afc1_cbt"
testSet="ov.afc2_xpn"

# directory inputs
workDir="/home/dchiu/Projects/PrOType/array_classifier/1_cluster_pipeline/"
outputDir="/extscratch/shahlab/huntsman/dchiu/outputs/"
inputDir="/home/dchiu/Projects/PrOType/raw_data/"
logDir="/extscratch/shahlab/huntsman/dchiu/logs/"

. ./assets/dev_params.sh
