#!/bin/bash

# data information
#dataSet="ov.afc1_cbt"
#dataSets=(ov.afc1_xpn ov.afc1_cbt ov.afc2_xpn ov.afc2_cbt)
dataSets=(ov.afc1_xpn ov.afc1_cbt)
#dataSets=(ov.afc1_xpn)
shouldCompute=TRUE

# Unsupervised parameters
reps=5
k=4
c=1 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=5 # 100
normalizeBy="None"
threshold=(0.0 0.5)
minVar=0.0
normType="conventional"

# IV Summary
top=3

# directory inputs
workDir=~/"PrOType/array_classifier/1_cluster_pipeline/"
outputDir="/share/lustre/mpaletta/Projects/PrOType/outputs/"
inputDir=~/"PrOType/raw_data/"
logDir=~"/logs/"

. ./assets/dev_params.sh