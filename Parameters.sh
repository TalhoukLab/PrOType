#!/bin/bash

# data information
#dataSets=(ov.afc1_xpn ov.afc1_cbt ov.afc2_xpn ov.afc2_cbt)
dataSets=(ov.afc1_cbt ov.afc1_xpn)
shouldCompute=FALSE

# Unsupervised parameters
reps=10
k=4
c=1 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=5 # 5
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

# Gene Selection
numBootstraps=5 # 500

# directory inputs
workDir="/extscratch/shahlab/huntsman/dchiu/workDir_cleanup/"
outputDir="/extscratch/shahlab/huntsman/dchiu/outputs_cleanup/"
inputDir="/home/dchiu/Projects/PrOType/raw_data/"
baseLogDir="/extscratch/shahlab/huntsman/dchiu/logs_cleanup/"

. ./assets/dev_params.sh
