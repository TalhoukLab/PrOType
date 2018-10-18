#!/bin/bash

# data information
dataSets=(ov.afc1_cbt ov.afc1_xpn)
shouldCompute=FALSE

# Unsupervised parameters
reps=1000
k=4
c=50

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

# Gene Selection
numBootstraps=500

# directory inputs
workDir="/extscratch/shahlab/huntsman/dchiu/workDir"
outputDir="/extscratch/shahlab/huntsman/dchiu/outputs"
inputDir="/home/dchiu/Projects/PrOType/raw_data"
baseLogDir="/extscratch/shahlab/huntsman/dchiu/logs"

. ./assets/dev_params.sh
