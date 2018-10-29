#!/bin/bash

# Data parameters
trainSet="ov.afc1_xpn"
trainSet2="ov.afc1_cbt"
testSet="ov.afc2_xpn"
dataSets=($trainSet $trainSet2 $testSet)

# Unsupervised parameters
reps=1000
k=4
c=50
referenceClass="majority"

# Supervised parameters
supervised_reps=500
normalizeBy="None"
threshold=0
minVar=0
normType="conventional"
top=5

# Gene Selection parameters
numBootstraps=500

# Directory parameters
workDir="/extscratch/shahlab/huntsman/dchiu/workDir"
outputDir="/extscratch/shahlab/huntsman/dchiu/outputs"
inputDir="/home/dchiu/Projects/PrOType/raw_data"
baseLogDir="/extscratch/shahlab/huntsman/dchiu/logs"

. ./assets/dev_params.sh
. ./assets/check.sh
