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
user="$(whoami)"
rootDir="/home"/$user
projDir=$rootDir"/Projects/PrOType"
inputDir=$projDir"/raw_data"
resultsDir=$rootDir"/results/PrOType"
scriptDir=$resultsDir"/scripts"
RDir=$scriptDir"/R"
shDir=$scriptDir"/sh"
outputDir=$resultsDir"/outputs"
logDir=$resultsDir"/logs"

. ./assets/dev_params.sh
. ./assets/check.sh
