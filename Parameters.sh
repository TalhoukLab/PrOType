#!/bin/bash

# data information
#dataSet="ov.afc1_cbt"
#dataSets=(ov.afc1_xpn ov.afc1_cbt ov.afc2_xpn ov.afc2_cbt)
dataSets=(ov.afc1_xpn ov.afc1_cbt)
shouldCompute=FALSE

# Unsupervised parameters
reps=5
k=4
c=1 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=5 # 100
normalizeBy="None"
threshold=(0.0 0.5)
minVar="0.0"
normType="conventional"

# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/"
outputDir="/outputs/"
inputDir="/PrOType/raw_data/"
logDir="/logs/"

# Developer params
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
user="$(whoami)"
RPath="$(which R)"