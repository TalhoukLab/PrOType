#!/bin/sh

# User Params

# user information
user="$(whoami)"

# data information
dataSet="ov.afc1_xpn"

# Unsupervised parameters
reps=5
k=4
c=1 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=5 # 100
normalizeBy="None"
threshold="0.0"
minVar="0.0"
normType="conventional"

# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/"
outputDir="/outputs/"
inputDir="/PrOType/raw_data/"
logDir="/logs/"
RPath="/usr/bin"

# Developer params
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
