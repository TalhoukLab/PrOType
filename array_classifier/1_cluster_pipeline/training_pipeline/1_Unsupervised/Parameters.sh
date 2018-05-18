#!/bin/sh

# User Params

# user information
user="mpaletta"

# data information
dataSet="ov.afc1_cbt_sample"

# Unsupervised parameters
reps=5
k=4
c=1 # use for determining splitting criterion (min 100 reps required)


# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/"
outputDir="/outputs/"
inputDir="/PrOType/array_classifier/1_cluster_pipeline/raw_data/raw_data/"
logDir="/logs/"
RPath="/usr/bin"


# Developer params
#algs=(nmfbrunet nmflee distalgs rest)
algs=(distalgs rest)

cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)