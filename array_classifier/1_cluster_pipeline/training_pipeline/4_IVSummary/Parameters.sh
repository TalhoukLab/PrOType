#!/bin/bash

# user information
user="mpaletta"

# data information
#dataSet=("ov.afc2_cbt ov.afc1_cbt ov.af_cbt ov.afc2_xpn ov.afc1_xpn ov.af_xpn")
dataSet=("ov.afc1_cbt ov.afc1_xpn")

# Training Eval parameters
normalizeBy="None"
top=5

# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/"
outputDir="/outputs/"
inputDir=$outputDir
logDir="/logs/"
RPath="/usr/bin"
