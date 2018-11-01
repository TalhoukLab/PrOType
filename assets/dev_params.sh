#!/bin/bash

# Developer parameters
shouldCompute=FALSE
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
supervisedAlgs=(first second third fourth)
geneSelectionAlgs=(lasso rf ada)

# Bash parameters
RPath="$(which R | sort | tail -n 1)"
GREEN_TICK='\033[0;32m\xe2\x9c\x94\033[0m'
GREEN_BULLET='\033[0;32m\xe2\x80\xa2\033[0m'
BLUE_BULLET='\033[0;34m\xe2\x80\xa2\033[0m'
RED_CROSS='\033[0;31m\xe2\x9c\x96\033[0m'

# Queue parameters
maxQueueLength=8000
shouldWait=TRUE
mem_free="4G"
mem_token="4G"
h_vmem="8G"
