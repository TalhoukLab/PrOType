#!/usr/bin/env bash

# Developer params
dataSets=($trainSet $trainSet2 $testSet)
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
classificationAlgs=(adaboost rf mlr_ridge mlr_lasso)
supervisedAlgs=(first second third fourth)

# Bash info
user="$(whoami)"
RPath="$(which R)"
maxQueueLength=8000

# specify the working directory
if [ "$workDir" = "" ]; then
  echo "Working directory must be specified"
	exit 1
fi

# specify the output directory
if [ "$outputDir" = "" ]; then
  echo "Output directory must be specified"
	exit 1
fi

# specify log directory
if [ "$logDir" = "" ]; then
  echo "Log directory must be specified"
	exit 1
fi

# specify the normalization method
if [ "$normalizeBy" = "" ]; then
  echo "Normalization method must be specified"
  exit 1
fi

# specify the minimum variance
if ! [[ $minVar -ge 0 ]]; then
  echo "Minimum variance type must be greater than or equal to 0.0"
	exit 1
fi

# specify the normalization type
if [ "$normType" = "" ]; then
  echo "Normalization type must be specified"
	exit 1
fi

# specify R path to place in $PATH
if [ "$RPath" = "" ]; then
  echo "Path to R directory must be specified"
  exit 1
fi

# specify number of bootstrap reps
if ! [[ $reps =~ ^[1-9][0-9]*$ ]]; then
  echo "reps needs to be a positive integer"
	exit 1
fi

# specify number of top algorithms
if ! [[ $top =~ ^[1-9][0-9]*$ ]]; then
  echo "top needs to be a positive integer"
	exit 1
fi

# specify the cluster size
if ! [[ $k =~ ^[1-9][0-9]*$ ]]; then
  echo "k needs to be a positive integer"
	exit 1
fi

# specify number of items to merge per script
if ! [[ $c =~ ^[1-9][0-9]* && $(echo "$reps % $c" | bc) == 0 ]]; then
  echo "c needs to be a positive integer and reps needs to be divisible by c"
	exit 1
fi
