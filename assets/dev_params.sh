#!/usr/bin/env bash
contains() {
    [[ $1 =~ (^|[[:space:]])$2($|[[:space:]]) ]] && return TRUE || return FALSE
}


# Developer params
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
classificationAlgs=(adaboost rf mlr_ridge mlr_lasso)
supervisedAlgs=(first second third fourth) #ldaRfe qdaRfe rfRfe lassoRfe)

# Bash info
user="$(whoami)"
RPath="$(which R)"

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
if ! [[ $reps -gt 0 ]]; then
    echo "reps Needs To Be Positive Integer"
	exit 1
fi

if ! [[ $top -gt 0 ]]
then echo "Top needs to be a positive integer"
	exit 1
fi


# specify the number of clusters k
if ! [[ $k -gt 0 ]]
then echo "k (number of clusters) needs to be a positive integer"
	exit 1
fi

if ! [[ $c -gt 0 ]]; then
    echo "c (# specify minimum number of reps required for merge.  the modulus of the dividend (reps) and divisor (c) must be greater or equal to zero."
	exit 1
fi


if ! [[ contains $dataSets $trainSet ]]; then
    echo "Your datasets must include your trainset."
	exit 1
fi

if ! [[ contains $dataSets $testSet ]]; then
    echo "Your datasets must include your testSet."
	exit 1
fi
