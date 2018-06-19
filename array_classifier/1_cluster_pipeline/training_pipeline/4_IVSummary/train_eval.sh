#!/bin/bash

. ./Parameters.sh

## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify the output directory
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify the normalization method
#if [ "$normalizeBy" = "" ]
#then echo "Normalization method must be specified"
#        exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi


# *************************************************************************
# Step 1: iterate through each study and output internal validation tables
# *************************************************************************

# extract correct Model directories
if [ "$normalizeBy" = "Genes" ];
then
	fname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ];
then
	fname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ];
then
	fname='Model-hc'
else
	echo "A normalization of type Genes, Samples or None must be specified"
fi

for dataset in "${dataSets[@]}"; do
    # extract data set vector
    dset=$dataset
    data_sets=${dset// /'"','"'}
    #data_sets=$dset

    # create R script
    mkdir -p $workDir'temp'
    Rname=$workDir'temp/train_eval.R'

    touch $Rname
    echo 'fdir <- "'$outputDir'"' > $Rname
    echo 'ndat <- c("'$data_sets'")' >> $Rname
    echo 'mname <- "'$fname'"' >> $Rname
    echo 'source("'$workDir'4_IVSummary/train_eval.R")' >> $Rname

    Rscript $Rname
done