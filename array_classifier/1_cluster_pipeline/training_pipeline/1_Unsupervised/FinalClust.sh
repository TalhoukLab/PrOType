#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

## Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data set cannot be empty"
#		 exit 1
#fi
#
## specify working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#		 exit 1
#fi
#
## specify output directory
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#		 exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ]; 
#then echo "Path to R directory must be specified"
#fi

##################################################
########## Execute R scripts for merge ###########
##################################################
#export PATH=$RPath:$PATH

R_eval=$workDir$dataSet/R_file/eval/eval_run.R

# execute R scripts
Rscript $R_eval
