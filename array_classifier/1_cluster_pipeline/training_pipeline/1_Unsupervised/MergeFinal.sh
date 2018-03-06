#!/bin/sh

##################################################
############# Input Paremeters for Run ###########
##################################################

. 1_Unsupervised/Parameters.sh

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
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ]; 
#then echo "Path to R directory must be specified"
#fi

##################################################
########## Execute R scripts for merge ###########
##################################################
export PATH=$RPath:$PATH

# get R scripts
R_merge_final_clust=$workDir$dataSet/R_file/merge/Merge_final_clust.R
R_merge_final_consmat=$workDir$dataSet/R_file/merge/Merge_final_consmat.R

# execute R scripts
Rscript $R_merge_final_clust
Rscript $R_merge_final_consmat
