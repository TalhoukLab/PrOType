#!/bin/sh

. 4_IVSummary/Parameters.sh

## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify the output directory
#if [ "$inputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify the output directory
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify number of top algorithms
#if ! [[ $top -gt 0 ]]
#then echo "Top needs to be a positive integer"
#	exit 1
#fi

# specify the output directory
#if [ "$IsHousekeepingNormalized" = "" ]
#then echo "IsHousekeepingNormalized must be specified as TRUE or FALSE"
#        exit 1
#fi

# specify the normalization method
#if [ "$normalizeBy" = "" ]
#then echo "Normalization method must be specified"
#        exit 1
#fi

# specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi


# *************************************************************************
# Step 1: 
# *************************************************************************

# extract data set vector
dset=$dataSet
data_sets=${dset// /'"','"'}

# create R script
mkdir $workDir'temp'
Rname=$workDir'temp/CIsupLearn_temp.R'
	
touch $Rname
echo 'outDir <- "'$outputDir'"' > $Rname
echo 'inDir <- "'$inputDir'"' >> $Rname
echo 'fdat <- c("'$data_sets'")' >> $Rname
echo 'top <- 3' >> $Rname
echo 'source("'$workDir'4_IVSummary/CIsupLearn.R")' >> $Rname

Rscript $Rname
