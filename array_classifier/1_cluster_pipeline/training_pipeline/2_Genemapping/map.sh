#!/bin/sh

##################################################
############# Input Paremeters for Run ###########
##################################################

. 2_Genemapping/Parameters.sh

## specify data set to use
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#	exit 1
#fi
#
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
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi

#************************************************
#************* create mapping scripts ***********
#************************************************
mkdir -p $workDir$dataSet/R_file/map
R_map=$workDir$dataSet/R_file/map/map.R

# create R script
touch $R_map
echo "source('"$workDir"2_Genemapping/GeneMapping.R')" > $R_map
echo "tdat <- readr::read_rds('"$outputDir$dataSet"/data_pr_"$dataSet"/tdat_"$dataSet".rds')" >> $R_map
echo "x <- data.frame(t(tdat))" >> $R_map
echo "dataSet <- '"$dataSet"'" >> $R_map
echo "inDir <- '"$workDir"2_Genemapping/'" >> $R_map
echo "outDir <- '"$outputDir"'" >> $R_map
echo "map_to_nano(x, dataSet, inDir, outDir)" >> $R_map

# Run Script
export PATH=$RPath:$PATH
Rscript $R_map
