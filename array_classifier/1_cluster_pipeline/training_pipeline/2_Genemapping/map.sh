#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

## specify data set to use
#if [ "$dataset" = "" ]
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
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir$dataset/R_file/map
    R_map=$workDir$dataset/R_file/map/map.R

    # create R script
    touch $R_map
    echo "source('"$workDir"2_Genemapping/GeneMapping.R')" > $R_map
    echo "tdat <- readr::read_rds('"$outputDir$dataset"/data_pr_"$dataset"/tdat_"$dataset".rds')" >> $R_map
    echo "x <- data.frame(t(tdat))" >> $R_map
    echo "dataset <- '"$dataset"'" >> $R_map
    echo "inDir <- '"$workDir"2_Genemapping/'" >> $R_map
    echo "outDir <- '"$outputDir"'" >> $R_map
    echo "map_to_nano(x, dataset, inDir, outDir)" >> $R_map

    # Run Script
    export PATH=$RPath:$PATH
    Rscript $R_map
done