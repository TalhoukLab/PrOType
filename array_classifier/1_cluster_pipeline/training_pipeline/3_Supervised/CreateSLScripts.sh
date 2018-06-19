#!/bin/bash

. ./Parameters.sh

## specify data set to use
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#	exit 1
#fi
#
## specify number of reps
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#	exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify the input directory
#if [ "$inputDir" = "" ]
#then echo "Input directory must be specified"
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
#	exit 1
#fi
#
## specify the minimum variance
#if [ "$minVar" = "" ]
#then echo "Minimum variance type must be specified"
#	exit 1
#fi
#
## specify the normalization type
#if [ "$normType" = "" ]
#then echo "Normalization type must be specified"
#	exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi

# specify algorithms
algs=(first second third fourth) #ldaRfe qdaRfe rfRfe lassoRfe)

# create R and sh scripts
for dataset in "${dataSets[@]}"; do

    # build directories
    rm -rf $workDir$dataset'/R_file/train'
    rm -rf $workDir$dataset'/sh_file/train'
    mkdir -p $workDir$dataset'/R_file/train'
    mkdir -p $workDir$dataset'/sh_file/train'

    if [ "$normalizeBy" = "Genes" ];
    then
        mkdir -p $outputDir$dataset'/Model-hc-genes_'$dataset
        fname='Model-hc-genes'
    elif [ "$normalizeBy" = "Samples" ];
    then
        mkdir -p $outputDir$dataset'/Model-hc-samples_'$dataset
        fname='Model-hc-samples'
    elif [ "$normalizeBy" = "None" ];
    then
        mkdir -p $outputDir$dataset'/Model-hc_'$dataset
        fname='Model-hc'
    else
        echo "A normalization of type Genes, Samples or None must be specified"
    fi

    for i in "${algs[@]}"; do
        for s in `seq 1 $supervised_reps`; do
            for t in "${threshold[@]}"; do
                #File names for R script, rds output file, shell job script
                R_train=$workDir$dataset/R_file/train/$i$s$t.R
                sh_train=$workDir$dataset/sh_file/train/$i$s$t.sh

                #Content of R file
                touch $R_train
                echo 'dataSet <- "'$dataset'"' >> $R_train
                echo 'reps <- '$s >> $R_train
                echo 'algs <- "'$i'"' >> $R_train
                echo 'inDir <- "'$outputDir$dataset'"' >> $R_train
                echo 'outDir <- "'$outputDir$dataset'"' >> $R_train
                echo 'normalizeBy <- "'$normalizeBy'"' >> $R_train
                echo 'minVar <- '$minVar >> $R_train
                echo 'normType <- "'$normType'"' >> $R_train
                echo 'fname <- "'$fname'"' >> $R_train
                echo 'threshold <- '$t >> $R_train
                echo 'shouldCompute <- '$shouldCompute >> $R_train
                echo 'source("'$workDir'3_Supervised/9_model_train.R")' >> $R_train
                echo 'train_supervised(dataSet, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, fname, shouldCompute)' >> $R_train

                # contents of sh file
                touch $sh_train
                echo '#!/bin/sh' >> $sh_train
                echo 'export PATH='$RPath':$PATH' >> $sh_train
                echo 'Rscript' $R_train >> $sh_train
            done
        done
    done
done