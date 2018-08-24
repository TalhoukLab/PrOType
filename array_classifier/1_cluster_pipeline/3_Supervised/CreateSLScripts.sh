#!/bin/bash

. ./Parameters.sh

for dataset in "${dataSets[@]}"; do

    # build directories

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

    for i in "${supervisedAlgs[@]}"; do
        for s in `seq 1 $supervised_reps`; do
            #File names for R script, rds output file, shell job script
            mkdir -p $workDir$dataset/R_file/train
            mkdir -p $workDir$dataset/sh_file/train

            R_train=$workDir$dataset/R_file/train/$i$s$t.R
            sh_train=$workDir$dataset/sh_file/train/$i$s$t.sh

            #Content of R file
            echo 'dataSet <- "'$dataset'"' > $R_train
            echo 'reps <- '$s >> $R_train
            echo 'algs <- "'$i'"' >> $R_train
            echo 'inDir <- "'$outputDir$dataset'"' >> $R_train
            echo 'outDir <- "'$outputDir$dataset'"' >> $R_train
            echo 'normalizeBy <- "'$normalizeBy'"' >> $R_train
            echo "minVar <- '$minVar'" >> $R_train
            echo 'normType <- "'$normType'"' >> $R_train
            echo 'fname <- "'$fname'"' >> $R_train
            echo 'threshold <- '$threshold >> $R_train
            echo 'shouldCompute <- '$shouldCompute >> $R_train
            echo 'library(magrittr)' >> $R_train
            echo 'library(splendid)' >> $R_train
            echo 'source("'$workDir'3_Supervised/9_model_train.R")' >> $R_train
            echo 'train_supervised(dataSet, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, fname, shouldCompute)' >> $R_train

            # contents of sh file
            echo 'Rscript' $R_train > $sh_train
        done
    done
done
