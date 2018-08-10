#!/bin/bash

. ./Parameters.sh

# specify algs
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    # build directories

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

    mkdir -p $outputDir'/supervised/train/'$fname'_'$dataset

    for s in `seq 1 $supervised_reps`; do
        for i in "${supervisedAlgs[@]}"; do
            #File names for R script, rds output file, shell job script
            mkdir -p $workDir/R_file/train/$dataset
            mkdir -p $workDir/sh_file/train/$dataset

            R_train=$workDir/R_file/train/$dataset/$i$s$t.R
            sh_train=$workDir/sh_file/train/$dataset/$i$s$t.sh

            #Content of R file
            echo 'dataSet <- "'$dataset'"' > $R_train
            echo 'reps <- '$s >> $R_train
            echo 'algs <- "'$i'"' >> $R_train
            echo 'inDir <- "'$outputDir$dataset'"' >> $R_train
            echo 'outDir <- "'$outputDir'"' >> $R_train
            echo 'normalizeBy <- "'$normalizeBy'"' >> $R_train
            echo "minVar <- '$minVar'" >> $R_train
            echo 'normType <- "'$normType'"' >> $R_train
            echo 'fname <- "'$fname'"' >> $R_train
            echo 'threshold <- '$threshold >> $R_train
            echo 'shouldCompute <- '$shouldCompute >> $R_train
            echo 'source("R/3_supervised/train.R")' >> $R_train
            echo 'train_supervised(dataSet, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, fname, shouldCompute)' >> $R_train

            # contents of sh file
            echo 'Rscript' $R_train > $sh_train

            if command -v qsub &>/dev/null; then
                # execute shell_file to cluster
                echo "Adding To Queue: $sh_train"
                file_to_submit+=($sh_train)
            fi

            chmod +x $sh_train
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
        python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step train
    fi
done

logDir=$baseLogDir'/supervised/training'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
