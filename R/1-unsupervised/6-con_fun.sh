#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

# specify consensus metrics to compute
file_to_submit=()
##################################################
############ Execute jobs on cluster #############
##################################################
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/consensus/$dataset
    mkdir -p $workDir/sh_file/consensus/$dataset

    mkdir -p $outputDir'/unsupervised/consensus/'$dataset

    for i in "${cons[@]}"; do
        # execute shell_file to cluster node

        # file names
        R_cons=$workDir/R_file/consensus/$dataset/Create_$i.R
        sh_cons=$workDir/sh_file/consensus/$dataset/Create_$i.sh

        mkdir -p $outputDir'/unsupervised/consensus/'$dataset

        # create R scripts
        echo 'outputDir <- "'$outputDir'"' > $R_cons
        echo 'dataset <- "'$dataset'"' >> $R_cons
        echo 'cons.funs <- "'$i'"'>> $R_cons
        echo "k <- $k" >> $R_cons
        echo 'dir <- "'$outputDir$dataset'/data_pr_'$dataset'"' >> $R_cons
        echo 'shouldCompute <- '$shouldCompute >> $R_cons
        echo 'source("R/1-unsupervised/6-con_fun.R")' >> $R_cons

        # create sh scripts
        echo 'Rscript' $R_cons > $sh_cons
        chmod +x $sh_cons

        if command -v qsub &>/dev/null; then
            file_to_submit+=($sh_cons)
            echo -e "$GREEN_TICK Added to queue: $sh_cons"
        fi
    done

    if command -v qsub &>/dev/null; then
        :
    else
      python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step consensus
    fi
done

logDir=$baseLogDir'/unsupervised/confun'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
