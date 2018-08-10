#!/bin/bash

. ./Parameters.sh


##################################################
########## Execute R scripts for merge ###########
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/eval/$dataset
    mkdir -p $workDir/sh_file/eval/$dataset

    R_eval=$workDirR_file/eval/$dataset/eval_run.R
    shell_file=$workDir/sh_file/eval/$dataset/eval_run.sh

    mkdir -p $outputDir'/unsupervised/final/'$dataset

    # create R script
    echo "fdir <- '"$outputDir"'" > $R_eval
    echo "dat <- '"$dataset"'" >> $R_eval
    echo "referenceClass <- '"$referenceClass"'" >> $R_eval
    echo 'shouldCompute <- '$shouldCompute >> $R_eval
    echo "source('R/1_unsupervised/final_clust.R')" >> $R_eval

    # execute R scripts
    echo "Rscript $R_eval" > $shell_file

    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        echo "Adding To Queue: $shell_file"
        file_to_submit+=($shell_file)
    else
        bash $shell_file
    fi
done

logDir=$baseLogDir'/unsupervised/finalClust'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
