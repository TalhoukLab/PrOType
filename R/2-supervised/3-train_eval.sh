#!/bin/bash

. ./Parameters.sh

# extract correct Model directories
if [ "$normalizeBy" = "Genes" ];
then
	mname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ];
then
	mname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ];
then
	mname='Model-hc'
else
	echo "A normalization of type Genes, Samples or None must be specified"
fi

for dataset in "${dataSets[@]}"; do
    # create R script
    mkdir -p $workDir/sh_file/train/$dataset
    mkdir -p $workDir/R_file/train/$dataset

    mkdir -p $outputDir'/supervised/train_eval'

    Rname=$workDir/R_file/train/$dataset/train_eval.R
    shell_file=$workDir/sh_file/train/$dataset/train.sh

    echo 'outputDir <- "'$outputDir'"' > $Rname
    echo 'dataset <- "'$dataset'"' >> $Rname
    echo 'mname <- "'$mname'"' >> $Rname
    echo 'source("R/2-supervised/3-train_eval.R")' >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        file_to_submit+=($shell_file)
        echo -e "$GREEN_TICK Added to queue: $shell_file"
    else
        bash $shell_file
    fi
done

logDir=$baseLogDir'/supervised/train_eval'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
