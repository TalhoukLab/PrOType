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
    echo 'source("R/3_supervised/train_eval.R")' >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file

    if command -v qsub &>/dev/null; then
        echo "Adding To Queue: $shell_file"
        file_to_submit+=($shell_file)
        chmod +x $shell_file
    else
        bash $shell_file
    fi
done

logDir=$baseLogDir'/IVSummary/train_eval'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
