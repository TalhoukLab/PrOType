#!/bin/bash

. ./Parameters.sh

# extract correct Model directories
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

for dataset in "${dataSets[@]}"; do
    # extract data set vector
    data_sets=${dataset// /'"','"'}

    # create R script
    mkdir -p $workDir/sh_file/train/
    mkdir -p $workDir/R_file/train/

    Rname=$workDir$dataset/R_file/train/train_eval.R
    shell_file=$workDir$dataset/sh_file/train/train.sh

    echo 'fdir <- "'$outputDir'"' > $Rname
    echo 'ndat <- c("'$data_sets'")' >> $Rname
    echo 'mname <- "'$fname'"' >> $Rname
    echo 'source("R/IV_summary/train_eval.R")' >> $Rname

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
