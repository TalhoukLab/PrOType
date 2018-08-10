#!/bin/bash

. ./Parameters.sh

file_to_submit=()
for dataset in "${dataSets[@]}"; do

    data_sets=${dataset// /'"','"'}

    # create R script
    mkdir -p $workDir/R_file/CIsuplearn/$dataset
    mkdir -p $workDir/sh_file/CIsuplearn/$dataset

    Rname=$workDir/R_file/CIsuplearn/$dataset/CIsupLearn.R
    shell_file=$workDir/sh_file/CIsuplearn/$dataset/CIsupLearn.sh

    mkdir -p $outputDir'/iv_summary/ci_sup_lrn/'$dataset

    echo 'outDir <- "'$outputDir'"' > $Rname
    echo 'inDir <- "'$outputDir'"' >> $Rname
    echo 'fdat <- c("'$data_sets'")' >> $Rname
    echo "top <- $top" >> $Rname
    echo 'source("R/4_IV_summary/CIsupLearn.R")' >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        echo "Adding To Queue: $shell_file"
        file_to_submit+=($shell_file)
    else
        bash $shell_file
    fi
done


logDir=$baseLogDir'/IVSummary/CIsupLearn'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
