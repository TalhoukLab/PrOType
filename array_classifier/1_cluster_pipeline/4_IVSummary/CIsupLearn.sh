#!/bin/bash

. ./Parameters.sh

file_to_submit=()
for dataset in "${dataSets[@]}"; do

    data_sets=${dataset// /'"','"'}

    # create R script
    mkdir -p $workDir$dataset/R_file/CIsuplearn
    mkdir -p $workDir$dataset/sh_file/CIsuplearn

    Rname=$workDir$dataset/R_file/CIsuplearn/CIsupLearn.R
    shell_file=$workDir$dataset/sh_file/CIsuplearn/CIsupLearn.sh

    echo 'outDir <- "'$outputDir'"' > $Rname
    echo 'inDir <- "'$outputDir'"' >> $Rname
    echo 'fdat <- c("'$data_sets'")' >> $Rname
    echo "top <- $top" >> $Rname
    echo 'source("'$workDir'4_IVSummary/CIsupLearn.R")' >> $Rname

    # Run Script
    echo "Rscript $Rname" > $shell_file
    chmod +x $shell_file

    if command -v qsub &>/dev/null; then
        echo "Using: $shell_file"
        file_to_submit+=($shell_file)
    else
        bash $shell_file
    fi
done



if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi