#!/bin/bash

. ./Parameters.sh

# create R and sh scripts
file_to_submit=()
for dataset in "${dataSets[@]}"; do
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

    mkdir -p $outputDir'/supervised/reduce/'$fname'_'$dataset

    for i in "${supervisedAlgs[@]}"; do
        mkdir -p $workDir/R_file/reduce/$dataset
        mkdir -p $workDir/sh_file/reduce/$dataset

        Rname=$workDir/R_file/reduce/$dataset/reduce_$i.R
        shell_file=$workDir/sh_file/reduce/$dataset/reduce_$i.sh

        #Content of R file
        echo 'outDir <- "'$outputDir'"' > $Rname
        echo 'dataSet <- "'${dataset// /'"','"'}'"' >> $Rname
        echo 'alg <- "'$i'"' >> $Rname
        echo 'fname <- "'$fname'"' >> $Rname
        echo 'threshold <- "'$threshold'"' >> $Rname
        echo 'source("R/3_supervised/reduce.R")' >> $Rname
        echo 'reduce_supervised(dataSet, alg, outDir, fname, threshold)' >> $Rname

        # Content of shell script
        echo 'Rscript' $Rname > $shell_file

        chmod +x $shell_file

        if command -v qsub &>/dev/null; then
            # execute shell_file to cluster
            echo "Adding To Queue: $shell_file"
            file_to_submit+=($shell_file)
        else
            bash $shell_file
        fi
    done
done

logDir=$baseLogDir'/supervised/reduce'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi