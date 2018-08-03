#!/bin/bash

. ./Parameters.sh

# create R and sh scripts
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    # extract correct Model directories
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
        mkdir -p $workDir$dataset/R_file/reduce
        mkdir -p $workDir$dataset/sh_file/reduce

        Rname=$workDir$dataset/R_file/reduce/reduce_$i.R
        shell_file=$workDir$dataset/sh_file/reduce/reduce_$i.sh

        #Content of R file
        echo 'outDir <- "'$outputDir'"' > $Rname
        echo 'dataSet <- "'${dataset// /'"','"'}'"' >> $Rname
        echo 'alg <- "'$i'"' >> $Rname
        echo 'fname <- "'$fname'"' >> $Rname
        echo 'threshold <- "'$threshold'"' >> $Rname
        echo 'source("R/supervised/reduce.R")' >> $Rname
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
