#!/bin/bash

. ./Parameters.sh

# extract correct Model directories
if [ "$normalizeBy" = "Genes" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc-genes_'$dataSet
	fname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc-samples_'$dataSet
	fname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc_'$dataSet
	fname='Model-hc'
else
	echo "A normalization of type Genes, Samples or None must be specified"
fi

# create R and sh scripts
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    for i in "${supervisedAlgs[@]}"; do
        for t in "${threshold[@]}"; do
            mkdir -p $workDir$dataSet'/R_file/reduce'
            mkdir -p $workDir$dataSet'/sh_file/reduce'

            Rname=$workDir$dataset/R_file/reduce/reduce_$i.R
            shell_file=$workDir$dataset/sh_file/reduce/reduce_$i.sh

            #Content of R file
            echo 'outDir <- "'$outputDir'"' > $Rname
            echo 'dataSet <- "'${dataset// /'"','"'}'"' >> $Rname
            echo 'alg <- "'$i'"' >> $Rname
            echo 'fname <- "'$fname'"' >> $Rname
            echo 'threshold <- "'$t'"' >> $Rname
            echo 'source("'$workDir'3_Supervised/11_reduce.R")' >> $Rname
            echo 'reduce_supervised(dataSet, alg, outDir, fname, threshold)' >> $Rname

            # Content of shell script
            echo 'Rscript' $Rname > $shell_file

            chmod +x $shell_file

            if command -v qsub &>/dev/null; then
                # execute shell_file to cluster
                echo "Using: $shell_file"
                file_to_submit+=($shell_file)
            else
                bash $shell_file
            fi
        done
    done
done


if command -v qsub &>/dev/null; then
    ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi