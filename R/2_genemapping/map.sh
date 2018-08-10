#!/bin/bash

. ./Parameters.sh

#************************************************
#************* create mapping scripts ***********
#************************************************
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir/R_file/map/$dataset
    mkdir -p $workDir/sh_file/map/$dataset

    Rname=$workDir/R_file/map/$datasetmap.R
    shell_file=$workDir/sh_file/map/$dataset/map.sh

    mkdir -p $outputDir'/genemapping/'$dataset

    # create R script
    echo "source('R/2_genemapping/GeneMapping.R')" > $Rname
    echo "dataset <- '"$dataset"'" >> $Rname
    echo "outDir <- '"$outputDir"'" >> $Rname
    echo 'shouldCompute <- '$shouldCompute >> $Rname
    echo "map_to_nano(dataset, outDir, shouldCompute)" >> $Rname

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


shouldWait=FALSE
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
