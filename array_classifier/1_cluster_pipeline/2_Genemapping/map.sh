#!/bin/bash

. ./Parameters.sh

#************************************************
#************* create mapping scripts ***********
#************************************************
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    mkdir -p $workDir$dataset/R_file/map
    mkdir -p $workDir$dataset/sh_file/map
    
    R_map=$workDir$dataset/R_file/map/map.R
    shell_file=$workDir$dataset/sh_file/map/map.sh

    # create R script
    echo "source('"$workDir"2_Genemapping/GeneMapping.R')" > $Rname
    echo "tdat <- readr::read_rds('"$outputDir$dataset"/data_pr_"$dataset"/tdat_"$dataset".rds')" >> $Rname
    echo "x <- data.frame(t(tdat))" >> $Rname
    echo "dataset <- '"$dataset"'" >> $Rname
    echo "inDir <- '"$workDir"2_Genemapping/'" >> $Rname
    echo "outDir <- '"$outputDir"'" >> $Rname
    echo "map_to_nano(x, dataset, inDir, outDir)" >> $Rname

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
    ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi