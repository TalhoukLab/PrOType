#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/predictions
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir

# Loop over studies
studies=(`Rscript R/5-gene_selection/get_studies.R`)
for study in "${studies[@]}"; do
    # Content of R file
    R_file=$R_dir/predict_$study.R
    echo 'study <- "'$study'"' > $R_file
    echo 'algs <- strsplit("'${geneSelectionAlgs[@]}'", " ")[[1]]' >> $R_file
    echo 'B <- '$numBootstraps >> $R_file
    echo 'shouldCompute <- '$shouldCompute >> $R_file
    echo 'outputDir <- "'$outputDir'"' >> $R_file
    echo 'source("R/5-gene_selection/4-make_predictions.R")' >> $R_file

    # Content of sh file
    sh_file=$sh_dir/predict_$study.sh
    echo 'Rscript' $R_file > $sh_file
    chmod +x $sh_file

    # Add to queue if qsub exists
    if command -v qsub &>/dev/null; then
        file_to_submit+=($sh_file)
        echo -e "$GREEN_TICK Added to queue: $sh_file"
    fi
done

# Submit to queue if qsub exists, to python otherwise
logDir=$baseLogDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step geneselection_predict
fi
