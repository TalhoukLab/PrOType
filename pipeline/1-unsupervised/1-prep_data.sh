#!/bin/bash

. ./Parameters.sh

echo 'Preparing data'

# Make directories for R script, shell script
subDir=unsupervised/prep_data
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    datadir=$outputDir/$subDir/$dataset
    mkdir -p $datadir

    # Content of R file
    R_file=$RSubDir/$dataset/prep_data.R
    echo 'pr <- "cs"' > $R_file
    echo 'dataset <- "'$dataset'"' >> $R_file
    echo 'datadir <- "'$datadir'"' >> $R_file
    echo 'dpath <- "'$inputDir'"' >> $R_file
    echo 'source("pipeline/1-unsupervised/1-prep_data.R")' >> $R_file

    # Content of sh file
    sh_file=$shSubDir/$dataset/prep_data.sh
    echo "Rscript $R_file" > $sh_file
    chmod +x $sh_file
    $sh_file

    echo -e "$GREEN_TICK Data preparation completed for dataset $dataset"
done

echo -e "$GREEN_TICK All data preparation completed"
