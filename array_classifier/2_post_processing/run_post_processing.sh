#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/post_processing

Rname=$workDir$dataSet/R_file/post_processing/post_processing.R
rm -f $Rname

for dataset in "${dataSets[@]}"; do
    cp -r $outputDir$dataset/data_pr_$dataset/ $outputDir/iv_summaries/data_pr_$dataset/
done

mkdir -p $outputDir/fits
mkdir -p $outputDir/evals
mkdir -p $outputDir/plots
mkdir -p $outputDir/predictions

echo 'for (dataset in unlist(strsplit("'"${dataSets[*]}"'", " "))) {' > $Rname
    echo 'cat("Starting Part 0\n")' >> $Rname
    echo 'cat(dataset, "\n")' >> $Rname
    echo 'output_dir <- "'$outputDir'/iv_summary"' >> $Rname
    echo 'source("array_classifier/2_post_processing/step0_InternalValidation.R")' >> $Rname
echo '}' >> $Rname
echo 'datasets <- unlist(strsplit("'"${dataSets[*]}"'", " "))' >> $Rname
echo 'algs <- purrr::set_names(unlist(strsplit("'"${classificationAlgs[*]}"'", " ")))' >> $Rname
echo 'output_dir <- "'$outputDir'"' >> $Rname
echo 'cat("Starting Part 1\n")' >> $Rname
echo 'source("array_classifier/2_post_processing/step1_BuildFinalModel.R")' >> $Rname
echo 'cat("Starting Part 2\n")' >> $Rname
echo 'source("array_classifier/2_post_processing/step2_ArrayValidation.R")' >> $Rname

Rscript $Rname