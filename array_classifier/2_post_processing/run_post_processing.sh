#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/post_processing
mkdir -p $outputDir/fits
mkdir -p $outputDir/evals
mkdir -p $outputDir/plots
mkdir -p $outputDir/predictions

Rname=$workDir$dataSet/R_file/post_processing/post_processing.R

echo 'trainSet <- "'$trainSet'"' > $Rname
echo 'trainSet2 <- "'$trainSet2'"' >> $Rname
echo 'testSet <- "'$testSet'"' >> $Rname
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname
echo 'outputDir <- "'$outputDir'"' >> $Rname
echo 'dataDir <- "assets/data"' >> $Rname
ppDir="array_classifier/2_post_processing/"

echo 'cli::cat_line("Validating Results")' >> $Rname
echo 'source("'$ppDir'0-validate_baseline_results.R")' >> $Rname

echo 'cli::cat_line("Post-processing 1: evaluating batch effects")' >> $Rname
echo 'source("'$ppDir'1-evaluate_batch_effects.R")' >> $Rname

echo 'cli::cat_line("Post-processing 2: internal validity plots")' >> $Rname
echo 'source("'$ppDir'2-internal_validity_plots.R")' >> $Rname

echo 'cli::cat_line("Post-processing 3: fitting top models")' >> $Rname
echo 'source("'$ppDir'3-fit_top_models.R")' >> $Rname

echo 'cli::cat_line("Post-processing 4: validating overlap array")' >> $Rname
echo 'source("'$ppDir'4-validate_array")' >> $Rname

echo 'cli::cat_line("Post-processing 5: predict on cut2")' >> $Rname
echo 'source("'$ppDir'5-predict_C2.R")' >> $Rname

echo 'cli::cat_line("Post-processing 6: probe mapping on cut2")' >> $Rname
echo 'source("'$ppDir'6-probe_mapping_C2v2.R")' >> $Rname

echo 'cli::cat_line("Post-processing 7: mapping signatures on cut2")' >> $Rname
echo 'source("'$ppDir'7-mapping_signatures_C2.R")' >> $Rname

Rscript $Rname
