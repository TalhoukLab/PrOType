#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$/R_file/post_processing
mkdir -p $workDir$/sh_file/post_processing

mkdir -p $outputDir/post_processing/fits
mkdir -p $outputDir/post_processing/evals
mkdir -p $outputDir/post_processing/plots
mkdir -p $outputDir/post_processing/predictions

Rname0=$workDir$dataSet/R_file/post_processing/check_baseline.R
Rname1=$workDir$dataSet/R_file/post_processing/plot_iv.R
Rname2=$workDir$dataSet/R_file/post_processing/validate_overlap_c2.R

shname0=$workDir$dataSet/sh_file/post_processing/check_baseline.sh
shname1=$workDir$dataSet/sh_file/post_processing/plot_iv.sh
shname2=$workDir$dataSet/sh_file/post_processing/validate_overlap_c2.sh

echo "Rscript $Rname0" > $shname0
echo "Rscript $Rname1" > $shname1
echo "Rscript $Rname2" > $shname2

chmod +x $shname0
chmod +x $shname1
chmod +x $shname2

echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $Rname0
echo 'outputDir <- "'$outputDir'"' >> $Rname0
echo 'dataDir <- "assets/data"' >> $Rname0

echo 'cli::cat_line("Validating baseline results")' >> $Rname0
echo 'source("R/3-post_processing/0-validate_baseline_results.R")' >> $Rname0

echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $Rname1
echo 'outputDir <- "'$outputDir'"' >> $Rname1

echo 'cli::cat_line("Post-processing 1: internal validity plots")' >> $Rname1
echo 'source("R/3-post_processing/1-internal_validity_plots.R")' >> $Rname1

echo 'trainSet <- "'$trainSet'"' > $Rname2
echo 'testSet <- "'$testSet'"' >> $Rname2
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname2
echo 'outputDir <- "'$outputDir'"' >> $Rname2
echo 'dataDir <- "assets/data"' >> $Rname2

echo 'cli::cat_line("Post-processing 2: evaluating batch effects")' >> $Rname2
echo 'source("R/3-post_processing/2-evaluate_batch_effects.R")' >> $Rname2
echo 'cli::cat_line("Post-processing 3: fitting top models")' >> $Rname2
echo 'source("R/3-post_processing/3-fit_top_models.R")' >> $Rname2
echo 'cli::cat_line("Post-processing 4: validating overlap array")' >> $Rname2
echo 'source("R/3-post_processing/4-validate_array.R")' >> $Rname2
echo 'cli::cat_line("Post-processing 5: predict on cut2")' >> $Rname2
echo 'source("R/3-post_processing/5-predict_C2.R")' >> $Rname2
echo 'cli::cat_line("Post-processing 6: probe mapping on cut2")' >> $Rname2
echo 'source("R/3-post_processing/6-probe_mapping_C2v2.R")' >> $Rname2
echo 'cli::cat_line("Post-processing 7: mapping signatures on cut2")' >> $Rname2
echo 'source("R/3-post_processing/7-mapping_signatures_C2.R")' >> $Rname2

file_to_submit=($shname0 $shname1 $shname2)
echo -e "$GREEN_TICK Added to queue: $shname0"
echo -e "$GREEN_TICK Added to queue: $shname1"
echo -e "$GREEN_TICK Added to queue: $shname2"

if command -v qsub &>/dev/null; then
  :
else
  python assets/submit_local.py --num_parallel 4 --file_location $workDir --step post_processing
fi

logDir=$baseLogDir'/post_processing'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
