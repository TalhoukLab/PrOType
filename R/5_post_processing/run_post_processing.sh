#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/post_processing
mkdir -p $workDir$dataSet/sh_file/post_processing

mkdir -p $outputDir/fits
mkdir -p $outputDir/evals
mkdir -p $outputDir/plots
mkdir -p $outputDir/predictions

Rname0=$workDir$dataSet/R_file/post_processing/baseline_validity.R
Rname1=$workDir$dataSet/R_file/post_processing/evalute_batch.R
Rname2=$workDir$dataSet/R_file/post_processing/interval_validity.R
Rname3=$workDir$dataSet/R_file/post_processing/mapping_predict.R


shname0=$workDir$dataSet/sh_file/post_processing/baseline_validity.sh
shname1=$workDir$dataSet/sh_file/post_processing/evalute_batch.sh
shname2=$workDir$dataSet/sh_file/post_processing/interval_validity.sh
shname3=$workDir$dataSet/sh_file/post_processing/mapping_predict.sh

echo "Rscript $Rname0" > $shname0
echo "Rscript $Rname1" > $shname1
echo "Rscript $Rname2" > $shname2
echo "Rscript $Rname3" > $shname3

chmod +x $shname0
chmod +x $shname1
chmod +x $shname2
chmod +x $shname3


echo 'trainSet <- "'$trainSet'"' > $Rname0
echo 'trainSet2 <- "'$trainSet2'"' >> $Rname0
echo 'testSet <- "'$testSet'"' >> $Rname0
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname0
echo 'outputDir <- "'$outputDir'"' >> $Rname0
echo 'dataDir <- "assets/data"' >> $Rname0

echo 'cli::cat_line("Validating Results")' >> $Rname0
echo 'source("R/5_post_processing/validate_baseline_results.R")' >> $Rname0


echo 'trainSet <- "'$trainSet'"' > $Rname1
echo 'trainSet2 <- "'$trainSet2'"' >> $Rname1
echo 'testSet <- "'$testSet'"' >> $Rname1
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname1
echo 'outputDir <- "'$outputDir'"' >> $Rname1
echo 'dataDir <- "assets/data"' >> $Rname1

echo 'cli::cat_line("Post-processing step 1")' >> $Rname1
echo 'source("R/5_post_processing/evaluate_batch_effects.R")' >> $Rname1

echo 'trainSet <- "'$trainSet'"' > $Rname2
echo 'trainSet2 <- "'$trainSet2'"' >> $Rname2
echo 'testSet <- "'$testSet'"' >> $Rname2
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname2
echo 'outputDir <- "'$outputDir'"' >> $Rname2
echo 'dataDir <- "assets/data"' >> $Rname2

echo 'cli::cat_line("Post-processing step 2")' >> $Rname2
echo 'source("R/5_post_processing/internal_validity_plots.R")' >> $Rname2

echo 'trainSet <- "'$trainSet'"' > $Rname3
echo 'trainSet2 <- "'$trainSet2'"' >> $Rname3
echo 'testSet <- "'$testSet'"' >> $Rname3
echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' >> $Rname3
echo 'outputDir <- "'$outputDir'"' >> $Rname3
echo 'dataDir <- "assets/data"' >> $Rname3

echo 'cli::cat_line("Post-processing step 3")' >> $Rname3
echo 'source("R/5_post_processing/predict_C2.R")' >> $Rname3
echo 'cli::cat_line("Post-processing step 4")' >> $Rname3
echo 'source("R/5_post_processing/probe_mapping_C2.R")' >> $Rname3
echo 'cli::cat_line("Post-processing step 5")' >> $Rname3
echo 'source("R/5_post_processing/mapping_signatures_C2.R")' >> $Rname3

file_to_submit=($shname0 $shname1 $shname2 $shname3)
if command -v qsub &>/dev/null; then
  :
else
  python assets/submit_local.py --num_parallel 4 --file_location $workDir --step post_processing
fi

logDir=$baseLogDir'/post_processing'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
