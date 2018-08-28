#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/post_processing
mkdir -p $workDir$dataSet/sh_file/post_processing

mkdir -p $outputDir/fits
mkdir -p $outputDir/evals
mkdir -p $outputDir/plots
mkdir -p $outputDir/predictions

Rname0=$workDir$dataSet/R_file/post_processing/baseline_validity.R
Rname1=$workDir$dataSet/R_file/post_processing/evaluate_batch.R
Rname2=$workDir$dataSet/R_file/post_processing/interval_validity.R
Rname3=$workDir$dataSet/R_file/post_processing/validate_overlap.R
Rname4=$workDir$dataSet/R_file/post_processing/mapping_predict.R

shname0=$workDir$dataSet/sh_file/post_processing/baseline_validity.sh
shname1=$workDir$dataSet/sh_file/post_processing/evaluate_batch.sh
shname2=$workDir$dataSet/sh_file/post_processing/interval_validity.sh
shname3=$workDir$dataSet/sh_file/post_processing/validate_overlap.sh
shname4=$workDir$dataSet/sh_file/post_processing/mapping_predict.sh

echo "Rscript $Rname0" > $shname0
echo "Rscript $Rname1" > $shname1
echo "Rscript $Rname2" > $shname2
echo "Rscript $Rname3" > $shname3
echo "Rscript $Rname4" > $shname4

chmod +x $shname0
chmod +x $shname1
chmod +x $shname2
chmod +x $shname3
chmod +x $shname4

echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $Rname0
echo 'outputDir <- "'$outputDir'"' >> $Rname0
echo 'dataDir <- "assets/data"' >> $Rname0

echo 'cli::cat_line("Validating baseline results")' >> $Rname0
echo 'source("R/5_post_processing/0-validate_baseline_results.R")' >> $Rname0

echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $Rname1
echo 'outputDir <- "'$outputDir'"' >> $Rname1
echo 'dataDir <- "assets/data"' >> $Rname1

echo 'cli::cat_line("Post-processing 1: evaluating batch effects")' >> $Rname1
echo 'source("R/5_post_processing/1-evaluate_batch_effects.R")' >> $Rname1

echo 'datasets <- c("'$trainSet'", "'$trainSet2'")' > $Rname2
echo 'outputDir <- "'$outputDir'"' >> $Rname2

echo 'cli::cat_line("Post-processing 2: internal validity plots")' >> $Rname2
echo 'source("R/5_post_processing/2-internal_validity_plots.R")' >> $Rname2

echo 'trainSet <- "'$trainSet'"' > $Rname3
echo 'outputDir <- "'$outputDir'"' >> $Rname3
echo 'dataDir <- "assets/data"' >> $Rname3

echo 'cli::cat_line("Post-processing 3: fitting top models")' >> $Rname3
echo 'source("R/5_post_processing/3-fit_top_models.R")' >> $Rname3
echo 'cli::cat_line("Post-processing 4: validating overlap array")' >> $Rname3
echo 'source("R/5_post_processing/4-validate_array.R")' >> $Rname3

echo 'trainSet <- "'$trainSet'"' > $Rname4
echo 'testSet <- "'$testSet'"' >> $Rname4
echo 'outputDir <- "'$outputDir'"' >> $Rname4
echo 'dataDir <- "assets/data"' >> $Rname4

echo 'cli::cat_line("Post-processing 5: predict on cut2")' >> $Rname4
echo 'source("R/5_post_processing/5-predict_C2.R")' >> $Rname4
echo 'cli::cat_line("Post-processing 6: probe mapping on cut2")' >> $Rname4
echo 'source("R/5_post_processing/6-probe_mapping_C2.R")' >> $Rname4
echo 'cli::cat_line("Post-processing 7: mapping signatures on cut2")' >> $Rname4
echo 'source("R/5_post_processing/7-mapping_signatures_C2.R")' >> $Rname4

file_to_submit=($shname0 $shname1 $shname2 $shname3 $shname4)
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
