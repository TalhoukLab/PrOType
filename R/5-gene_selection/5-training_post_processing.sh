#!/bin/bash

. ./Parameters.sh

echo "Processing Evaluate Predictions"

# Make directories for R script, shell script
R_dir=$workDir/R_file/gene_selection/post_processing
sh_dir=$workDir/sh_file/gene_selection/post_processing
mkdir -p $R_dir
mkdir -p $sh_dir

# Content of R file
R_train=$R_dir/eval_preds.R
echo 'algs <- strsplit("'${geneSelectionAlgs[@]}'", " ")[[1]]' > $R_train
# echo 'algs <- unique(purrr::set_names(unlist(strsplit("'"${geneSelectionAlgs[*]}"'", " "))))' > $R_train
echo 'shouldCompute <- '$shouldCompute >> $R_train
echo 'outputDir <- "'$outputDir'"' >> $R_train
echo 'source("R/5-gene_selection/5-evaluate_predictions.R")' >> $R_train

# Content of sh file
sh_train=$sh_dir/eval_preds.sh
echo 'Rscript' $R_train > $sh_train
chmod +x $sh_train

# Add to queue if qsub exists, submit by bash otherwise
file_to_submit=()
if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_train"
  file_to_submit+=($sh_train)
else
  bash $sh_train
fi

echo "Processing Summary Frequencies"

# Content of R file
R_train=$R_dir/summary_freq.R
echo 'algs <- unique(purrr::set_names(unlist(strsplit("'"${geneSelectionAlgs[*]}"'", " "))))' > $R_train
echo 'shouldCompute <- '$shouldCompute >> $R_train
echo 'outputDir <- "'$outputDir'"' >> $R_train
echo 'source("R/5-gene_selection/5-summary_frequencies.R")' >> $R_train

# Content of sh file
sh_train=$sh_dir/summary_freq.sh
echo 'Rscript' $R_train > $sh_train
chmod +x $sh_train

# Add to queue if qsub exists, submit by bash otherwise
file_to_submit=()
if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_train"
  file_to_submit+=($sh_train)
else
  bash $sh_train
fi

# Submit to queue
logDir=$baseLogDir'/gene_selection/post_processing'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
