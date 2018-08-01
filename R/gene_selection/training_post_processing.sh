#!/bin/bash

. ./Parameters.sh

#File names for R script, rds output file, shell job script
mkdir -p $workDir$dataset/R_file/geneselection_post_processing
mkdir -p $workDir$dataset/sh_file/geneselection_post_processing

file_to_submit=()
echo "Processing Evaluate Predictions"
R_train=$workDir/R_file/geneselection_post_processing/eval_preds.R
sh_train=$workDir/sh_file/geneselection_post_processing/eval_preds.sh

gene_selection_algs=${geneSelectionAlgs// /'"','"'}

#Content of R file
echo 'algs <- c("'$gene_selection_algs'")' > $Rname
echo 'shouldCompute <- '$shouldCompute >> $R_train
echo 'source("R/gene_selection/evaluate_predictions.R")' >> $R_train

# contents of sh file
echo 'Rscript' $R_train > $sh_train

chmod +x $sh_train

if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_train"
  file_to_submit+=($sh_train)
else
  bash $sh_train
fi

echo "Processing Summary Frequencies"
R_train=$workDir/R_file/geneselection_post_processing/summary_freq.R
sh_train=$workDir/sh_file/geneselection_post_processing/summary_freq.sh

gene_selection_algs=${geneSelectionAlgs// /'"','"'}

#Content of R file
echo 'algs <- c("'$gene_selection_algs'")' > $Rname
echo 'shouldCompute <- '$shouldCompute >> $R_train
echo 'source("R/gene_selection/summary_frequencies.R")' >> $R_train

# contents of sh file
echo 'Rscript' $R_train > $sh_train

chmod +x $sh_train

if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_train"
  file_to_submit+=($sh_train)
else
  bash $sh_train
fi

shouldWait=FALSE
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
