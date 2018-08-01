#!/bin/bash

. ./Parameters.sh

echo "Getting Studies"
Rscript R/gene_selection/get_studies.R

#File names for R script, rds output file, shell job script
mkdir -p $workDir$dataset/R_file/geneselection_predict
mkdir -p $workDir$dataset/sh_file/geneselection_predict

file_to_submit=()
while read study; do
    R_train=$workDir/R_file/geneselection_predict/predict_$study.R
    sh_train=$workDir/sh_file/geneselection_predict/predict_$study.sh

    gene_selection_algs=${geneSelectionAlgs// /'"','"'}

    #Content of R file
    echo 'study <- "'$study'"' > $R_train
    echo 'algs <- c("'$gene_selection_algs'")' >> $Rname
    echo 'B <- '$numBootstraps >> $R_train
    echo 'shouldCompute <- '$shouldCompute >> $R_train
    echo 'source("R/gene_selection/make_predictions.R")' >> $R_train

    # contents of sh file
    echo 'Rscript' $R_train > $sh_train

    if command -v qsub &>/dev/null; then
        echo "Adding To Queue: $sh_train"
        file_to_submit+=($sh_train)
        chmod +x $sh_train
    fi
done < "$outputDir/GeneSelection/tmp/studies.txt"

if command -v qsub &>/dev/null; then
    :
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step geneselection_predict
fi

if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
