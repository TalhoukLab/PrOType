#!/bin/bash

. ./Parameters.sh

echo "Getting Studies"
Rscript R/6_gene_selection/get_studies.R $outputDir/GeneSelection/tmp/studies.txt

#File names for R script, rds output file, shell job script
mkdir -p $workDir$dataset/R_file/gene_selection_final_training
mkdir -p $workDir$dataset/sh_file/gene_selection_final_training

file_to_submit=()
while read study; do
    for alg in "${geneSelectionAlgs[@]}"; do
        R_train=$workDir/R_file/gene_selection_final_training/train_${study}_$alg.R
        sh_train=$workDir/sh_file/gene_selection_final_training/train_${study}_$alg.sh

        #Content of R file
        echo 'study <- "'$study'"' > $R_train
        echo 'alg <- "'$alg'"' >> $R_train
        echo 'B <- '$numBootstraps >> $R_train
        echo 'shouldCompute <- '$shouldCompute >> $R_train
        echo 'outputDir <- "'$outputDir'"' >> $R_train
        echo 'source("R/6_gene_selection/run_final_training.R")' >> $R_train

        # contents of sh file
        echo 'Rscript' $R_train > $sh_train

        if command -v qsub &>/dev/null; then
            echo "Adding To Queue: $sh_train"
            file_to_submit+=($sh_train)
            chmod +x $sh_train
        fi
    done
done < "$outputDir/GeneSelection/tmp/studies.txt"

if command -v qsub &>/dev/null; then
    :
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step gene_selection_final_training
fi

logDir=$baseLogDir'/gene_selection/final_training'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi