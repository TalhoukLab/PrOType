#!/bin/bash

. ./Parameters.sh

echo "Getting Studies"
Rscript R/5-gene_selection/get_studies.R $outputDir/GeneSelection/tmp/studies.txt

# Make directories for R script, shell script
R_dir=$workDir/R_file/gene_selection/final_training
sh_dir=$workDir/sh_file/gene_selection/final_training
mkdir -p $R_dir
mkdir -p $sh_dir

# Loop over studies
file_to_submit=()
while read study; do
    for alg in "${geneSelectionAlgs[@]}"; do
        # Content of R file
        R_train=$R_dir/train_${study}_$alg.R
        echo 'study <- "'$study'"' > $R_train
        echo 'alg <- "'$alg'"' >> $R_train
        echo 'B <- '$numBootstraps >> $R_train
        echo 'shouldCompute <- '$shouldCompute >> $R_train
        echo 'outputDir <- "'$outputDir'"' >> $R_train
        echo 'source("R/5-gene_selection/3-run_final_training.R")' >> $R_train

        # Content of sh file
        sh_train=$sh_dir/train_${study}_$alg.sh
        echo 'Rscript' $R_train > $sh_train
        chmod +x $sh_train

        # Add to queue if qsub exists
        if command -v qsub &>/dev/null; then
            file_to_submit+=($sh_train)
            echo -e "$GREEN_TICK Added to queue: $sh_train"
        fi
    done
done < "$outputDir/GeneSelection/tmp/studies.txt"

# Submit to queue if qsub exists, to python otherwise
logDir=$baseLogDir'/gene_selection/final_training'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step gene_selection_final_training
fi
