#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/run_bootstrap
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir
mkdir -p $R_dir
mkdir -p $sh_dir
mkdir -p $outputDir/gene_selection/training

# Loop over studies
studies=(`Rscript R/5-gene_selection/get_studies.R`)
for study in "${studies[@]}"; do
    for alg in "${geneSelectionAlgs[@]}"; do
        # Content of R file
        R_file=$R_dir/boot_${study}_${alg}.R
        echo 'study <- "'$study'"' > $R_file
        echo 'alg <- "'$alg'"' >> $R_file
        echo 'B <- '$numBootstraps >> $R_file
        echo 'shouldCompute <- '$shouldCompute >> $R_file
        echo 'outputDir <- "'$outputDir'"' >> $R_file
        echo 'source("R/5-gene_selection/1-run_bootstrap.R")' >> $R_file

        # Content of sh file
        sh_file=$sh_dir/boot_${study}_${alg}.sh
        echo 'Rscript' $R_file > $sh_file
        chmod +x $sh_file

        # Add to queue if qsub exists
        if command -v qsub &>/dev/null; then
            file_to_submit+=($sh_file)
            echo -e "$GREEN_TICK Added to queue: $sh_file"
        fi
    done
done

# Submit to queue if qsub exists, to python otherwise
logDir=$baseLogDir/$subDir
outputDir=$outputDir/
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step gene_selection_bootstrap
fi
