#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=gene_selection/boot_freq
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir
mkdir -p $RSubDir
mkdir -p $shSubDir
mkdir -p $outputDir/$subDir

# Loop over studies
studies=(`Rscript pipeline/5-gene_selection/get_studies.R`)
for study in "${studies[@]}"; do
    for alg in "${geneSelectionAlgs[@]}"; do
        # Content of R file
        R_file=$RSubDir/boot_${study}_${alg}.R
        echo 'study <- "'$study'"' > $R_file
        echo 'alg <- "'$alg'"' >> $R_file
        echo 'B <- '$numBootstraps >> $R_file
        if [[ $alg == "lasso" ]]; then
            echo 'seed_alg <- NULL' >> $R_file
        elif [[ $alg == "rf" ]]; then
            echo 'seed_alg <- 2018' >> $R_file
            echo 'packrat::off()' >> $R_file
        else
            echo 'seed_alg <- 2018' >> $R_file
        fi
        echo 'shouldCompute <- '$shouldCompute >> $R_file
        echo 'outputDir <- "'$outputDir'"' >> $R_file
        echo 'source("pipeline/5-gene_selection/1-boot_freq.R")' >> $R_file

        # Content of sh file
        sh_file=$shSubDir/boot_${study}_${alg}.sh
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
logDir=$logDir/$subDir
outputDir=$outputDir/$subDir
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $scriptDir --step gs_boot_freq
fi
