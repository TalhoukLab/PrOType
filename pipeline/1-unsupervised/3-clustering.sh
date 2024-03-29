#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/clustering
RSubDir=$RDir/$subDir
shSubDir=$shDir/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $RSubDir/$dataset
    mkdir -p $shSubDir/$dataset
    mkdir -p $outputDir/$subDir'/raw_clust/'$dataset
    mkdir -p $outputDir/$subDir'/imputed_clust/'$dataset

    for s in $(seq -f "%0${#reps}g" 1 $reps); do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$RSubDir/$dataset/clustering_$alg$s.R
            echo 'k <- '$k > $R_file
            echo 's <- "'$s'"' >> $R_file
            echo 'alg <- "'$alg'"' >> $R_file
            echo 'dataset <- "'$dataset'"' >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'source("pipeline/1-unsupervised/3-clustering.R")' >> $R_file

            # Content of sh file
            sh_file=$shSubDir/$dataset/clustering_$alg$s.sh
            echo "Rscript $R_file" > $sh_file
            chmod +x $sh_file

            # Add to queue if qsub exists
            if command -v qsub &>/dev/null; then
                file_to_submit+=($sh_file)
                echo -e "$GREEN_TICK Added to queue: $sh_file"
            fi
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
        python assets/submit_local.py --num_parallel 4 --file_location $scriptDir$dataset --step clust
    fi
done

# Submit to queue if qsub exists
logDir=$logDir/$subDir
outputDir="$outputDir/$subDir/raw_clust $outputDir/$subDir/imputed_clust"
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
