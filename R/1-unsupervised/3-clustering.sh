#!/bin/bash

. ./Parameters.sh

file_to_submit=()

# Make directories for R script, shell script
subDir=unsupervised/clustering
R_dir=$workDir/R_file/$subDir
sh_dir=$workDir/sh_file/$subDir

for dataset in "${dataSets[@]}"; do
    # Make job and output directories for dataset
    mkdir -p $R_dir/$dataset
    mkdir -p $sh_dir/$dataset
    mkdir -p $outputDir/$subDir'/raw_clust/'$dataset
    mkdir -p $outputDir/$subDir'/imputed_clust/'$dataset

    for s in `seq 1 $reps`; do
        for alg in "${algs[@]}"; do
            # Content of R file
            R_file=$R_dir/$dataset/clustering_$alg$s.R
            echo 'k <- '$k > $R_file
            echo 's <- '$s >> $R_file
            echo 'alg <- "'$alg'"' >> $R_file
            echo 'dataset <- "'$dataset'"' >> $R_file
            echo 'outputDir <- "'$outputDir'"' >> $R_file
            echo 'shouldCompute <- '$shouldCompute >> $R_file
            echo 'source("R/1-unsupervised/3-clustering.R")' >> $R_file

            # Content of sh file
            sh_file=$sh_dir/$dataset/clustering_$alg$s.sh
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
        python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step clust
    fi
done

# Submit to queue if qsub exists
logDir=$baseLogDir/$subDir
outputDir="$outputDir/$subDir/raw_clust $outputDir/$subDir/imputed_clust"
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
fi
