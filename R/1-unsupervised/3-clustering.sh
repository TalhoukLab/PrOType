#!/bin/bash

. Parameters.sh

##################################################
############# Submit jobs to cluster #############
##################################################
file_to_submit=()

for dataset in "${dataSets[@]}"; do
    mkdir -p $workDir/R_file/clust/$dataset
    mkdir -p $workDir/sh_file/clust/$dataset

    mkdir -p $outputDir'/unsupervised/clustering/rds_out_'$dataset
    mkdir -p $outputDir'/unsupervised/clustering/imputed_clust_'$dataset
    mkdir -p $outputDir'/unsupervised/clustering/con_mat_'$dataset

    for s in `seq 1 $reps`; do
        for i in "${algs[@]}"; do
            # File names for R script, rds output file, shell job script

            R_clust=$workDir/R_file/clust/$dataset/$i$s.R
            sh_clust=$workDir/sh_file/clust/$dataset/$i$s.sh

            # Content of R file
            echo 'k <-'$k > $R_clust
            echo 's <-'$s >> $R_clust
            echo 'algs <- "'$i'"' >> $R_clust
            echo 'pr <- "cs"' >> $R_clust
            echo 'dataset <- "'$dataset'"' >> $R_clust
            echo 'outputdir <- "'$outputDir'"' >> $R_clust
            echo 'shouldCompute <- '$shouldCompute >> $R_clust
            echo 'source("R/1-unsupervised/3-clustering.R")' >> $R_clust

            # Contents of sh file
            echo 'Rscript' $R_clust > $sh_clust

            chmod +x $sh_clust

            if command -v qsub &>/dev/null; then
                echo "Adding To Queue: $sh_clust"
                file_to_submit+=($sh_clust)
            fi

            chmod +x $sh_clust
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
        chmod +x assets/submit_local.py
        python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step clust
    fi
done

logDir=$baseLogDir'/unsupervised/clustering'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
