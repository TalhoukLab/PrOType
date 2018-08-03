#!/bin/bash

. ./Parameters.sh

##################################################
############# Submit jobs to cluster #############
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do
    mkdir -p $workDir$dataset/R_file/merge
    mkdir -p $workDir$dataset/sh_file/merge

    mkdir -p $outputDir$dataset'/con_mat_merged_'$dataset

    for s in `seq $c $c $reps`; do
        for i in "${algs[@]}"; do
            # file names
            R_merge=$workDir$dataset/R_file/merge/Merge_$i$s.R
            sh_merge=$workDir$dataset/sh_file/merge/Merge_$i$s.sh

            # Content of R file
            echo 'ndat<- "'$dataset'"' > $R_merge
            echo 'dir <- "'$outputDir$dataset'"' >> $R_merge
            echo 'algs<- "'$i'"' >> $R_merge
            echo 'c <- '$c >> $R_merge
            echo 'r <- '$r >> $R_merge
            echo 'reps <- '$reps >> $R_merge
            echo 'k <- '$k >> $R_merge
            echo 'shouldCompute <- '$shouldCompute >> $R_merge
            echo 'source("R/unsupervised/merge_partial_consmat.R")' >> $R_merge

            # Content of sh file
            echo 'Rscript' $R_merge > $sh_merge

            chmod +x $sh_merge

            if command -v qsub &>/dev/null; then
                # execute shell_file to cluster
                echo "Adding To Queue: $sh_merge"
                file_to_submit+=($sh_merge)
            fi

            chmod +x $sh_merge
        done
    done

    if command -v qsub &>/dev/null; then
        :
    else
      python assets/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step merge
    fi
    # complete
    echo 'Submitted merge files to the queue!'
done

logDir=$baseLogDir'/unsupervised/CMmerge'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
