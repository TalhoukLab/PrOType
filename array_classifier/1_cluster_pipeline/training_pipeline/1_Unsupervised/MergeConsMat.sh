#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

## Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data set cannot be empty"
#		 exit 1
#fi
#
## Set number of bootstrap replications
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#		 exit 1
#fi
#
## specify working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#		 exit 1
#fi
#
## specify log directory
#if [ "$logDir" = "" ]
#then echo "Log directory must be specified"
#		 exit 1
#fi

# specify minimum number of reps required for merge.
# the modulus of the dividend (reps) and divisor (c) must be greater or equal to zero.
c=100


##################################################
############# Submit jobs to cluster #############
##################################################
for dataset in "${dataSets[@]}"; do
    for s in `seq $c $c $reps`; do
        for i in "${algs[@]}"; do
            shell_file=$workDir$dataset/sh_file/merge/Merge_$i$s.sh
            echo "Using: $shell_file"
            if command -v qsub &>/dev/null; then
                # execute shell_file to cluster
                qsub -V -p -1 -l mem_free=20G -l mem_token=20G -l h_vmem=30G -e $logDir -o $logDir -q all.q $shell_file
            else
                echo "Submitting Locally"
                chmod +x $shell_file
            fi
        done
    done

    if command -v qsub &>/dev/null; then
        echo "Using Shell"
    else
      python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step merge
    fi
    # complete
    echo 'Submitted merge files to the queue!'
done