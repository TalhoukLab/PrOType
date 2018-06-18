#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. Parameters.sh
## Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data set cannot be empty"
#		 exit 1
#fi
#
## number of bootstrap replications
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#		 exit 1
#fi
#
## specified working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#		 exit 1
#fi
#
## specified log directory to output log files
#if [ "$logDir" = "" ]
#then echo "Log directory must be specified"
#		 exit 1
#fi


##################################################
############# Submit jobs to cluster #############
##################################################
file_to_submit=()
for dataset in "${dataSets[@]}"; do

    for s in `seq 1 $reps`; do
        for i in "${algs[@]}"; do
            shell_file=$workDir$dataset/sh_file/clust/$i$s.sh
            if command -v qsub &>/dev/null; then
                echo "Using: $shell_file"
                file_to_submit+=($shell_file)
                chmod +x $shell_file
                #if [ $i == rest ]
                #then
                    #qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
                #else
                    #qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
                #fi
            fi
        done
    done

    if command -v qsub &>/dev/null; then
        ./assets/submit_queue.sh
    else
        chmod +x $workDir/1_Unsupervised/submit_local.py
        python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step clust
    fi

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
done