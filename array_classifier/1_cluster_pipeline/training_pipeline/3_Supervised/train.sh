#!/bin/bash

. ./Parameters.sh

##Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#		 exit 1
#fi
#
## specify number of bootstrap reps
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#		 exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify log directory
#if [ "$logDir" = "" ]
#then echo "Log directory must be specified"
#		 exit 1
#fi

# specify algs
algs=(first second third fourth) # ldaRfe qdaRfe rfRfe lassoRfe svmRfe)
for dataset in "${dataSets[@]}"; do
    for s in `seq 1 $reps`; do
        for i in "${algs[@]}"; do
            for t in "${threshold[@]}"; do
                shell_file=$workDir$dataset/sh_file/train/$i$s$t.sh
                if command -v qsub &>/dev/null; then
                    echo "Using: $shell_file"
                    qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
                else
                  chmod +x $shell_file
                fi
            done
        done
    done
done


if command -v qsub &>/dev/null; then
    echo "skipping"
else
    for dataset in "${dataSets[@]}"; do
        python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataset --step train
    done
fi

echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
echo "The logs can be found in \"${logDir}\""
