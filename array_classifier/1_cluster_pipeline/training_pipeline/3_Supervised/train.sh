#!/bin/bash

. 3_Supervised/Parameters.sh

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

for s in `seq 1 $reps`; do
	for i in "${algs[@]}"; do
		shell_file=$workDir$dataSet/sh_file/train/$i$s.sh
		if command -v qsub &>/dev/null; then
		    echo "Using: $shell_file"
		    qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
		else
		  chmod +x $shell_file
		fi
	done
done


if command -v qsub &>/dev/null; then
    echo "skipping"
else
    python $workDir/1_Unsupervised/submit_local.py --num_parallel 4 --file_location $workDir$dataSet --step train
fi

echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
echo "The logs can be found in \"${logDir}\""
