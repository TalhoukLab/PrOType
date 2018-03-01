#!/bin/sh

##################################################
############# Input Paremeters for Run ###########
##################################################

. 1_Unsupervised/Parameters.sh

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

# specify algorithm batches
algs=(nmfbrunet nmflee distalgs rest)

# specify minimum number of reps required for merge.
# the modulus of the dividend (reps) and divisor (c) must be greater or equal to zero.
c=100


##################################################
############# Submit jobs to cluster #############
##################################################

for s in `seq $c $c $reps`; do
	for i in "${algs[@]}"; do
		# execute shell_file to cluster
		shell_file=$workDir$dataSet/sh_file/merge/Merge_$i$s.sh
		qsub -V -p -1 -l mem_free=25G -l mem_token=20G -l h_vmem=35G -e $logDir -o $logDir -q all.q $shell_file
	done
done

# complete
echo 'Submitted merge files to the queue!'
