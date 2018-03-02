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

# algorithms to execute
algs=(nmfbrunet nmflee distalgs rest)


##################################################
############# Submit jobs to cluster #############
##################################################

for s in `seq 1 $reps`; do
	for i in "${algs[@]}"; do
		shell_file=$workDir$dataSet/sh_file/clust/$i$s.sh
		if [ $i == rest ]
		  then
		     qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
		  else
		     qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shell_file
		fi
	done
done

