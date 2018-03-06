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

# specify consensus metrics to compute
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)


##################################################
############ Execute jobs on cluster #############
##################################################

for i in "${cons[@]}"; do
	# execute shell_file to cluster node
	shell_file=$workDir$dataSet/sh_file/consensus/Create_$i.sh
	qsub -V -p -1 -l mem_free=25G -l mem_token=25G -l h_vmem=35G -e $logDir -o $logDir -q all.q $shell_file
done

# complete
echo 'Submitted merge files to the queue!'
