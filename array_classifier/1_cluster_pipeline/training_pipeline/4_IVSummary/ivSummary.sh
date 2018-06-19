#!/bin/bash

. ./Parameters.sh

## specify the working directory
#if [ "$user" = "" ]
#then echo "User must be specified"
#	exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify the working directory
#if [ "$inputDir" = "" ]
#then echo "Input directory must be specified"
#	exit 1
#fi
#
## specify the output directory
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify log directory
#if [ "$logDir" = "" ]
#then echo "Log directory must be specified"
#                 exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi


# *************************************************************************
# Step 1: iterate through each study and output internal validation tables
# *************************************************************************
qsubJobArray=()

for i in "${dataSets[@]}"; do
	# check if data set was specified
	if [ "$i" = "" ]; then
		echo "Data set must be specified."
		exit 1
	fi

	# create scripts and directories
	mkdir -p $workDir$i'/R_file/iv_summary'
	mkdir -p $workDir$i'/sh_file/iv_summary'

	Rname=$workDir$i'/R_file/iv_summary/iv.R'
	shname=$workDir$i'/sh_file/iv_summary/iv.sh'

	touch $Rname
	echo 'fdir <- "'$outputDir$i'"' > $Rname
	echo 'sdir <- "'$outputDir$i'/data_pr_'$i'/iv_summary_'$i'.rds"' >> $Rname
	echo 'source("'$workDir'4_IVSummary/ivSummary.R")' >> $Rname

    echo 'sdir <- "'$outputDir$i'/data_pr_'$i'/iv_summary_'$i'_threshold.rds"' >> $Rname
	echo 'source("'$workDir'4_IVSummary/ivSummary.R")' >> $Rname


	touch $shname
	echo '#!/bin/bash' > $shname
	echo 'export PATH='$RPath':$PATH' >> $shname
	echo 'Rscript' $Rname >> $shname
	chmod +x $shname

	# execute shell script to queue
  if command -v qsub &>/dev/null; then
		  echo "Using: $shname"
	    qsubJobArray+=($(qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shname))
  else
      echo "Making File"
      bash +x $shname
  fi
done


# *************************************************************************
# Step 2: wait until cluster jobs are complete until proceeding
# *************************************************************************
temp=$outputDir'temp/'
mkdir -p $temp

containsElement () {
	local e
	for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
	return 1
}

test=0
if command -v qsub &>/dev/null; then
  test=0
else
  test=1
fi

while [ $test == 0 ]; do
	tempf=$temp'qstat_temp.txt'
	touch $tempf
	qstat -u $user | awk '{print $1}' > $tempf
	IFS=$'\r\n' GLOBIGNORE='*' command eval  'qstatJobArray=($(cat '$tempf'))'

	for i in `seq 1 ${#qsubJobArray[@]}`; do
		containsElement "${qsubJobArray[${i}]}" "${qstatJobArray[@]}"
		test=$?
		if [[ $test == 0 ]]; then
			echo "Jobs are still running. Please wait..."
			sleep 20s
			break
		fi
	done
	if [[ $test == 1 ]]; then
		test=1
		rm $tempf
		echo "jobs completed! Combining all IV tables..."
	fi
done


# *************************************************************************
# Step 3: combine all IV tables into one
# *************************************************************************
ivSummary=$outputDir'/iv_summary'
mkdir -p $ivSummary

for i in "${dataSets[@]}"; do
	cp -r $outputDir$i'/data_pr_'$i $ivSummary
done

# build R script


# combine all iv tables
export PATH=$RPath:$PATH
Rscript $workDir'4_IVSummary/ivCombine.R' $ivSummary

