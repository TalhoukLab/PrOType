#!/bin/bash

qsubJobArray=()

for shname in "${file_to_submit[@]}"; do
	# check if data set was specified
	if [ "$shname" = "" ]; then
		echo "script set must be specified."
		exit 1
	fi

	# execute shell script to queue
    if command -v qsub &>/dev/null; then
		  echo "Using: $shname"
		  qcmd="qsub -V -p -1 -l mem_free=1G -l mem_token=1G -l h_vmem=1G -e $logDir -o $logDir -q all.q $shname"
          qq=`$qcmd` # runs a qsub command
          qt=`echo $qq | awk '{print $3}'`

          jobid=${qt%%.*}
          qsubJobArray+=($jobid)
    fi
done


# *************************************************************************
# Step 2: wait until cluster jobs are complete until proceeding
# *************************************************************************
test=100
if command -v qsub &>/dev/null; then
  test=100
else
  test=0
fi

while [[ $test > 0 ]]; do
    echo "Checking Queue"
    test=`qstat -u $user | awk '{print $1}' | wc -l`

    echo "Waiting on: ${test} jobs to complete"
    sleep 30s
done