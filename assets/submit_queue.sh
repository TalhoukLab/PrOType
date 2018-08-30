#!/bin/bash

qsubJobArray=()

# Return number of jobs in queue for user
function njobs() {
  qstat -u $1 | grep -r ".*$1.*" | awk '{print $1}' | wc -l
}

for shname in "${file_to_submit[@]}"; do
	# check if data set was specified
	if [ "$shname" = "" ]; then
		  echo "script set must be specified."
		  exit 1
	fi

  mkdir -p $logDir
  startQLength=`njobs $user`
  curr_max_submit=$(($maxQueueLength - $startQLength))

	# execute shell script to queue
  if command -v qsub &>/dev/null; then
		  echo "Submitting To Queue ($user): $shname"

      while [[ "$currQLength" -ge "$curr_max_submit" ]]; do
        currQLength=`njobs $user`
        echo "Waiting on: ${currQLength} jobs to complete before submitting job."
        sleep 30s
      done

		  qcmd="qsub -V -p -1 -l mem_free=4G -l mem_token=4G -l h_vmem=8G -e $logDir -o $logDir -q all.q $shname"
      qq=`$qcmd` # runs a qsub command
      qt=`echo $qq | awk '{print $3}'`

      jobid=${qt%%.*}
      qsubJobArray+=($jobid)
      var=$((var - 1))
  fi
done


# *************************************************************************
# Step 2: wait until cluster jobs are complete until proceeding
# *************************************************************************
currQLength=100
if command -v qsub &>/dev/null; then
  currQLength=100
else
  currQLength=0
fi

while [[ $currQLength > 0 && shouldWait ]]; do
    echo "Checking Queue"
    currQLength=`njobs $user`

    if [[ $currQLength > 0 ]]; then
      echo "Waiting on: ${currQLength} jobs to complete"
      sleep 30s
    else
      echo "All jobs completed.";
    fi
done
