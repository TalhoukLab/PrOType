#!/bin/bash

qsubJobArray=()

for shname in $file_to_submit; do
	# check if data set was specified
	if [ "$shname" = "" ]; then
		echo "script set must be specified."
		exit 1
	fi

	# execute shell script to queue
    if command -v qsub &>/dev/null; then
		  echo "Using: $shname"
	        qsubJobArray+=($(qsub -V -p -1 -l mem_free=1G -l mem_token=2G -l h_vmem=15G -e $logDir -o $logDir -q all.q $shname))
    else
        :
    fi
done


# *************************************************************************
# Step 2: wait until cluster jobs are complete until proceeding
# *************************************************************************
temp='/tmp/'

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
		echo "jobs completed!"
	fi
done