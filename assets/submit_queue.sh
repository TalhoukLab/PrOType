#!/bin/bash

# Dynamic messages about standard error log files
function err_msg() {
  err_nfiles=`find $1 -name "*.e*" -type f ! -empty | wc -l`
  err_nlines=`find $1 -name "*.e*" -exec cat {} + | wc -l`
  if (($err_nfiles > 0)); then
    echo -e "$RED_CROSS ${err_nlines} lines written to ${err_nfiles} standard error log files"
  else
    echo -e "$GREEN_TICK ${err_nlines} lines written to standard error log files"
  fi
}

# Queue status for user without the column headers
function qstatu() {
  qstat -u $1 | grep -r ".*$1.*"
}

# Current number of jobs in queue for user
function njobs() {
  qstatu $1 | wc -l
}

# Elapsed time since first job submitted for user
# Reference: https://www.linuxquestions.org/questions/linux-newbie-8/time-difference-calculation-4175459414/
function elapsed() {
  first=`qstatu $1 | awk "{print $7}" | head -n 1`
  now=$(date +"%T")
  SEC1=`date +%s -d ${first}`
  SEC2=`date +%s -d ${now}`
  DIFFSEC=`expr ${SEC2} - ${SEC1}`
  date +%H:%M:%S -ud @${DIFFSEC}
}

# Print number of total jobs to submit
echo -e "$BLUE_BULLET Total jobs: ${#file_to_submit[@]}"

# Step 1: Loop over array of shell scripts and submit batch jobs
for shname in "${file_to_submit[@]}"; do
	# Check if data set was specified
	if [ "$shname" = "" ]; then
		  echo "Shell script must be specified."
		  exit 1
	fi

	# Check current max job limit
  mkdir -p $logDir
  startQLength=`njobs $user`
  curr_max_submit=$(($maxQueueLength - $startQLength))

  # Job submission
  if command -v qsub &>/dev/null; then
      # Ensure no jobs are submitted if current max job limit is reached
      while [[ "$currQLength" -ge "$curr_max_submit" ]]; do
        currQLength=`njobs $user`
        echo -e "$BLUE_BULLET Waiting on: ${currQLength} jobs to complete before submitting job"
        sleep 30s
      done

      # Submit shell script to queue without extra verbosity using variable qq
		  qcmd="qsub -V -p -1 -l mem_free=$mem_free -l mem_token=$mem_token -l h_vmem=$h_vmem -e $logDir -o $logDir -q all.q $shname"
      qq=`$qcmd`
      echo -e "$GREEN_TICK Submitted to queue: $shname"
  fi
done
echo -e "$GREEN_TICK Job submission finished. Check status with \"qstat -u ${user}\""

# Step 2: wait until all jobs are complete before proceeding
currQLength=100
if command -v qsub &>/dev/null; then
  currQLength=100
else
  currQLength=0
fi

while [[ $currQLength > 0 && shouldWait ]]; do
    currQLength=`njobs $user`

    if [[ $currQLength > 0 ]]; then
      echo -e "$BLUE_BULLET Time elapsed: `elapsed ${user}` | Jobs remaining: ${currQLength}"
      sleep 30s
    else
      echo -e "$GREEN_TICK All jobs completed"
    fi
done

# Step 3: Verbose printouts
err_msg $logDir
echo -e "$GREEN_BULLET Logs written to \"${logDir}\""
echo -e "$GREEN_BULLET Outputs written to \"${outputDir}\""
