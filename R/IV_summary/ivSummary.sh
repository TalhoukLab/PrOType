#!/bin/bash

. ./Parameters.sh

file_to_submit=()
for dataset in "${dataSets[@]}"; do
	# check if data set was specified
	if [ "$dataset" = "" ]; then
		echo "Data set must be specified."
		exit 1
	fi

	# create scripts and directories
	mkdir -p $workDir$dataset'/R_file/iv_summary'
	mkdir -p $workDir$dataset'/sh_file/iv_summary'

	Rname=$workDir$dataset'/R_file/iv_summary/iv.R'
	shname=$workDir$dataset'/sh_file/iv_summary/iv.sh'

    mkdir -p $outputDir'/iv_summary/summary'$dataset

	echo 'outputDir <- "'$outputDir'"' > $Rname
	echo 'dataset <- "'$dataset'"' >> $Rname

	echo 'source("R/IV_summary/ivSummary.R")' >> $Rname
	#echo 'source("R/IV_summary/ivSummary.R")' >> $Rname

	echo "Rscript $Rname" > $shname
	chmod +x $shname

	# execute shell script to queue
    if command -v qsub &>/dev/null; then
		echo "Adding To Queue: $shname"
	    file_to_submit+=($shname)
    else
        echo "Making File"
        bash $shname
    fi
done

logDir=$baseLogDir'/IVSummary/ivSummary'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi

# *************************************************************************
# Step 3: combine all IV tables into one
# *************************************************************************
# create scripts and directories
mkdir -p $workDir'/R_file/iv_summary'
R_combine_name=$workDir'/R_file/iv_summary/combine.R'

echo 'ndat <- purrr::set_names(unlist(strsplit("'"${dataSets[*]}"'", " ")))' > $R_combine_name
echo 'outputDir <- "'$outputDir'"' >> $R_combine_name
echo "source('R/IV_summary/ivCombine.R')" >> $R_combine_name
Rscript $R_combine_name
