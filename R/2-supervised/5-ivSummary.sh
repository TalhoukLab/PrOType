#!/bin/bash

. ./Parameters.sh

file_to_submit=()
for dataset in "${dataSets[@]}"; do

	# create scripts and directories
	mkdir -p $workDir/R_file/supervised/$dataset
	mkdir -p $workDir/sh_file/supervised/$dataset

	Rname=$workDir/R_file/supervised/$dataset/iv.R
	shname=$workDir/sh_file/supervised/$dataset/iv.sh

  mkdir -p $outputDir'/supervised/summary/'$dataset

	echo 'outputDir <- "'$outputDir'"' > $Rname
	echo 'dataset <- "'$dataset'"' >> $Rname

	echo 'source("R/2-supervised/5-ivSummary.R")' >> $Rname

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

logDir=$baseLogDir'/supervised/ivSummary'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi

# *************************************************************************
# Step 3: combine all IV tables into one
# *************************************************************************
# create scripts and directories
mkdir -p $workDir'/R_file/supervised'
R_combine_name=$workDir'/R_file/supervised/combine.R'

echo 'outputDir <- "'$outputDir'"' >> $R_combine_name
echo "source('R/2-supervised/5-ivCombine.R')" >> $R_combine_name
Rscript $R_combine_name
