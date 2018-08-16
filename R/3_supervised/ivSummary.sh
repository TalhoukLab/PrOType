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
	mkdir -p $workDir/R_file/supervised/$dataset
	mkdir -p $workDir/sh_file/supervised/$dataset

	Rname=$workDir/R_file/supervised/$dataset/iv.R
	shname=$workDir/sh_file/supervised/$dataset/iv.sh

    mkdir -p $outputDir'/supervised/summary/'$dataset

	echo 'outputDir <- "'$outputDir'"' > $Rname
	echo 'dataset <- "'$dataset'"' >> $Rname

	echo 'source("R/3_supervised/ivSummary.R")' >> $Rname

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


mkdir -p $workDir/R_file/supervised
mkdir -p $workDir/sh_file/supervised


Rname=$workDir/R_file/supervised/top_models.R
shname=$workDir/sh_file/supervised/top_models.sh

echo 'outputDir <- "'$outputDir'"' > $Rname
echo 'source("R/3_supervised/top_models.R")' >> $Rname

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

echo 'ndat <- purrr::set_names(unlist(strsplit("'"${dataSets[*]}"'", " ")))' > $R_combine_name
echo 'outputDir <- "'$outputDir'"' >> $R_combine_name
echo "source('R/3_supervised/ivCombine.R')" >> $R_combine_name
Rscript $R_combine_name
