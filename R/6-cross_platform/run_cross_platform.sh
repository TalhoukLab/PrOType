#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/cross_platform
mkdir -p $workDir$dataSet/sh_file/cross_platform

mkdir -p $outputDir/CrossPlatform/output
mkdir -p $outputDir/CrossPlatform/plots

Rname1=$workDir$dataSet/R_file/cross_platform/cp_analysis.R
shname1=$workDir$dataSet/sh_file/cross_platform/cp_analysis.sh

echo 'outputDir <- "'$outputDir'"' > $Rname1
echo "source('R/6-cross_platform/1-analysis.R')" >> $Rname1

Rname2=$workDir$dataSet/R_file/cross_platform/cp_predictions.R
shname2=$workDir$dataSet/sh_file/cross_platform/cp_predictions.sh

echo 'outputDir <- "'$outputDir'"' > $Rname2
echo "source('R/6-cross_platform/2-predictions.R')" >> $Rname2

echo "Rscript $Rname1" > $shname1
echo "Rscript $Rname2" > $shname2

chmod +x $shname1
chmod +x $shname2

file_to_submit=($shname1 $shname2)
if command -v qsub &>/dev/null; then
  :
else
  bash $shname
fi

logDir=$baseLogDir'/cross_platform'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
