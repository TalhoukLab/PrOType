#!/bin/bash

. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/cross_platform
mkdir -p $workDir$dataSet/sh_file/cross_platform

mkdir -p $outputDir/CrossPlatform/output
mkdir -p $outputDir/CrossPlatform/plots

Rname=$workDir$dataSet/R_file/cross_platform/analysis_preds.R
shname=$workDir$dataSet/sh_file/cross_platform/analysis_preds.sh

echo 'outputDir <- "'$outputDir'"' > $Rname
echo "source('R/7_cross_platform/analysis.R')" >> $Rname
echo "source('R/7_cross_platform/predictions.R')" >> $Rname

echo "Rscript $Rname" > $shname

chmod +x $shname

file_to_submit=($shname)
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
