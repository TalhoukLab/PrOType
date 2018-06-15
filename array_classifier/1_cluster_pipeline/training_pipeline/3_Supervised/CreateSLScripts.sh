#!/bin/bash

. ./Parameters.sh

## specify data set to use
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#	exit 1
#fi
#
## specify number of reps
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#	exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify the input directory
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
## specify the normalization method
#if [ "$normalizeBy" = "" ]
#then echo "Normalization method must be specified"
#	exit 1
#fi
#
## specify the minimum variance
#if [ "$minVar" = "" ]
#then echo "Minimum variance type must be specified"
#	exit 1
#fi
#
## specify the normalization type
#if [ "$normType" = "" ]
#then echo "Normalization type must be specified"
#	exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi

# specify algorithms
algs=(first second third fourth) #ldaRfe qdaRfe rfRfe lassoRfe)

# build directories
rm -rf $workDir$dataSet'/R_file/train'
rm -rf $workDir$dataSet'/sh_file/train'
mkdir -p $workDir$dataSet'/R_file/train'
mkdir -p $workDir$dataSet'/sh_file/train'

if [ "$normalizeBy" = "Genes" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc-genes_'$dataSet
	fname='Model-hc-genes'
elif [ "$normalizeBy" = "Samples" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc-samples_'$dataSet
	fname='Model-hc-samples'
elif [ "$normalizeBy" = "None" ];
then
	mkdir -p $outputDir$dataSet'/Model-hc_'$dataSet
	fname='Model-hc'
else
	echo "A normalization of type Genes, Samples or None must be specified"
fi

# create R and sh scripts
for i in "${algs[@]}"; do

	for s in `seq 1 $supervised_reps`; do

		#File names for R script, rds output file, shell job script
		R_train=$workDir$dataSet/R_file/train/$i$s.R
		sh_train=$workDir$dataSet/sh_file/train/$i$s.sh

		#Content of R file
		touch $R_train
		echo 'dataSet <- "'$dataSet'"' >> $R_train
		echo 'reps <- '$s >> $R_train
		echo 'algs <- "'$i'"' >> $R_train
		echo 'inDir <- "'$outputDir$dataSet'"' >> $R_train
		echo 'outDir <- "'$outputDir$dataSet'"' >> $R_train
		echo 'normalizeBy <- "'$normalizeBy'"' >> $R_train
		echo 'minVar <- '$minVar >> $R_train
		echo 'normType <- "'$normType'"' >> $R_train
		echo 'fname <- "'$fname'"' >> $R_train
		echo 'threshold <- '$threshold >> $R_train
		echo 'source("'$workDir'3_Supervised/9_model_train.R")' >> $R_train
		echo 'train_supervised(dataSet, algs, reps, inDir, outDir, normalizeBy, minVar, threshold, normType, fname)' >> $R_train

		# contents of sh file
		touch $sh_train
		echo '#!/bin/sh' >> $sh_train
		echo 'export PATH='$RPath':$PATH' >> $sh_train
		echo 'Rscript' $R_train >> $sh_train
	done
done
