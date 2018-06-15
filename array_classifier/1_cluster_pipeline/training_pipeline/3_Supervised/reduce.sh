#!/bin/bash

. ./Parameters.sh

##Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#		 exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
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
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi

# build directories
rm -rf $workDir$dataSet'/R_file/reduce'
rm -rf $workDir$dataSet'/sh_file/reduce'
mkdir -p $workDir$dataSet'/R_file/reduce'
mkdir -p $workDir$dataSet'/sh_file/reduce'

# extract correct Model directories
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

# specify algs
algs=(first second third fourth) # rfRfe ldaRfe)

# create R and sh scripts
for i in "${algs[@]}"; do

	Rname=$workDir$dataSet/R_file/reduce/reduce_$i.R
	shname=$workDir$dataSet/sh_file/reduce/reduce_$i.sh

	#Content of R file
	touch $Rname
	echo 'outDir <- "'$outputDir'"'>>$Rname
	echo 'dataSet <- "'$dataSet'"'>>$Rname
	echo 'alg <- "'$i'"' >> $Rname
	echo 'fname <- "'$fname'"' >> $Rname
	echo 'source("'$workDir'3_Supervised/11_reduce.R")' >> $Rname
	echo 'reduce_supervised(dataSet, alg, outDir, fname)' >> $Rname

	# Content of shell script
	touch $shname
	echo '#!/bin/bash' >> $shname
	echo 'export PATH='$RPath':$PATH' >> $shname
	echo 'Rscript' $Rname >> $shname
	chmod 755 $shname
	$shname
done

