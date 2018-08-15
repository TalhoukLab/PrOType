#!/bin/bash

. ./Parameters.sh

for dataset in "${dataSets[@]}"; do

    echo "Creating folders"

    mkdir -p $workDir'/R_file/prep/'$dataset
    mkdir -p $workDir'/sh_file/prep/'$dataset

    datadir=$outputDir'/unsupervised/prep_data/'$dataset
    mkdir -p $datadir

    #File names for R script, rds output file, shell job script
    Rname=$workDir/R_file/prep/$dataset/prep.R
    shname=$workDir/sh_file/prep/$dataset/prep.sh

    #Content of R file
    echo 'pr<- "cs"' > $Rname
    echo 'sfdir<- "'$outputDir$dataset'"' >> $Rname
    echo 'dataset<- "'$dataset'"' >> $Rname
    echo 'datadir<- "'$datadir'"' >> $Rname
    echo 'dpath<- "'$inputDir'"' >> $Rname
    echo 'source("R/1_unsupervised/read_data.R")' >> $Rname

    echo 'Rscript' $Rname > $shname

    echo 'Preparation Script Completed!'

    chmod +x $shname
    $shname

    echo 'Preparation of Data Completed!'
done
