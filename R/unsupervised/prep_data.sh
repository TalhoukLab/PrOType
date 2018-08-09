#!/bin/bash

. ./Parameters.sh

for dataset in "${dataSets[@]}"; do

    echo "Creating folders"

    mkdir -p $workDir$dataset'/R_file/prep'
    mkdir -p $workDir$dataset'/sh_file/prep'

    datadir=$outputDir'/unsupervised/prep_data/'$dataset
    mkdir -p $datadir

    #File names for R script, rds output file, shell job script
    Rname=$workDir$dataset/R_file/prep/prep.R
    shname=$workDir$dataset/sh_file/prep/prep.sh

    echo "Working with folders: $Rname: $shname"

    #Content of R file
    echo 'pr<- "cs"' > $Rname
    echo 'sfdir<- "'$outputDir$dataset'"' >> $Rname
    echo 'dataset<- "'$dataset'"' >> $Rname
    echo 'datadir<- "'$datadir'"' >> $Rname
    echo 'dpath<- "'$inputDir'"' >> $Rname
    echo 'source("R/unsupervised/read_data.R")' >> $Rname

    echo 'Rscript' $Rname > $shname

    echo 'Preparation Script Completed!'

    chmod +x $shname
    $shname

    echo 'Preparation of Data Completed!'
done
