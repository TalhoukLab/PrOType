#!/bin/bash

. ./Parameters.sh

for dataset in "${dataSets[@]}"; do

    echo "Removing Folders"
    rm -rf $workDir$dataset'/R_file/prep'
    rm -rf $workDir$dataset'/sh_file/prep'

    echo "Creating folders"
    mkdir -p $workDir$dataset'/R_file/prep'
    mkdir -p $workDir$dataset'/R_file/clust'
    mkdir -p $workDir$dataset'/sh_file/prep'
    mkdir -p $workDir$dataset'/sh_file/clust'
    mkdir -p $outputDir$dataset
    mkdir -p $outputDir$dataset'/data_pr_'$dataset

    #File names for R script, rds output file, shell job script
    Rname=$workDir$dataset/R_file/prep/prep.R
    shname=$workDir$dataset/sh_file/prep/prep.sh

    echo "Working with folders: $Rname: $shname"

    #Content of R file
    echo 'pr<- "cs"' > $Rname
    echo 'sfdir<- "'$outputDir$dataset'"' >> $Rname
    echo 'ndat<- "'$dataset'"' >> $Rname
    echo 'datadir<- "'$outputDir$dataset'/data_pr_'$dataset'"' >> $Rname
    echo 'dpath<- "'$inputDir'"' >> $Rname
    echo 'source("'$workDir'1_Unsupervised/0_read_data.R")' >> $Rname

    echo 'Rscript' $Rname > $shname

    echo 'Preparation Script Completed!'

    chmod +x $shname
    $shname

    echo 'Preparation of Data Completed!'
done
