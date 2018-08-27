#!/bin/bash
. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/gene_selection
mkdir -p $outputDir/GeneSelection/output/training
mkdir -p $outputDir/GeneSelection/output/TrainingC1
mkdir -p $outputDir/GeneSelection/output/sumFreq
mkdir -p $outputDir/GeneSelection/output/finalTraining
mkdir -p $outputDir/GeneSelection/output/studyPreds

mkdir -p $outputDir/GeneSelection/plots

Rname=$workDir$dataSet/R_file/gene_selection/gene_selection.R

echo 'output_dir <- file.path("'$outputDir'", "GeneSelection")' > $Rname
echo 'plot_dir <- file.path(output_dir, "plots")' >> $Rname
echo 'source("GeneSelection/GSTraining_initial.R")' >> $Rname
echo 'source("GeneSelection/GSTraining_part2.R")' >> $Rname
Rscript $Rname
