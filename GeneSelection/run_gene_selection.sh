#!/bin/bash
. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/gene_selection
mkdir -p $outputDir/GeneSelection
mkdir -p $outputDir/output/TrainingC1
mkdir -p $outputDir/plots

Rname=$workDir$dataSet/R_file/gene_selection/gene_selection.R

echo 'output_dir <- file.path("'$outputDir'", "GeneSelection")' > $Rname
echo 'source("GeneSelection/GSTraining_initial.R")' >> $Rname
echo 'source("GeneSelection/GSTraining_part2.R")' >> $Rname
Rscript $Rname
