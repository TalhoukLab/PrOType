#!/bin/bash
. ./Parameters.sh

mkdir -p $workDir$dataSet/R_file/gene_selection
mkdir -p $outputDir/GeneSelection/output/training
mkdir -p $outputDir/GeneSelection/output/TrainingC1
mkdir -p $outputDir/GeneSelection/output/sumFreq
mkdir -p $outputDir/GeneSelection/output/finalTraining
mkdir -p $outputDir/GeneSelection/output/studyPreds
mkdir -p $outputDir/GeneSelection/tmp

mkdir -p $outputDir/plots

. ./R/gene_selection/run_bootstrap.sh
. ./R/gene_selection/process_bootstrap.sh
. ./R/gene_selection/run_final_training.sh
. ./R/gene_selection/make_predictions.sh

. ./R/gene_selection/training_post_processing.sh

# TODO:// Move these to Qsub.
Rscript R/gene_selection/training_part2.R
