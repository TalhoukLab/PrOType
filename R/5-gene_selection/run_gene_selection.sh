#!/bin/bash

. ./Parameters.sh

# mkdir -p $outputDir/GeneSelection/output/training
# mkdir -p $outputDir/GeneSelection/output/TrainingC1
# mkdir -p $outputDir/GeneSelection/output/sumFreq
# mkdir -p $outputDir/GeneSelection/output/finalTraining
# mkdir -p $outputDir/GeneSelection/output/studyPreds
# mkdir -p $outputDir/GeneSelection/output/finalPredictions
# mkdir -p $outputDir/GeneSelection/plots

. ./R/5-gene_selection/1-run_bootstrap.sh
. ./R/5-gene_selection/2-process_bootstrap.sh
. ./R/5-gene_selection/3-run_final_training.sh
. ./R/5-gene_selection/4-make_predictions.sh
. ./R/5-gene_selection/5-training_post_processing.sh
. ./R/5-gene_selection/6-training_part2.sh
. ./R/5-gene_selection/7-build_final_model.sh
