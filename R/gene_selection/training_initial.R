# Clear the deck----

# Load Parameters----
B <- 5
runBoot <- 1
processBoot <- 1
finalTraining <- 1
makePreds <- 1
evaluatePreds <- 1
producePlots <- 1
sumOverallFreqs <- 1
geneAnalysis <- 1
algs <- c("lasso", "rf")

# Load packages----
library(here)
library(tidyverse)
suppressPackageStartupMessages({
  library(magrittr)
  library(splendid)
  library(caret)
  library(glmnet)
  library(cli)
})

GS_training_dir <- "R/gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R",
  "evaluate.R",
  "summary.R",
  "analysis.R"
)
walk(here(GS_training_dir, GS_training_files), source)

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab

# Get studies
studies <- unique(train_dat$site)

# Train over study site----
runTrainingSequence(runBoot, processBoot, finalTraining, makePreds,
                    output_dir, studies, train_dat, train_lab, B, algs)

# Evaluate the predictions----
if (evaluatePreds) {
  evaluatePredictions(output_dir, train_dat, train_lab, algs, producePlots)
}

# Combine sumFreqs by averaging over all Freqs over studies----
if (sumOverallFreqs) {
  writeSummaryFreqs(output_dir, train_dat, algs)
}

# Perform Gene Analysis ----
if (geneAnalysis) {
  runGeneAnalysis(output_dir, train_dat, algs)
}
