source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))
library(Matrix)

evaluatePredictions(outputDir, train_dat, train_lab, algs)
cli::cat_line("Finished Evaluating Predictions")

writeSummaryFreqs(outputDir, train_dat, algs)
cli::cat_line("Finished Writing Summary Freqs")

runGeneAnalysis(outputDir, train_dat, algs)
cli::cat_line("Finished Gene Analysis")
