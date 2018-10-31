source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))
library(Matrix)

evaluate_predictions(outputDir, train_dat, train_lab, algs)
cli::cat_line("Finished Evaluating Predictions")

summarize_freqs(outputDir, train_dat, algs)
cli::cat_line("Finished Writing Summary Freqs")

analyze_genes(outputDir, train_dat, algs)
cli::cat_line("Finished Gene Analysis")
