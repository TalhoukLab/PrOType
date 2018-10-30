source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))

makePredictions(outputDir, study, train_dat, train_lab, algs)
cli::cat_line("Finished Making Predictions")
