source(here::here("R/5-gene_selection/0-gs_setup.R"))

cli::cat_line("Getting Data")
x <- sl_data(train_dat, study, "training")
y <- sl_class(train_lab, x)
genes <- get_genes(train_dat)
ntop <- 100
seed_boot <- 2018
seed_alg <- 2018

output_file <- file.path(outputDir, "gene_selection", "boot_freq",
                         paste0(study, "_freq_", alg, ".csv"))
if (file.exists(output_file) && !shouldCompute) {
  cli::cat_line("Output already exists.")
} else {
  cli::cat_line("Training ", alg,  " model")
  fit <- splendid::splendid_model(x, y, match_alg(alg), B, seed_boot, seed_alg)
  cli::cat_line("Calculating ", alg, " bootstrap frequencies")
  mods <- purrr::pluck(fit, "models")
  freq <- gene_freq(mods, alg, genes, B, ntop)
  readr::write_csv(freq, output_file)
  if (alg == "lasso") {
    lasso_by_class <- lasso_freq(fit, genes)
    readr::write_csv(lasso_by_class,
                     file.path(outputDir, "gene_selection", "boot_freq",
                               paste0(study, "_lasso_by_class.csv")))
  }
}
cli::cat_line("Completed ", alg, " bootstrap frequencies")
