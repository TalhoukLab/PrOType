runBootstrap <- function(output_dir, study, x, y, B,
                         algs = c("lasso", "rf", "ada"),
                         seed_boot = 2018, seed_alg = 2018) {
  cli::cat_line("Running Bootstrap")
  output_dir <- mkdir(file.path(output_dir, "GeneSelection/output/training"))
  args <- tibble::lst(output_dir, study, x, y, B, seed_boot, seed_alg)
  purrr::walk(algs, ~ purrr::invoke(train_model, args, alg = .))
}

train_model <- function(output_dir, study, x, y, B, alg, seed_boot, seed_alg, shouldCompute=TRUE) {
  output_file <- file.path(output_dir, paste0(study, "_fit_", alg, ".rds"))
  if (file.exists(output_file) && !shouldCompute) {
    cli::cat_line("Output already exists.")
  } else {
    cli::cat_line("Train ", alg,  " model and write output")
    fit <- splendid::splendid_model(x, y, match_alg(alg), B, seed_boot, seed_alg)
    readr::write_rds(fit, output_file)
  }
}
