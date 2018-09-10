train_model <- function(output_dir, study, x, y, genes, B, ntop, alg, seed_boot,
                        seed_alg, shouldCompute = TRUE) {
  output_file <- file.path(output_dir, paste0(study, "_fit_", alg, ".rds"))
  if (file.exists(output_file) && !shouldCompute) {
    cli::cat_line("Output already exists.")
  } else {
    cli::cat_line("Train ", alg,  " model and write output")
    fit <- splendid::splendid_model(x, y, match_alg(alg), B, seed_boot, seed_alg)
    mods <- purrr::pluck(fit, "models")
    freq <- boot_freq(mods, alg, genes, B, ntop)
    readr::write_csv(freq, output_file)
  }
}
