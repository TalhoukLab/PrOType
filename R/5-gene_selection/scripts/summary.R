overall_freq <- function(files) {
  files %>%
    purrr::map(readr::read_csv, col_types = readr::cols()) %>%
    purrr::map(dplyr::arrange, .data$genes) %>%
    purrr::map(as.data.frame) %>%
    purrr::map(tibble::column_to_rownames, "genes") %>%
    purrr::map(function(x) x / length(.)) %>%
    purrr::reduce(`+`) %>%
    tibble::rownames_to_column("genes")
}

writeSummaryFreqs <- function(output_dir, train_dat, algs) {
  cli::cat_rule("Calculating summary frequencies")
  fnames <- list.files(
    path = file.path(output_dir, "gene_selection", "sum_freq"),
    pattern = "[A-Z]{3}_sum_freq.csv",
    full.names = TRUE
  )
  overall <- overall_freq(fnames)

  cli::cat_line("Overall frequencies")
  readr::write_csv(overall,
                   file.path(output_dir, "gene_selection", "sum_freq", "overall_freqs.csv"))

  if ("lasso" %in% algs) {
    fnames <- list.files(
      path = file.path(output_dir, "gene_selection", "boot_freq"),
      pattern = "lasso",
      full.names = TRUE
    )
    overall_lasso <- overall_freq(fnames)

    cli::cat_line("Overall lasso by-class frequencies")
    readr::write_csv(overall_lasso,
                     file.path(output_dir, "gene_selection", "sum_freq", "overall_lasso_by_class.csv"))
  }
}
