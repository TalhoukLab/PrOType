writeSummaryFreqs <- function(output_dir, train_dat, algs) {
  cli::cat_rule("Calculating summary frequencies")
  fnames <- list.files(
    path = file.path(output_dir, "gene_selection", "sum_freq"),
    pattern = "[A-Z]{3}_sum_freq.csv",
    full.names = TRUE
  )
  overall <- fnames %>%
    purrr::map(read.csv, stringsAsFactors = FALSE, row.names = 1) %>%
    purrr::map(~ magrittr::extract(., order(rownames(.)), , drop = FALSE)) %>%
    Reduce(`+`, .) %>%
    magrittr::divide_by(length(fnames)) %>%
    tibble::rownames_to_column("genes")

  cli::cat_line("Write overall summary")
  readr::write_csv(overall,
                   file.path(output_dir, "gene_selection", "sum_freq", "overall_freqs.csv"))

  if ("lasso" %in% algs) {
    fnames <- list.files(
      path = file.path(output_dir, "gene_selection", "boot_freq"),
      pattern = "lasso",
      full.names = TRUE
    )
    genes <- get_genes(train_dat)
    overall_lasso <- fnames %>%
      purrr::map(readRDS) %>%
      magrittr::divide_by(length(.)) %>%
      purrr::reduce(`+`) %>%
      tibble::rownames_to_column("genes")
      # purrr::map(function(f) {
      #   li <- f %>%
      #     readRDS() %>%
      #     purrr::pluck("models", "mlr_lasso") %>%
      #     purrr::map(function(x) {
      #       ndx <- x$glmnet.fit$lambda == x$lambda.1se
      #       x %>%
      #         purrr::pluck("glmnet.fit", "beta") %>%
      #         purrr::map_df(~ ifelse(genes %in% names(which(.[, ndx] != 0)), 1, 0))
      #     })
      #   tmp <- Reduce(`+`, li) / length(li)
      #   rownames(tmp) <- genes
      #   tmp
      # }) %>%
      # Reduce(`+`, .) %>%
      # magrittr::divide_by(length(fnames)) %>%
      # tibble::rownames_to_column("genes")

    cli::cat_line("Write overall lasso summary")
    readr::write_csv(overall_lasso,
                     file.path(output_dir, "gene_selection", "sumFreq", "overall_lasso_by_class.csv"))
  }
}
