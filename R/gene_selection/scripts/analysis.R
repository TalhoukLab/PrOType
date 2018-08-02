library(purrr)

runGeneAnalysis <- function(output_dir, train_dat, algs) {
  cli::cat_rule("Running Gene analysis")
  fnames <- list.files(file.path(output_dir, "GeneSelection/output/sumFreq"),
                       pattern = "_sumFreq.csv", full.names = TRUE)
  site_names <- table(train_dat$site) %>% paste0(names(.), .)

  if ("lasso" %in% algs) {
    cli::cat_line("lasso30")
    lasso30 <- purrr::map(fnames, ~ {
      readr::read_csv(file = ., col_types = readr::cols()) %>%
        dplyr::arrange(dplyr::desc(lassoFreq)) %>%
        dplyr::pull(genes) %>%
        head(30)
    }) %>%
      purrr::set_names(site_names)

    genes30 <- Reduce(union, lasso30)
    geneRanks_lasso <- lasso30 %>%
      purrr::map(match, x = genes30, nomatch = 50) %>%
      as.data.frame() %>%
      magrittr::set_rownames(genes30)

    pdf(file.path(plot_dir, "lasso30_heatmap.pdf"))
    pheatmap::pheatmap(geneRanks_lasso, fontsize_row = 7, main = "Lasso")
    dev.off()
  }
  if ("rf" %in% algs) {
    cli::cat_line("rf60")
    rf60 <- purrr::map(fnames, ~ {
      readr::read_csv(file = ., col_types = readr::cols()) %>%
        dplyr::arrange(dplyr::desc(rfFreq)) %>%
        dplyr::pull(genes) %>%
        head(60)
    }) %>%
      purrr::set_names(site_names)

    genes60 <- Reduce(union, rf60)
    geneRanks_rf <- rf60 %>%
      purrr::map(match, x = genes60, nomatch = 70) %>%
      as.data.frame() %>%
      magrittr::set_rownames(genes60)

    pdf(file.path(plot_dir, "rf60_heatmap.pdf"))
    pheatmap::pheatmap(geneRanks_rf, fontsize_row = 6, main = "Random Forest")
    dev.off()
  }
}
