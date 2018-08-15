# Functions ---------------------------------------------------------------
library(magrittr)

# Create CIs
create_ci <- function(df, alg) {
  cli::cat_line("Creating CIs for ", alg)
  res <- paste(sprintf("%.2f", df[, "50%"]),
               paste0("(", sprintf("%.2f", df[, "5%"]),
                      "-", sprintf("%.2f", df[, "95%"]), ")")) %>%
    data.frame() %>%
    magrittr::set_colnames(alg) %>%
    magrittr::set_rownames(rownames(df))
  res
}

# Sort best
sort_best <- function(train_eval, top = 5) {
  cli::cat_line("Combining median evaluation metrics")
  te <- purrr::map(train_eval, ~ {
    as.data.frame(.) %>%
      dplyr::mutate(logloss = -logloss) %>%
      t() %>%
      magrittr::extract(, 1)
  }) %>%
    data.frame()
  stopifnot(rownames(te)[1] == "logloss")

  cli::cat_line("Calculating top ", top, " algorithms by rank aggregation")
  top.list <- te %>%
    apply(1, function(x) colnames(.)[order(-x, sample(length(x)))]) %>%
    t() %>%
    RankAggreg::RankAggreg(., ncol(.), method = "GA",
                           verbose = FALSE, maxIter = 2000) %>%
    purrr::pluck("top.list") %>%
    magrittr::extract(seq_len(top))

  cli::cat_line("Extracting top AUC, accuracy, by-class F1")
  te[c("auc", "accuracy", "f1.X1", "f1.X2", "f1.X3", "f1.X4"), top.list]
}

# Read in -----------------------------------------------------------------

# data
final <- fdat %>%
  purrr::map(~ {
    FinalR_lab <- readRDS(file.path(outputDir, "unsupervised", "final", ., paste0("all_clusts_", ., ".rds")))
    train_eval <- readRDS(file.path(outputDir, "iv_summary", "train_eval", paste0("train_eval_", ., ".rds")))
    bests <- sort_best(train_eval, top = top)
    ci <- train_eval %>%
      purrr::imap(~ {
        data.frame(.x) %>%
          t() %>%
          create_ci(.y)
      }) %>%
      data.frame()
    ci[, colnames(bests)]
  }) %>%
  magrittr::set_names(fdat)

purrr::iwalk(final, ~ {
  write.csv(.x, file.path(outputDir, "iv_summary", "ci_sup_lrn", paste0("sup_lrn_", .y, ".csv")))
})
