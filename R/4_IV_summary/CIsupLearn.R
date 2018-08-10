# Functions ---------------------------------------------------------------
# Sort Matrix
library(magrittr)

matrix.sort <- function(matrix) {
  cli::cat_line("Sorting matrix")
  if (nrow(matrix) != ncol(matrix)) stop("Not diagonal")
  if (is.null(rownames(matrix))) rownames(matrix) <- seq_len(nrow(matrix))

  row.max <- apply(matrix, 1, which.max)
  if (all(table(row.max) != 1)) stop("Ties cannot be resolved")

  matrix[names(sort(row.max)), ]
}

# Create CIs
create_ci <- function(df) {
  cli::cat_line("Creating CIs")
  res <- paste(sprintf("%.2f", df[, "50%"]),
               paste0("(", sprintf("%.2f", df[, "5%"]),
                      "-", sprintf("%.2f", df[, "95%"]), ")")) %>%
    data.frame() %>%
    magrittr::set_rownames(rownames(df))
  res
}

# Sort best
sort_best <- function(train_eval, top = 5) {
  cli::cat_line("Sorting best")
  te <- purrr::map(train_eval, function(x) {
    x %>%
      as.data.frame() %>%
      dplyr::mutate(logloss = -logloss) %>%
      t() %>%
      magrittr::extract(, 1)}) %>%
    data.frame(.)
  stopifnot(rownames(te)[1] == "logloss")

  cli::cat_line("Before calculating best")
  algsfull <- colnames(te)
  df <- te %>%
    apply(1, function(x) algsfull[order(rank(-x, ties.method = "random"))]) %>%
    t() %>%
    RankAggreg::RankAggreg(., ncol(.), method = "GA",
                           verbose = FALSE, maxIter = 2000)

  cli::cat_line("Calculating best")
  best <- te[, df$top.list]
  res <- best[c("auc", "accuracy", "f1.1", "f1.2", "f1.3", "f1.4"),
              seq_len(top)] %>%
    data.frame() %>%
    magrittr::set_colnames(colnames(best)[seq_len(top)])
  tibble::lst(best, res)
}

# Relabel classes
relabel_classes <- function(df, FinalR_lab) {
  cli::cat_line("Relaballing classes")
  a <- table(FinalR_lab[, 1], FinalR_lab[, "CL"])
  equi <- data.frame(class = rownames(matrix.sort(a)),
                     label = colnames(matrix.sort(a)))
  equi <- equi[order(equi$class), ]
  rownames(df)[grep(".\\d", rownames(df))] <-
    paste("F1", paste0(equi$class, "-", equi$label), sep = ".")
  df
}


# Read in -----------------------------------------------------------------

# data
final <- fdat %>%
  purrr::map(~ {
    FinalR_lab <- readRDS(file.path(outDir, "unsupervised", "final", ., paste0("all_clusts_", ., ".rds")))
    train_eval <- readRDS(file.path(outDir, "iv_summary", "train_eval", ., paste0("train_eval_", ., ".rds")))
    bests <- sort_best(train_eval, top = top)
    ci <- train_eval %>%
      purrr::map(~ {
        data.frame(.) %>%
          t() %>%
          create_ci()
      }) %>%
      data.frame() %>%
      magrittr::set_names(names(train_eval))
    ci[, colnames(bests$res)] # CI across evaluation metrics
  }) %>%
  magrittr::set_names(fdat)

purrr::iwalk(final, ~ {
  write.csv(.x, file.path(outDir, "iv_summary", "ci_sup_lrn", .y, paste0("sup_lrn_", .y, ".csv")))
})
