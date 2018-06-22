# Functions ---------------------------------------------------------------
# Sort Matrix
library(magrittr)

matrix.sort <- function(matrix) {
  cat("Sorting matrix\n")
  if (nrow(matrix) != ncol(matrix)) stop("Not diagonal")
  if (is.null(rownames(matrix))) rownames(matrix) <- seq_len(nrow(matrix))

  row.max <- apply(matrix, 1, which.max)
  if (all(table(row.max) != 1)) stop("Ties cannot be resolved")

  matrix[names(sort(row.max)), ]
}

# Create CIs
create_ci <- function(df) {
  cat("Creating CIs\n")
  res <- paste(sprintf("%.2f", df[, "50%"]),
               paste0("(", sprintf("%.2f", df[, "5%"]),
                      "-", sprintf("%.2f", df[, "95%"]), ")")) %>%
    data.frame() %>%
    magrittr::set_rownames(rownames(df))
  res
}

# Sort best
sort_best <- function(train_eval, top = 5) {
  cat("Sorting best\n")
  te <- train_eval %>%
    `[`(grep("evals", names(.))) %>%
    purrr::map(~ {
      do.call(cbind, .) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::mutate(logloss = -logloss) %>%
        `colnames<-`(gsub("\\.X", "\\.", colnames(.))) %>%
        t() %>%
        magrittr::extract(, which.max(.["logloss", ])) # Choose best logloss
    }) %>%
    as.data.frame()
  algsfull <- colnames(te)
  df <- te %>%
    apply(1, function(x) algsfull[order(rank(-x, ties.method = "random"))]) %>%
    t() %>%
    RankAggreg::RankAggreg(., ncol(.), method = "GA",
                           verbose = FALSE, maxIter = 2000)
  cat("Calculating best\n")
  best <- te[, df$top.list]
  res <- best[c("auc", "accuracy", "f1.1", "f1.2", "f1.3", "f1.4"),
              seq_len(top)] %>%
    data.frame() %>%
    magrittr::set_colnames(colnames(best)[seq_len(top)])
  tibble::lst(best, res)
}

# Relabel classes
relabel_classes <- function(df, FinalR_lab) {
  cat("Relaballing classes\n")
  a <- table(FinalR_lab[, 1], FinalR_lab[, "CL"])
  equi <- data.frame(class = rownames(matrix.sort(a)),
                     label = colnames(matrix.sort(a)))
  equi <- equi[order(equi$class), ]
  rownames(df)[grep(".\\d", rownames(df))] <-
    paste("F1", paste0(equi$class, "-", equi$label), sep = ".")
  df
}


# Inputs ------------------------------------------------------------------

inDir <- inDir
outDir <- outDir
fdat <- fdat
top <- top


# Read in -----------------------------------------------------------------

# data
final <- fdat %>%
  purrr::map(~ {
    FinalR_lab <- readRDS(paste0(inDir, ., "/data_pr_", ., "/all_clusts_", ., ".rds"))
    train_eval <- readRDS(paste0(inDir, ., "/data_pr_", ., "/train_eval_", ., ".rds"))
    te_names <- names(train_eval)
    train_eval <- train_eval %>%
      purrr::set_names(paste0(te_names,
                              rep(seq_len(unique(table(te_names))),
                                  each = dplyr::n_distinct(te_names))))
    bests <- sort_best(train_eval, top = top)
    ci <- train_eval %>%
      `[`(grep("evals", names(.))) %>%
      purrr::map(~ {
        do.call(cbind, .) %>%
          t() %>%
          as.data.frame() %>%
          dplyr::mutate(logloss = -logloss) %>%
          `colnames<-`(gsub("\\.X", "\\.", colnames(.))) %>%
          t() %>%
          magrittr::extract(, which.max(.["logloss", ])) # Choose best logloss
      }) %>%
      purrr::map(quantile, probs = c(0.05, 0.5, 0.95)) %>%
      do.call(rbind, .) %>%
      create_ci() %>%
      t()
    ci[, colnames(bests$res)] # CI across evaluation metrics?
  }) %>%
  magrittr::set_names(fdat)

purrr::iwalk(final, ~ {
  write.csv(.x, paste0(outDir, .y, "/data_pr_", .y, "/sup_lrn_", .y, ".csv"))
})
