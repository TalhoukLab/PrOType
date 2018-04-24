#' Train Splendid ensemble algorithm
#'
#' Normalizes a given training set and trains the splendid ensemble
#'
#' @param dataSet is a data frame with feature columns and samples as rows
#' @param algs an algorithm supported by Splendid
#' @param reps an integer specifying the index of the bootstrap repeat
#' @param inDir directory containing the data inputs
#' @param outDir directory specifying the output location
#' @param normalize.by how you would like to normalize the data. "None" does not
#'   normalize the data, "genes" normalizes gene-wise, "samples" normalizes
#'   sample-wise.
#' @param min.var the minimum variability at which you are willing to accept a
#'   given factor
#' @param norm.type the type of normalization performed: "conventional",
#'   "robust", "tsne", or "largevis". See ?diceR::prepare_data for details.
#' @return a list of model fits from Spendid
#' @author Last updated on 30/10/2017 by Dustin Johnson. Edited by Derek Chiu
train_supervised <- function(dataSet, algs, reps, inDir, outDir,
                             is.housekeeping.normalized = FALSE,
                             normalize.by = "None", min.var = 0.5,
                             threshold = 0, norm.type = "conventional",
                             fname = "Model") {
  # import training data
  if (!is.housekeeping.normalized) {
    npcp <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/npcp_", dataSet, ".rds"))
  } else {
    npcp <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds"))
  }

  class <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/all_clusts_", dataSet, ".rds"))
  class.train <- class[, 1]

  # normalization
  if (normalize.by == "None") {
    data.train <- npcp %>%
      diceR::prepare_data(scale = FALSE, min.var = minVar, type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Genes") {
    data.train <- npcp %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Samples") {
    data.train <- npcp %>%
      t() %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      t() %>%
      as.data.frame()
  }

  # train algorithms
  reps <- as.integer(reps)
  sm_args <- list(data = data.train, class = class.train, n = 1, seed = reps, threshold = threshold)
  sm <- switch(
    algs,
    first = purrr::invoke(splendid::splendid_model, sm_args,
                          algorithms = c("lda", "rf", "pam", "mlr_lasso"), # xgboost
                          rfe = FALSE),
    second = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = "svm", rfe = FALSE),
    third = purrr::invoke(splendid::splendid_model, sm_args,
                          algorithms = c("knn", "adaboost"), # nnet
                          rfe = FALSE),
    fourth = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = c("nbayes", "mlr_ridge"),
                           rfe = FALSE),
    ldaRfe = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = "lda", rfe = TRUE),
    rfRfe = purrr::invoke(splendid::splendid_model, sm_args,
                          algorithms = "rf", rfe = TRUE),
    lassoRfe = purrr::invoke(splendid::splendid_model, sm_args,
                             algorithms = "mlr_lasso", rfe = TRUE),
    svmRfe = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = "svm", rfe = TRUE)
  )

  # write to file
  readr::write_rds(sm, paste0(outDir, "/", fname, "_", dataSet, " / c1_", algs, reps, "_", dataSet, ".rds"))
}
