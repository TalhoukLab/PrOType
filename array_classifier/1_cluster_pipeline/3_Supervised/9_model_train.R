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
                             normalize.by = "None", minVar = 0.5,
                             threshold = 0, norm.type = "conventional",
                             fname = "Model", shouldCompute = FALSE) {
  outputFile <- paste0(outDir, "/", fname, "_", dataSet, "/c1_", algs, reps, "_", dataSet, ".rds")

  cat("Checking previous input:", outputFile, "\n")


  if (file.exists(outputFile) && !shouldCompute) {
      cat("File already exists, skipping.\n")
      quit(status = 0)
  }


  cat("Reading training data:", algs, "-", reps, "\n")
  # import training data and final cluster assignments
  npcp <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds"))
  class <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/all_clusts_", dataSet, ".rds"))
  class.train <- class[["kmodes"]]

  cat("Normalizing data:", algs, "-", reps, "\n")
  # normalization
  if (normalize.by == "None") {
    cat("Normalizing None:", algs, "-", reps, "\n")
    data.train <- npcp %>%
       diceR::prepare_data(scale = FALSE, min.var = minVar, type = norm.type) %>%
       as.data.frame()
  } else if (normalize.by == "Genes") {
    cat("Normalizing Genes:", algs, "-", reps, "\n")
    data.train <- npcp %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Samples") {
    cat("Normalizing Samples:", algs, "-", reps, "\n")
    data.train <- npcp %>%
      t() %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      t() %>%
      as.data.frame()
  }

  cat("Running training algorithms:", algs, "-", reps, "\n")
  # train algorithms
  sm_args <- list(data = data.train, class = make.names(class.train), n = 1, seed_boot = reps, seed_alg = 1, threshold = threshold)
  sm <- switch(
    algs,
    first = purrr::invoke(splendid::splendid_model, sm_args,
                          algorithms = c("lda", "rf", "pam", "mlr_lasso")),
    second = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = "svm"),
    third = purrr::invoke(splendid::splendid_model, sm_args,
                          algorithms = c("knn", "adaboost")),
    fourth = purrr::invoke(splendid::splendid_model, sm_args,
                           algorithms = c("nbayes", "mlr_ridge"))
  )

  cat("Saving output:", algs, "-", reps, "\n")
  # write evaluations to file
  readr::write_rds(sm[["evals"]], outputFile)
}
