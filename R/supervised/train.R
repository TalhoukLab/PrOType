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


library(magrittr)

train_supervised <- function(dataSet, algs, reps, inDir, outDir,
                             normalize.by = "None", minVar = 0.5,
                             threshold = 0, norm.type = "conventional",
                             fname = "Model", shouldCompute = FALSE) {
  outputFile <- file.path(outDir, paste0(fname, "_", dataSet), paste0("c1_", algs, reps, "_", dataSet, ".rds"))

  cli::cat_line("Checking previous input:", outputFile)

  if (file.exists(outputFile) && !shouldCompute) {
      cli::cat_line("File already exists, skipping.")
      quit(status = 0)
  }


  cli::cat_line("Reading training data:", algs, "-", reps)
  # import training data
  npcp <-  readr::read_rds(file.path(inDir, paste0("data_pr_", dataSet), paste0("npcp-hcNorm_", dataSet, ".rds")))
  class <- readr::read_rds(file.path(inDir, paste0("data_pr_", dataSet), paste0("all_clusts_", dataSet, ".rds")))

  # class to train on is best performing ensemble algorithm
  ens <- c("CSPA", "kmodes", "majority", "cts", "srs", "asrs")
  ens.id <- which.min(match(ens, names(class)))
  cli::cat_line("Best ensemble algorithm: ", ens[ens.id])
  class.train <- class[[ens[ens.id]]]

  cli::cat_line("Normalizing data:", algs, "-", reps)
  # normalization
  if (normalize.by == "None") {
    cli::cat_line("Normalizing None:", algs, "-", reps)
    data.train <- npcp %>%
       diceR::prepare_data(scale = FALSE, min.var = minVar, type = norm.type) %>%
       as.data.frame()
  } else if (normalize.by == "Genes") {
    cli::cat_line("Normalizing Genes:", algs, "-", reps)
    data.train <- npcp %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Samples") {
    cli::cat_line("Normalizing Samples:", algs, "-", reps)
    data.train <- npcp %>%
      t() %>%
      diceR::prepare_data(scale = TRUE, min.var = minVar, type = norm.type) %>%
      t() %>%
      as.data.frame()
  }

  cli::cat_line("Running training algorithms:", algs, "-", reps)
  # train algorithms
  reps <- as.integer(reps)
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
    # ldaRfe = purrr::invoke(splendid::splendid_model, sm_args,
    #                        algorithms = "lda", rfe = TRUE),
    # rfRfe = purrr::invoke(splendid::splendid_model, sm_args,
    #                       algorithms = "rf", rfe = TRUE),
    # lassoRfe = purrr::invoke(splendid::splendid_model, sm_args,
    #                          algorithms = "mlr_lasso", rfe = TRUE),
    # svmRfe = purrr::invoke(splendid::splendid_model, sm_args,
    #                        algorithms = "svm", rfe = TRUE)
  )

  cli::cat_line("Saving output:", algs, "-", reps, "\n")
  # write to file
  readr::write_rds(sm, outputFile)
}
