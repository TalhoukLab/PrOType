#' Train Splendid ensemble algorithm
#'
#' Normalizes a given training set and trains the splendid ensemble
#'
#' @param dataset is a data frame with feature columns and samples as rows
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
`%>%` <- magrittr::`%>%`

train_supervised <- function(dataset, algs, reps, inDir, outDir,
                             normalize.by = "None", minVar = 0.5,
                             threshold = 0, norm.type = "conventional",
                             model = "Model", shouldCompute = FALSE) {
  outputFile <- file.path(outDir, "supervised", "train_eval", dataset,
                          paste0(algs, reps, "_", dataset, ".rds"))
  cli::cat_line("Checking previous input: ", outputFile)
  if (file.exists(outputFile) && !shouldCompute) {
    cli::cat_line("File already exists, skipping.")
    quit(status = 0)
  }

  # Import training data and final cluster assignments
  cli::cat_line("Importing training data and final clusters")
  npcp <- readRDS(file.path(outDir, "unsupervised", "map_genes", dataset,
                            paste0("npcp-hcNorm_", dataset, ".rds")))
  class <- readRDS(file.path(outDir, "unsupervised", "final", dataset,
                             paste0("all_clusts_", dataset, ".rds")))
  class.train <- make.names(class[["kmodes"]])

  # Normalization
  if (normalize.by == "None") {
    cli::cat_line("No normalization")
    data.train <- npcp %>%
      diceR::prepare_data(scale = FALSE,
                          min.var = minVar,
                          type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Genes") {
    cli::cat_line("Normalizing data by genes")
    data.train <- npcp %>%
      diceR::prepare_data(scale = TRUE,
                          min.var = minVar,
                          type = norm.type) %>%
      as.data.frame()
  } else if (normalize.by == "Samples") {
    cli::cat_line("Normalizing data by samples")
    data.train <- npcp %>%
      t() %>%
      diceR::prepare_data(scale = TRUE,
                          min.var = minVar,
                          type = norm.type) %>%
      t() %>%
      as.data.frame()
  }

  # Splendid model arguments
  sm_args <- list(
    data = data.train,
    class = class.train,
    n = 1,
    seed_boot = as.integer(reps),
    seed_alg = 1,
    threshold = threshold
  )
  cli::cat_line("Training each supervised learning algorithm")
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

  # Write evaluations to file
  cli::cat_line("Writing model evaluations to file")
  readr::write_rds(sm[["evals"]], outputFile)
}
