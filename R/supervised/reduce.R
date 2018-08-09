#' Mapping to Nanostring
#'
#' This script evaluates all the models that were trained on 100 bootstrap
#' samples and saves the output in a list of data.frame with the median across
#' all bootstrap samples for each data set. i.e. 24 indices, 11 algorithms, 100
#' bootstraps.
#'
#' @param dataSet is a data frame with feature columns and samples as rows
#' @param alg an algorithm supported by Splendid
#' @param outDir directory specifying the output location
#' @return a data frame containing median and 95% confidence interval
#' @author Last updated on 30/10/2017 by Dustin Johnson. Edited by Derek Chiu.
reduce_supervised <- function(dataSet, alg, outDir, fname = "Model", threshold = 0.0) {
  # Store data_directory path
  dirpath <- file.path(outDir, dataSet, paste0(fname, "_", dataSet, "/"))

  # grep files in directory matching pattern
  files.in <- list.files(
    path = dirpath,
    pattern = paste0("c1_", alg, "[0-9]+_", dataSet, ".rds"),
    full.names = TRUE
  )

  # import data into memory, keeping only evaluations
  files.read <- purrr::map(files.in, readRDS)

  # transpose evaluation measures to group by algorithm
  reduced <- purrr::transpose(files.read)

  # compute median + 95% confidence interval
  reduced_quantiles <- reduced %>%
    purrr::map(~ apply(data.frame(.), 1, quantile, c(0.5, 0.05, 0.95), na.rm = TRUE))

  # write to file
  outputFile <- file.path(dirpath, paste0(alg, "_train_eval_", dataSet, ".rds"))
  readr::write_rds(reduced_quantiles, outputFile)
}
