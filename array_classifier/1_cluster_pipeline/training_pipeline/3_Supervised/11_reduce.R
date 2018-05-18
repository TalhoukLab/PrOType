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
reduce_supervised <- function(dataSet, alg, outDir, fname = "Model") {
  # Store data_directory path
  dirpath <- paste0(outDir, dataSet, "/", fname, "_", dataSet, "/")

  # grep files in directory matching pattern
  files.in <- grep(
    pattern = paste("c1", alg, sep = "_"),
    x = list.files(dirpath),
    value = TRUE
  )

  # import data into memory
  files.read <- paste0(dirpath, files.in) %>%
    purrr::map(readr::read_rds) %>%
    purrr::transpose()

  # compute median + 95% confidence interval
  reduced <- files.read$evals %>%
    purrr::transpose() %>%
    purrr::map(~ apply(data.frame(.), 1, quantile, c(0.5, 0.05, 0.95), na.rm = TRUE))

  # write to file
  readr::write_rds(reduced, paste0(dirpath, alg, "_train_eval_", dataSet, ".rds"))
}
