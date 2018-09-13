#' Mapping to Nanostring
#'
#' This script evaluates all the models that were trained on 100 bootstrap
#' samples and saves the output in a list of data.frame with the median across
#' all bootstrap samples for each data set. i.e. 24 indices, 11 algorithms, 100
#' bootstraps.
#'
#' @param dataset is a data frame with feature columns and samples as rows
#' @param alg an algorithm supported by Splendid
#' @param outputDir directory specifying the output location
#' @return a data frame containing median and 95% confidence interval
#' @author Last updated on 30/10/2017 by Dustin Johnson. Edited by Derek Chiu.
`%>%` <- magrittr::`%>%`
reduce_supervised <- function(dataset, alg, outputDir, fname = "Model",
                              threshold = 0.0) {
  # Evaluations for each rep of alg
  files <- list.files(
    path = file.path(outputDir, "supervised", "train",
                     paste0(fname, "_", dataset)),
    pattern = paste0(alg, "[0-9]+_", dataset, ".rds"),
    full.names = TRUE
  )
  # Compute median + 95% CI for evaluations merged by algorithm
  eval_quantiles <- files %>%
    purrr::map(readRDS) %>%
    purrr::transpose() %>%
    purrr::map(~ apply(data.frame(.), 1, quantile, c(0.5, 0.05, 0.95),
                       na.rm = TRUE))
  # Write evaluations merged by algorithm
  outputFile <- file.path(outputDir, "supervised", "reduce",
                          paste0(fname, "_", dataset),
                          paste0(alg, "_train_eval_", dataset, ".rds"))
  saveRDS(eval_quantiles, outputFile)
}
