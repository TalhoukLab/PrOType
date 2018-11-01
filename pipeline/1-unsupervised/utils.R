source(here::here("assets/utils.R"))

#' Default hc from diceR but with data matrix as input
#' @param x data matrix
#' @param k number of clusters
#' @param method method of hierarchical clustering
hc <- function(x, k, method = "average") {
  diceR:::hc(stats::dist(x), k = k, method = method)
}

#' Extract seed values used as reps for file names
#' @param fnames sorted file names
#' @param alg name of algorithm
#' @param dataset name of dataset
seed_from_file <- function(fnames, alg, dataset) {
  fnames %>%
    basename() %>%
    gsub(paste0(".*", alg, "([[:digit:]]+)_", dataset, ".rds"), "\\1", .) %>%
    as.numeric()
}
