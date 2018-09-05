`%>%` <- magrittr::`%>%`
source(here::here("assets/utils.R"))

#' Default hc from diceR but with data matrix as input
#' @param x data matrix
#' @param k number of clusters
#' @param method method of hierarchical clustering
hc <- function(x, k, method = "average") {
  diceR:::hc(stats::dist(x), k = k, method = method)
}

#' Sort file names numerically
#'
#' By default numbers are sorted lexicographically so sorting occurs like 1, 10,
#' 2, 3, ... To sort numerically we use `gtools::mixedsort()`.
#' @param path file path
#' @param ... additional arguments to pass to `list.files`
sort_filenames <- function(path = ".", ...) {
  gtools::mixedsort(list.files(path = path, ...))
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
