# Fixed Inputs: cdat, ndat, sfdir, r, k
# Variable Inputs: algs, s

library(diceR)
library(tidyverse)

# Categories of algorithms
NALG <- "nmf"
DALG <- c("hc", "diana", "km", "pam")
OALG <- c("ap", "sc", "gmm", "block", "som", "cmeans")

#' Helper functions for parallel clustering
pl_cluster <- function(data, nk = 4, reps = 1000, algorithms = NULL,
                       nmf.method = c("brunet", "lee"), distance = "euclidean",
                       seed.data = 1, dir.name = ".", file.name = NULL, ...) {
  args <- tibble::lst(data, nk, reps = 1, ...)
  ALG <- tibble::lst(NALG, DALG, OALG)
  algorithms <- algorithms %||% purrr::flatten_chr(ALG)
  algs <- purrr::map(ALG, ~ algorithms[algorithms %in% .])
  fs::dir_create(dir.name)
  if (length(algs$NALG) > 0) {
    pl_cluster_nmf(args, seed.data, algs$NALG, nmf.method, dir.name, file.name)
  }
  if (length(algs$DALG) > 0) {
    pl_cluster_dist(args, seed.data, algs$DALG, distance, dir.name, file.name)
  }
  if (Length(algs$OALG) > 0) {
    pl_cluster_other(args, seed.data, algs$OALG, dir.name, file.name)
  }
}

pl_cluster_nmf <- function(args, seed.data, algorithms, nmf.method, dir.name,
                           file.name) {
  purrr::map(nmf.method, function(m) {
    purrr::invoke(
      consensus_cluster,
      args,
      algorithms = algorithms,
      nmf.method = m,
      seed.data = seed.data,
      file.name = file.path(dir.name,
                            paste(algorithms, m, seed.data, file.name,
                                  sep = "_"))
    )
  })
}

pl_cluster_dist <- function(args, seed.data, algorithms, distance, dir.name,
                            file.name) {
  purrr::map(algorithms, function(a) {
    purrr::map(distance, function(d) {
      purrr::invoke(
        consensus_cluster,
        args,
        algorithms = a,
        distance = d,
        seed.data = seed.data,
        file.name = file.path(dir.name,
                              paste(a, d, seed.data, file.name, sep = "_"))
      )
    })
  })
}

pl_cluster_other <- function(args, seed.data, algorithms, dir.name, file.name) {
  purrr::map(algorithms, function(a) {
    purrr::invoke(
      consensus_cluster,
      args,
      algorithms = a,
      seed.data = seed.data,
      file.name = file.path(dir.name,
                            paste(a, seed.data, file.name, sep = "_"))
    )
  })
}

# Run parallel clustering and save objects
k <- 4
r <- 1

ssclust <- pl_cluster(
  data = cdat,
  nk = k,
  reps = r,
  algorithms = algs,
  distance = c("euclidean", "spearman", "manhattan"),
  prep.data = "none",
  seed.data = s,
  save = TRUE,
  dir.name = paste0(sfdir, "/rds_out_", ndat),
  file.name = ndat
)
