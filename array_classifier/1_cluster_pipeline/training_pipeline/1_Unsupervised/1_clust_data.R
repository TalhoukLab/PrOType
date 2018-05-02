# Fixed Inputs: cdat, ndat, sfdir, r, k
# Variable Inputs: algs, s

library(diceR)
library(tidyverse)

#' Helper functions for parallel clustering
pl_cluster <- function(data, nk = 4, reps = 100,
                       algorithms = c("nmf", "dist", "other"),
                       nmf.method = c("scd", "lee"), distance = "euclidean",
                       seed.data = 1, dir.name = ".", file.name = NULL, ...) {
  args <- tibble::lst(data, nk, reps = 1, seed.data, ...)
  fs::dir_create(dir.name)
  switch(
    algorithms,
    nmf = {
      algs = "nmf"
      base.name <- paste(algorithms, seed.data, file.name, sep = "_")
      pl_cluster_nmf(args, algs, nmf.method, dir.name, base.name)
    },
    dist = {
      algs = c("hc", "diana", "km", "pam")
      base.name <- paste(algorithms, seed.data, file.name, sep = "_")
      pl_cluster_dist(args, algs, distance, dir.name, base.name)
    },
    other = {
      algs = c("ap", "sc", "gmm", "block", "som", "cmeans")
      base.name <- paste(algorithms, seed.data, file.name, sep = "_")
      pl_cluster_other(args, algs, dir.name, base.name)
    }
  )
}

pl_cluster_nmf <- function(args, algorithms, nmf.method, dir.name, base.name) {
  purrr::invoke(
    consensus_cluster,
    args,
    algorithms = algorithms,
    nmf.method = nmf.method,
    file.name = file.path(dir.name, base.name)
  )
}

pl_cluster_dist <- function(args, algorithms, distance, dir.name, base.name) {
  purrr::invoke(
    consensus_cluster,
    args,
    algorithms = algorithms,
    distance = distance,
    file.name = file.path(dir.name, base.name)
  )
}

pl_cluster_other <- function(args, algorithms, dir.name, base.name) {
  purrr::invoke(
    consensus_cluster,
    args,
    algorithms = algorithms,
    file.name = file.path(dir.name, base.name)
  )
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
