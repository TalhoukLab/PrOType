# Fixed Inputs: cdat, ndat, sfdir, r, k
# Variable Inputs: algs, s

library(diceR)
library(tidyverse)

#' Helper functions for parallel clustering
pl_cluster <- function(data, nk = 4, reps = 1000,
                       algorithms = c("NALG", "DALG", "OALG"),
                       nmf.method = c("scd", "lee"), distance = "euclidean",
                       seed.data = 1, dir.name = ".", file.name = NULL, ...) {
  args <- tibble::lst(data, nk, reps = 1, ...)
  fs::dir_create(dir.name)
  switch(
    algorithms,
    NALG = {
      algs = "nmf"
      pl_cluster_nmf(args, seed.data, algs, nmf.method, dir.name, file.name)
    },
    DALG = {
      algs = c("hc", "diana", "km", "pam")
      pl_cluster_dist(args, seed.data, algs, distance, dir.name, file.name)
    },
    OALG = {
      algs = c("ap", "sc", "gmm", "block", "som", "cmeans")
      pl_cluster_other(args, seed.data, algs, dir.name, file.name)
    }
  )
}

pl_cluster_nmf <- function(args, seed.data, algorithms, nmf.method, dir.name,
                           file.name) {
  purrr::map(nmf.method, function(m) {
    base.name <- paste(algorithms, m, seed.data, file.name, sep = "_")
    purrr::invoke(
      consensus_cluster,
      args,
      algorithms = algorithms,
      nmf.method = m,
      seed.data = seed.data,
      file.name = file.path(dir.name, base.name)
    )
  })
}

pl_cluster_dist <- function(args, seed.data, algorithms, distance, dir.name,
                            file.name) {
  purrr::map(algorithms, function(a) {
    purrr::map(distance, function(d) {
      base.name <- paste(a, d, seed.data, file.name, sep = "_")
      purrr::invoke(
        consensus_cluster,
        args,
        algorithms = a,
        distance = d,
        seed.data = seed.data,
        file.name = file.path(dir.name, base.name)
      )
    })
  })
}

pl_cluster_other <- function(args, seed.data, algorithms, dir.name, file.name) {
  purrr::map(algorithms, function(a) {
    base.name <- paste(a, seed.data, file.name, sep = "_")
    purrr::invoke(
      consensus_cluster,
      args,
      algorithms = a,
      seed.data = seed.data,
      file.name = file.path(dir.name, base.name)
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
