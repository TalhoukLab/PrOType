library(diceR)

# Categories of algorithms
NALG <- "nmf"
DALG <- c("hc", "diana", "km", "pam")
OALG <- c("ap", "sc", "gmm", "block", "som", "cmeans", "hdbscan")

#' Helper functions for parallel clustering
pl_cluster <- function(data, nk = 4, reps = 1000, algorithms = NULL,
                       nmf.method = c("brunet", "lee"), distance = "euclidean",
                       file.name = NULL, ...) {
  args <- dplyr::lst(data, nk, reps = 1, ...)
  r <- seq_len(reps)
  algs <- dplyr::lst(NALG, DALG, OALG) %>%
    purrr::map(~ algorithms[algorithms %in% .])
  fs::dir_create("raw")
  pl_cluster_nmf(args, r, algs$NALG, nmf.method, file.name)
  pl_cluster_dist(args, r, algs$DALG, distance, file.name)
  pl_cluster_other(args, r, algs$OALG, file.name)
}

pl_cluster_nmf <- function(args, reps, algorithms, nmf.method, file.name) {
  if (length(algorithms) > 0) {
    purrr::map(nmf.method, function(m) {
      purrr::map(reps, function(r) {
        purrr::invoke(
          consensus_cluster,
          args,
          algorithms = algorithms,
          nmf.method = m,
          seed.data = r,
          file.name = paste0("raw/",
                             paste("raw", algorithms, m, r, file.name, sep = "_"))
        )
      })
    })
  }
}

pl_cluster_dist <- function(args, reps, algorithms, distance, file.name) {
  if (length(algorithms) > 0) {
    purrr::map(algorithms, function(a) {
      purrr::map(distance, function(d) {
        purrr::map(reps, function(r) {
          purrr::invoke(
            consensus_cluster,
            args,
            algorithms = a,
            seed.data = r,
            file.name = paste0("raw/",
                               paste("raw", a, d, r, file.name, sep = "_"))
          )
        })
      })
    })
  }
}

pl_cluster_other <- function(args, reps, algorithms, file.name) {
  if (length(algorithms) > 0) {
    purrr::map(algorithms, function(a) {
      purrr::map(reps, function(r) {
        purrr::invoke(
          consensus_cluster,
          args,
          algorithms = a,
          seed.data = r,
          file.name = paste0("raw/",
                             paste("raw", a, r, file.name, sep = "_"))
        )
      })
    })
  }
}

# Run parallel clustering and save objects
ssclust <- pl_cluster(
  data = cdat,
  nk = 4,
  reps = 1,
  distance = c("euclidean", "spearman", "manhattan"),
  prep.data = "none",
  seed.data = 1,
  save = TRUE,
  file.name = "cc_out"
)
