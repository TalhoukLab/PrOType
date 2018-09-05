`%>%` <- magrittr::`%>%`

multMerge <- function(alg, dataset, dir) {
  # Numerically sorted file names for alg
  fnames <- list.files(path = dir, pattern = alg, full.names = TRUE) %>%
    gtools::mixedsort()

  # Extract seeds
  seeds <- fnames %>%
    basename() %>%
    gsub(paste0(".*", alg, "([[:digit:]]+)_", dataset, ".rds"), "\\1", .) %>%
    as.numeric()

  # Combine reps that succeeded for alg
  Ealg <- fnames %>%
    purrr::map(readRDS) %>%
    abind::abind(along = 2)
  dimnames(Ealg)[[2]] <- paste0("R", seeds)

  # Check for failed reps
  passed <- seq_len(reps) %in% seeds
  if (!all(passed)) {
    # Print failed seeds
    failed <- which(!passed)
    cli::cat_line(paste(alg, "failed rep:", failed))
    # Create dummy array for failed reps
    Efailed <-
      array(data = NA_real_, dim = replace(dim(Ealg), 2, length(failed)))
    dimnames(Efailed)[[2]] <- paste0("R", failed)
  } else {
    Efailed <- NULL
  }

  # Merge seeds with failed reps as needed
  list(Ealg, Efailed) %>%
    abind::abind(along = 2) %>%
    `[`(, gtools::mixedorder(colnames(.)), , , drop = FALSE)
}

# Write directory
writedir <- file.path(outputdir, "unsupervised", "merge",
                      paste0("data_pr_", dataset))

# Merge the raw clustering
cli::cat_line("Merging raw clustering")
raw_dir <- file.path(outputdir, "unsupervised", "clustering",
                     paste0("rds_out_", dataset))
E <- algs %>%
  purrr::map(multMerge, dataset = dataset, dir = raw_dir) %>%
  abind::abind(along = 3)
saveRDS(E, file = file.path(writedir, paste0("E_", dataset, ".rds")))

# Merge KNN-imputed clustering
cli::cat_line("Merging KNN-imputed clustering")
imputed_dir <- file.path(outputdir, "unsupervised", "clustering",
                         paste0("imputed_clust_", dataset))
E_knn <- algs %>%
  purrr::map(multMerge, dataset = dataset, dir = imputed_dir) %>%
  abind::abind(along = 3)
saveRDS(E_knn, file = file.path(writedir, paste0("E_knn_", dataset, ".rds")))

# Complete clustering by majority vote imputation
cli::cat_line("Complete clustering")
cdat <- readRDS(file.path(outputdir, "unsupervised", "prep_data", dataset,
                          paste0("cdat_", dataset, ".rds")))
Ecomp <- diceR::impute_missing(E_knn, data = cdat, nk = k)
saveRDS(Ecomp, file = file.path(writedir, paste0("Ecomp_", dataset, ".rds")))
