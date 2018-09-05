source(here::here("R/1-unsupervised/utils.R"))

multMerge <- function(path, alg, dataset) {
  # Extract seeds to merge
  fnames <- sort_filenames(path = path, pattern = alg, full.names = TRUE)
  seeds <- seed_from_file(fnames, alg, dataset)

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
writeDir <- file.path(outputDir, "unsupervised", "merge",
                      paste0("data_pr_", dataset))

# Merge the raw clustering
cli::cat_line("Merging raw clustering")
raw_path <- file.path(outputDir, "unsupervised", "clustering",
                      paste0("rds_out_", dataset))
E <- algs %>%
  purrr::map(multMerge, path = raw_path, dataset = dataset) %>%
  abind::abind(along = 3)
saveRDS(E, file = file.path(writeDir, paste0("E_", dataset, ".rds")))

# Merge KNN-imputed clustering
cli::cat_line("Merging KNN-imputed clustering")
imputed_path <- file.path(outputDir, "unsupervised", "clustering",
                          paste0("imputed_clust_", dataset))
E_knn <- algs %>%
  purrr::map(multMerge, path = imputed_path, dataset = dataset) %>%
  abind::abind(along = 3)
saveRDS(E_knn, file = file.path(writeDir, paste0("E_knn_", dataset, ".rds")))

# Complete clustering by majority vote imputation
cli::cat_line("Complete clustering")
cdat <- readRDS(file.path(outputDir, "unsupervised", "prep_data", dataset,
                          paste0("cdat_", dataset, ".rds")))
Ecomp <- diceR::impute_missing(E_knn, data = cdat, nk = k)
saveRDS(Ecomp, file = file.path(writeDir, paste0("Ecomp_", dataset, ".rds")))
