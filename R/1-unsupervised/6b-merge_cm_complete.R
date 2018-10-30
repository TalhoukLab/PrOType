source(here::here("R/1-unsupervised/utils.R"))

# Merge consensus matrices across all seeds
multMergeCM <- function(path, alg) {
  list.files(path = path, pattern = alg, full.names = TRUE) %>%
    purrr::map(readRDS) %>%
    purrr::transpose() %>%
    purrr::map(Reduce, f = `+`)
}

# Path of partially merged consensus matrices
consmat_path <- file.path(outputDir, "unsupervised", "merge_cm", dataset)

# Flatten after merge to get one consensus matrix per algorithm
consmatF <- algs %>%
  purrr::map(multMergeCM, path = consmat_path) %>%
  purrr::flatten()

# Write to file
saveRDS(consmatF, file.path(outputDir, "unsupervised", "data_pr", dataset,
                            paste0("Final_CM_", dataset, ".rds")))
