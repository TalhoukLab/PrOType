source(here::here("R/1-unsupervised/utils.R"))

# Extract seeds to merge
seeds_merge <- (r * c - c + 1):(r * c)

# Merge seeds of consensus matrices
consmat <- file.path(outputDir, "unsupervised", "consmat", dataset,
                     paste0("cm_", alg, seeds_merge, "_", dataset, ".rds")) %>%
  purrr::map(~ readRDS(.)[[k]]) %>%
  purrr::modify_depth(2, Matrix::as.matrix) %>%
  purrr::transpose() %>%
  purrr::map(Reduce, f = `+`)

# Write to file
saveRDS(consmat, file.path(outputDir, "unsupervised", "merge_cm", dataset,
                           paste0("merged_cm_", alg, r, "_", dataset, ".rds")))
