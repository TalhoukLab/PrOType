source(here::here("R/1-unsupervised/utils.R"))

# Path with raw clustering objects
raw_path <- file.path(outputDir, "unsupervised", "clustering",
                      paste0("rds_out_", dataset))

# Extract seeds to merge
fnames <- sort_filenames(path = raw_path, pattern = alg, full.names = TRUE)
seeds <- seed_from_file(fnames, alg, dataset)
seeds_merge <- (r * c - c + 1):(r * c)
part_complete <- seeds[seeds %in% seeds_merge]

# Merge seeds of consensus matrices
consmat <- file.path(outputDir, "unsupervised", "clustering",
                     paste0("con_mat_", dataset),
                     paste0("CM_", alg, part_complete, "_", dataset, ".rds")) %>%
  purrr::map(~ readRDS(.)[[k]]) %>%
  purrr::modify_depth(2, Matrix::as.matrix) %>%
  purrr::transpose() %>%
  purrr::map(Reduce, f = `+`)

# Write to file
saveRDS(consmat, file.path(outputDir, "unsupervised", "merge_consmat",
                           paste0("con_mat_merged_", dataset),
                           paste0(r, "_", alg, "_consmat_", dataset, ".rds")))
