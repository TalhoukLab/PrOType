`%>%` <- magrittr::`%>%`

# All raw clustering objects
fnames <- list.files(path = file.path(outputDir, "unsupervised", "clustering",
                                      paste0("rds_out_", dataset, "/"))) %>%
  gtools::mixedsort()

# Extract seeds to merge
algF <- grep(algs, fnames, value = TRUE)
seeds <- as.numeric(
  gsub(paste0(algs, "([[:digit:]]+)_", dataset, ".rds"), "\\1", algF)
)
seeds_merge <- (r * c - c + 1):(r * c)
part_complete <- seeds[seeds %in% seeds_merge]

# Merge seeds of consensus matrices
consmat <- file.path(outputDir, "unsupervised", "clustering",
                     paste0("con_mat_", dataset),
                     paste0("CM_", algs, part_complete, "_", dataset, ".rds")) %>%
  purrr::map(~ readRDS(.)[[k]]) %>%
  purrr::modify_depth(2, Matrix::as.matrix) %>%
  purrr::transpose() %>%
  purrr::map(Reduce, f = `+`)

# Write to file
saveRDS(consmat, file.path(outputDir, "unsupervised", "merge_consmat",
                           paste0("con_mat_merged_", dataset),
                           paste0(r, "_", algs, "_consmat_", dataset, ".rds")))
