`%>%` <- magrittr::`%>%`

# Merge consensus matrices across all seeds
multMergeCM <- function(path, alg) {
  list.files(path = path, pattern = alg, full.names = TRUE) %>%
    gtools::mixedsort() %>%
    purrr::map(readRDS) %>%
    purrr::transpose() %>%
    purrr::map(Reduce, f = `+`)
}

# Path of partially merged consensus matrices
consmat_path <- file.path(outputdir, "unsupervised", "merge_consmat",
                          paste0("con_mat_merged_", dataset))

# Flatten after merge to get one consensus matrix per algorithm
consmatF <- algs %>%
  purrr::map(multMergeCM, path = consmat_path) %>%
  purrr::flatten()

# Write to file
saveRDS(consmatF, file.path(outputdir, "unsupervised", "merge",
                            paste0("data_pr_", dataset),
                            paste0("Final_CM_", dataset, ".rds")))
