source(here::here("R/1-unsupervised/utils.R"))

obj_name <- paste0(alg, s, "_", dataset, ".rds")

E <- readRDS(file.path(outputDir, "unsupervised", "clustering", "raw_clust",
                       dataset, paste0("E_", obj_name)))

consmat <- diceR::consensus_combine(E, element = "matrix") %>%
  purrr::modify_depth(2, ~ Matrix::Matrix(., sparse = TRUE))

saveRDS(consmat, file.path(outputDir, "unsupervised", "consmat", dataset,
                           paste0("cm_", obj_name)))
