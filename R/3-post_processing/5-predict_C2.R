# Predict C2 --------------------------------------------------------------

# Load utility functions
source(here::here("R/3-post_processing/utils/utils.R"))

# Test data
npcp_test <- readRDS(file.path(outputDir, "genemapping", testSet,
                               paste0("npcp-hcNorm_", testSet, ".rds"))) %>%
  magrittr::set_colnames(make.names(colnames(.)))

# Reference Training labels
stopifnot(trainSet == "ov.afc1_xpn")
mapping <- build_mapping(trainSet)
lab <- readRDS(file.path(outputDir, "unsupervised", "final", trainSet,
                         paste0("all_clusts_", trainSet, ".rds"))) %>%
  dplyr::select(labs = "kmodes") %>%
  dplyr::inner_join(mapping, by = "labs") %>%
  dplyr::pull(labels)

# Load models fitted on training set
all_fits <- list.files(path = file.path(outputDir, "fits"),
                       full.names = TRUE) %>%
  purrr::set_names(gsub(trainSet, "preds", basename(.))) %>%
  purrr::set_names(gsub("\\.rds", "", names(.))) %>%
  purrr::map(readRDS)

# Make predictions on test set
preds_c2 <- all_fits %>%
  purrr::map_dfc(splendid::prediction, data = npcp_test, class = lab)
saveRDS(preds_c2, file.path(outputDir, "post_processing", "predictions",
                            paste0(testSet, ".rds")))
