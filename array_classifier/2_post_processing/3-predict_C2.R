# Predict C2 --------------------------------------------------------------
# Load utility functions
library(magrittr)
source(here::here("array_classifier/2_post_processing/utils/utils.R"))

# Load seeds
seed <- readRDS(file.path(dataDir, "seed.rds"))

# Training data
npcp_train <- readRDS(file.path(outputDir, trainSet,
                                paste0("data_pr_", trainSet),
                                paste0("npcp-hcNorm_", trainSet, ".rds"))) %>%
  magrittr::set_colnames(make.names(colnames(.)))

# Testing data
npcp_test <- readRDS(file.path(outputDir, testSet,
                               paste0("data_pr_", testSet),
                               paste0("npcp-hcNorm_", testSet, ".rds"))) %>%
  magrittr::set_colnames(make.names(colnames(.)))

# Training labels
lab <- readRDS(file.path(outputDir, trainSet,
                         paste0("data_pr_", trainSet),
                         paste0("/all_clusts_", trainSet, ".rds"))) %>%
  magrittr::extract(, 1) %>%
  data.frame(labs = .) %>%
  dplyr::inner_join(build_mapping_xpn(trainSet), by = "labs") %>%
  dplyr::pull(labels)

# Fitting model on training set
algs <- c("adaboost", "mlr_lasso", "mlr_ridge", "rf", "svm")
all_fits <- algs %>%
  purrr::map(splendid::classification,
             data = npcp_train, class = lab, seed_alg = seed)

# Predicting model on test set
predsC2 <- all_fits %>%
  purrr::set_names(paste("preds", c("ada", "lasso", "ridge", "rf", "svm"),
                         sep = "_")) %>%
  purrr::map_dfc(splendid::prediction, data = npcp_test, class = lab)
saveRDS(predsC2, file.path(outputDir, "predictions", paste0(testSet, ".rds")))
