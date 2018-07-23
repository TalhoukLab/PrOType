# load libraries
library(magrittr)
library(tidyverse)

# Functions-----

#********************************************************************
# Builds mapping matrix from integer class to correct labels names
buildMapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("Can only relabel C1 XPN")
  }
}

# Load Data  ----
seed <- read_rds(file.path(data_dir, "seed.rds"))

# Training data
npcp_train <- readRDS(file.path(outputDir, trainSet,
                                paste0("data_pr_", trainSet),
                                paste0("npcp-hcNorm_", trainSet, ".rds"))) %>%
  set_colnames(make.names(colnames(.)))

# Testing Data
npcp_test <- readRDS(file.path(outputDir, testSet,
                               paste0("data_pr_", testSet),
                               paste0("npcp-hcNorm_", testSet, ".rds"))) %>%
  set_colnames(make.names(colnames(.)))

lab <- readRDS(file.path(outputDir, trainSet,
                         paste0("data_pr_", trainSet),
                         paste0("/all_clusts_", trainSet, ".rds"))) %>%
  magrittr::extract(, 1) %>%
  data.frame(labs = .) %>%
  dplyr::inner_join(buildMapping(trainSet), by = "labs") %>%
  dplyr::pull(labels)

train <- cbind(npcp_train, lab)

# Fitting Model
algs <- c("adaboost", "mlr_lasso", "mlr_ridge", "rf", "svm")
all_fits <- purrr::map(algs, splendid::classification, data = npcp_subset, class = lab_subset, seed_alg = seed)

# Predicting Model
predsC2 <- all_fits %>%
  purrr::set_names(paste("preds", c("ada", "lasso", "ridge", "rf", "svm"), sep = "_")) %>%
  purrr::map_dfc(splendid::prediction, data = npcp_test_subset, class = lab_subset)
write_rds(predsC2, file.path(outputDir, "predictions", paste0(testSet, ".rds")))
