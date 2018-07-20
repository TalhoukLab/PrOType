# ADD SVM

# Load packages
rm(list = ls(all = TRUE))
library(magrittr)
library(tidyverse)

# Directories----
intermediate <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/intermediate/"
processed <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/processed/"
output <- "/Users/atalhouk/Repositories/NanoString/HGSCS/Results/"
v <- "outputs_Feb10_2018/"

# Functions-----

#********************************************************************
# Builds mapping matrix from integer class to correct labels names
#********************************************************************
buildMapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("Can only relabel C1 XPN")
  }
}

# Load Data----
seed <- read_rds(paste0(intermediate, v, "seed.rds"))

# Training data
trainSet <- "ov.afc1_xpn"
map <- buildMapping(trainSet)
npcp <- readRDS(paste0(intermediate, v,
                       trainSet, "/npcp-hcNorm_", trainSet, ".rds")) %>%
  set_colnames(make.names(colnames(.)))

lab <- readRDS(paste0(intermediate, v,
                      trainSet, "/all_clusts_", trainSet, ".rds")) %>%
  magrittr::extract(, 1) %>%
  data.frame(labs = .) %>%
  dplyr::inner_join(map, by = "labs") %>%
  dplyr::pull(labels)

# Fitting Model
algs <- c("adaboost", "mlr_lasso", "mlr_ridge", "rf", "svm")
all_fits <- purrr::map(algs, splendid::classification, data = npcp_subset, class = lab_subset, seed_alg = seed)

# Testing Data
testSet <- "ov.afc2_xpn"
npcp_test <- readRDS(paste0(intermediate, v,
                            testSet, "/npcp-hcNorm_", testSet, ".rds")) %>%
  set_colnames(make.names(colnames(.)))

# Predicting Model
predsC2 <- all_fits %>%
  purrr::set_names(paste("preds", c("ada", "lasso", "ridge", "rf", "svm"), sep = "_")) %>%
  purrr::map_dfc(splendid::prediction, data = npcp_test_subset, class = lab_subset)
write_rds(predsC2, paste0(intermediate, v, testSet, "/preds_ov.afc2_xpn.rds"))
