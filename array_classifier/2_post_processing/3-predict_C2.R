# load libraries
library(magrittr)
library(tidyverse)

# Directories----
# intermediate <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/intermediate/"
# processed <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/processed/"
# output <- "/Users/atalhouk/Repositories/NanoString/HGSCS/Results/"
# v <- "outputs_Feb10_2018/"

# Functions-----
buildMapping <- function(train.set)
                         #********************************************************************
                         # Builds mapping matrix from integer class to correct labels names
#********************************************************************
{
  # label mapping
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_xpn") {
    map <- data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("Can only relabel C1 XPN")
  }

  return(map)
}

# Load Data  ----
seed <- read_rds(paste0(data_dir, "seed.rds"))

# Training data
map <- buildMapping(trainSet)
npcp_train <- readRDS(file.path(outputDir, trainSet,
                          paste0("data_pr_", trainSet),
                          paste0("npcp-hcNorm_", trainSet, ".rds")))
npcp_test <- readRDS(file.path(outputDir, testSet,
                               paste0("data_pr_", testSet),
                               paste0("npcp-hcNorm_", testSet, ".rds")))

colnames(npcp_train) <- make.names(colnames(npcp_train))
colnames(npcp_test) <- make.names(colnames(npcp_test))

train <- cbind(npcp_train, lab = readRDS(file.path(outputDir, trainSet,
                                                  paste0("data_pr_", trainSet),
                                                  paste0("/all_clusts_", trainSet, ".rds")))[, 1] %>%
  data.frame(labs = .) %>%
  dplyr::inner_join(map, by = "labs") %>%
  .$labels)

# Fitting Model
# TODO:// Can this be factored? -- algorithms <- c("adaboost", "mlr_lasso", "mlr_ridge", "rf")
set.seed(seed)
fit_ada <- splendid::classification(npcp, class = train$lab, algorithms = "adaboost")

set.seed(seed)
fit_lasso <- splendid::classification(npcp, class = train$lab, algorithms = "mlr_lasso")

set.seed(seed)
fit_ridge <- splendid::classification(npcp, class = train$lab, algorithms = "mlr_ridge")

set.seed(seed)
fit_rf <- splendid::classification(npcp, class = train$lab, algorithms = "rf")

# Testing data
preds_ada <- splendid::prediction(fit_ada, data = npcp_test, class = NULL)
preds_lasso <- splendid::prediction(fit_lasso, data = npcp_test, class = NULL)
preds_ridge <- splendid::prediction(fit_ridge, data = npcp_test, class = NULL)
preds_rf <- splendid::prediction(fit_rf, data = npcp_test, class = NULL)

predsC2 <- data_frame(preds_ada, preds_lasso, preds_rf, preds_ridge)

write_rds(predsC2, file.path(outputDir, "predictions", paste0(testSet, ".rds"))
