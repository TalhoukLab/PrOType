# load libraries
rm(list = ls(all = TRUE))
library(magrittr)
library(tidyverse)

# Directories----
intermediate <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/intermediate/"
processed <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/processed/"
output <- "/Users/atalhouk/Repositories/NanoString/HGSCS/Results/"
v <- "outputs_Feb10_2018/"

# Functions-----
# Functions----
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
seed <- read_rds(paste0(intermediate, v, "seed.rds"))

# Training data
trainSet <- "ov.afc1_xpn"
map <- buildMapping(trainSet)
npcp <- readRDS(paste0(
  intermediate, v,
  trainSet, "/npcp-hcNorm_", trainSet, ".rds"
))
colnames(npcp) <- make.names(colnames(npcp))

train <- cbind(npcp, lab = readRDS(paste0(
  intermediate, v,
  trainSet, "/all_clusts_",
  trainSet, ".rds"
))[, 1] %>%
  data.frame(labs = .) %>%
  dplyr::inner_join(map, by = "labs") %>%
  .$labels)

# Fitting Model

set.seed(seed)
fit_ada <- splendid::classification(npcp, class = train$lab, algorithms = "adaboost")

set.seed(seed)
fit_lasso <- splendid::classification(npcp, class = train$lab, algorithms = "mlr_lasso")

set.seed(seed)
fit_ridge <- splendid::classification(npcp, class = train$lab, algorithms = "mlr_ridge")

set.seed(seed)
fit_rf <- splendid::classification(npcp, class = train$lab, algorithms = "rf")

# Testing Data
# Training data
testSet <- "ov.afc2_xpn"
npcp_test <- readRDS(paste0(
  intermediate, v,
  testSet, "/npcp-hcNorm_", testSet, ".rds"
))
colnames(npcp_test) <- make.names(colnames(npcp_test))

preds_ada <- splendid::prediction(fit_ada, data = npcp_test, class = NULL)
preds_lasso <- splendid::prediction(fit_lasso, data = npcp_test, class = NULL)
preds_ridge <- splendid::prediction(fit_ridge, data = npcp_test, class = NULL)
preds_rf <- splendid::prediction(fit_rf, data = npcp_test, class = NULL)

predsC2 <- data_frame(preds_ada, preds_lasso, preds_rf, preds_ridge)

write_rds(predsC2, paste0(intermediate, v, testSet, "/preds_ov.afc2_xpn.rds"))
