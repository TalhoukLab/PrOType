###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

suppressPackageStartupMessages({
  source("array_classifier/2_post_processing/utils/BuildFinalModel.R")
  source("array_classifier/2_post_processing/utils/build_mapping.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

seed<- readRDS("array_classifier/2_post_processing/data/seed.rds")

# import training data
train.dat.names <- c("ov.afc1_xpn", "ov.afc1_cbt")
traindat <- purrr::map(train.dat.names, function(x) {
  import_study("array_classifier/2_post_processing/data/", study = x) 
}) %>% set_names(cut1.dat)


# fit algos to cut 1
algos <- c("mlr_ridge", "mlr_lasso", "adaboost", "rf")
fit.c1 <- purrr::map2(traindat, train.dat.names, function(x, y) {
  purrr::map(algos, function(z) {
    #comment below to ignore individual fits
    fit <- train_final(x, alg = z);
    readr::write_rds(fit, path = paste0("array_classifier/2_post_processing/outputs/fits/", y, "_", z, ".rds"));
    return(fit)
  }) %>% purrr::set_names(algos)
}) %>% purrr::set_names(cut1.dat)

readr::write_rds(fit.c1, "array_classifier/2_post_processing/outputs/fits/all_fits.rds")
