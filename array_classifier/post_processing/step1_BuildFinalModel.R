###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

suppressPackageStartupMessages({
  source("utils/BuildFinalModel.R")
  source("utils/build_mapping.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

seed <- readRDS("./data/seed.rds")

# import training data
cut1.dat <- c("ov.afc1_xpn", "ov.afc1_cbt")
cut1 <- purrr::map(cut1.dat, function(x) {
  import_study("./data/", study = x) 
}) %>% set_names(cut1.dat)


# fit algos to cut 1
algos <- c("mlr_ridge", "mlr_lasso", "adaboost", "rf")
fit.c1 <- purrr::map2(cut1, cut1.dat, function(x, y) {
  purrr::map(algos, function(z) {
    #uncomment below to write to file
    #fit <- train(x, alg = z);
    #readr::write_rds(fit, path = paste0("outputs/fits/", y, "_", z, ".rds"));
    #return(fit)
    train(x, alg = z);
  }) %>% purrr::set_names(algos)
}) %>% purrr::set_names(cut1.dat)
readr::write_rds(fit.c1, "outputs/fits/all_fits.rds")