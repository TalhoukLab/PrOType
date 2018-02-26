###############################################################
###### STEP 1: Fit Models on Cut 1                       ######
###############################################################

suppressPackageStartupMessages({
  source("Rscripts/NanostringEvals.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

# import training data
cut1.dat <- c("ov.afc1_xpn", "ov.afc1_cbt")
cut1 <- purrr::map(cut1.dat, function(x) {
    import_study("./data/", study = x) 
  }) %>% set_names(cut1.dat)

seed <- readRDS("./data/seed.rds")
# Train Adaboost on XPN data
.Random.seed <- seed
adaboost_xpn <- train(cut1$ov.afc1_xpn, alg = "adaboost");
readr::write_rds(adaboost_xpn, path = paste0("outputs/fits/ov.afc1_xpn_adaboost.rds"));

# Train RF on XPN data
.Random.seed <- seed
rf_xpn <- train(cut1$ov.afc1_xpn, alg = "rf");
readr::write_rds(rf_xpn, path = paste0("outputs/fits/ov.afc1_xpn_rf.rds"));

# Train Lasso on XPN data
.Random.seed <- seed
lasso_xpn <- train(cut1$ov.afc1_xpn, alg = "mlr_lasso");
readr::write_rds(lasso_xpn, path = paste0("outputs/fits/ov.afc1_xpn_lasso.rds"));

# Train Ridge on XPN data
.Random.seed <- seed
ridge_xpn <- train(cut1$ov.afc1_xpn, alg = "mlr_ridge");
readr::write_rds(ridge_xpn, path = paste0("outputs/fits/ov.afc1_xpn_ridge.rds"));

# Train Adaboost on CBT data
.Random.seed <- seed
adaboost_CBT <- train(cut1$ov.afc1_cbt, alg = "adaboost");
readr::write_rds(adaboost_CBT, path = paste0("outputs/fits/ov.afc1_CBT_adaboost.rds"));

# Train RF on CBT data
.Random.seed <- seed
rf_CBT <- train(cut1$ov.afc1_cbt, alg = "rf");
readr::write_rds(rf_CBT, path = paste0("outputs/fits/ov.afc1_CBT_rf.rds"));

# Train Lasso on CBT data
.Random.seed <- seed
lasso_CBT <- train(cut1$ov.afc1_cbt, alg = "mlr_lasso");
readr::write_rds(lasso_CBT, path = paste0("outputs/fits/ov.afc1_CBT_lasso.rds"));

# Train Ridge on CBT data
.Random.seed <- seed
ridge_CBT <- train(cut1$ov.afc1_cbt, alg = "mlr_ridge");
readr::write_rds(ridge_CBT, path = paste0("outputs/fits/ov.afc1_CBT_ridge.rds"));

fit.c1 <- list(adaboost_xpn,rf_xpn,lasso_xpn,ridge_xpn) %>% set_names(c("adaboost_xpn","rf_xpn","lasso_xpn","ridge_xpn"))
readr::write_rds(fit.c1, "outputs/fits/all_fits.rds") 

