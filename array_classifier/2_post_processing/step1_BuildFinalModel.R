###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

library(here)
source(here("array_classifier/2_post_processing/utils/utils.R"))

# import training data
xpn.dat <- import_study("data", "ov.afc1_xpn")
cbt.dat <- import_study("data", "ov.afc1_cbt")
save_dir <- "outputs/fits"

# fit algo
seed <- readRDS("data/seed.rds")

.Random.seed <- seed
fit.xpn.adaboost <- train_final(xpn.dat, alg = "adaboost", file.name = file.path(save_dir, "ov.afc1_xpn_adaboost.rds"))

.Random.seed <- seed
fit.xpn.rf <- train_final(xpn.dat, alg = "rf", file.name = file.path(save_dir, "ov.afc1_xpn_rf.rds"))

.Random.seed <- seed
fit.xpn.mlr_ridge <- train_final(xpn.dat, alg = "mlr_ridge", file.name = file.path(save_dir, "ov.afc1_xpn_mlr_ridge.rds"))

.Random.seed <- seed
fit.xpn.mlr_lasso <- train_final(xpn.dat, alg = "mlr_lasso", file.name = file.path(save_dir, "ov.afc1_xpn_mlr_lasso.rds"))

.Random.seed <- seed
fit.cbt.adaboost <- train_final(cbt.dat, alg = "adaboost", file.name = file.path(save_dir, "ov.afc1_cbt_adaboost.rds"))

.Random.seed <- seed
fit.cbt.rf <- train_final(cbt.dat, alg = "rf", file.name = file.path(save_dir, "ov.afc1_cbt_rf.rds"))

.Random.seed <- seed
fit.cbt.mlr_ridge <- train_final(cbt.dat, alg = "mlr_ridge", file.name = file.path(save_dir, "ov.afc1_cbt_mlr_ridge.rds"))

.Random.seed <- seed
fit.cbt.mlr_lasso <- train_final(cbt.dat, alg = "mlr_lasso", file.name = file.path(save_dir, "ov.afc1_cbt_mlr_lasso.rds"))

# build list of all fits
be <- purrr::set_names(c("ov.afc1_xpn", "ov.afc1_cbt"))
algs <- purrr::set_names(c("adaboost", "rf", "mlr_ridge", "mlr_lasso"))
fit.c1 <- purrr::map(be, function(b) {
  purrr::map(algs, function(a) {
    readr::read_rds(file.path(save_dir, paste0(b, "_", a, ".rds")))
  })
})
readr::write_rds(fit.c1, file.path(save_dir, "all_fits.rds"))
