###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

source("utils/utils.R")

# import training data
xpn.dat <- import_study("data/", "ov.afc1_xpn")
cbt.dat <- import_study("data/", "ov.afc1_cbt")

# fit algo
seed <- readRDS("data/seed.rds")

.Random.seed <- seed
fit.xpn.adaboost <- train_final(xpn.dat, alg = "adaboost")
readr::write_rds(
  fit.xpn.adaboost,
  paste0("outputs/fits/ov.afc1_xpn_adaboost.rds")
)

.Random.seed <- seed
fit.xpn.rf <- train_final(xpn.dat, alg = "rf")
readr::write_rds(
  fit.xpn.rf,
  paste0("outputs/fits/ov.afc1_xpn_rf.rds")
)

.Random.seed <- seed
fit.xpn.mlr_ridge <- train_final(xpn.dat, alg = "mlr_ridge")
readr::write_rds(
  fit.xpn.mlr_ridge,
  paste0("outputs/fits/ov.afc1_xpn_mlr_ridge.rds")
)

.Random.seed <- seed
fit.xpn.mlr_lasso <- train_final(xpn.dat, alg = "mlr_lasso")
readr::write_rds(
  fit.xpn.mlr_lasso,
  paste0("outputs/fits/ov.afc1_xpn_mlr_lasso.rds")
)

.Random.seed <- seed
fit.cbt.adaboost <- train_final(cbt.dat, alg = "adaboost")
readr::write_rds(
  fit.cbt.adaboost,
  paste0("outputs/fits/ov.afc1_cbt_adaboost.rds")
)

.Random.seed <- seed
fit.cbt.rf <- train_final(cbt.dat, alg = "rf")
readr::write_rds(
  fit.cbt.rf,
  paste0("outputs/fits/ov.afc1_cbt_rf.rds")
)

.Random.seed <- seed
fit.cbt.mlr_ridge <- train_final(cbt.dat, alg = "mlr_ridge")
readr::write_rds(
  fit.cbt.mlr_ridge,
  paste0("outputs/fits/ov.afc1_cbt_mlr_ridge.rds")
)

.Random.seed <- seed
fit.cbt.mlr_lasso <- train_final(cbt.dat, alg = "mlr_lasso")
readr::write_rds(
  fit.cbt.mlr_lasso,
  paste0("outputs/fits/ov.afc1_cbt_mlr_lasso.rds")
)

# build list of all fits
be <- c("ov.afc1_xpn", "ov.afc1_cbt")
algs <- c("adaboost", "rf", "mlr_ridge", "mlr_lasso")
fit.c1 <- list()
alg.list <- list()
for (i in seq_along(be)) {
  for (j in seq_along(algs)) {
    fit.tmp <- readr::read_rds(
      paste0("outputs/fits/", be[i], "_", algs[j], ".rds")
    )
    alg.list[[j]] <- fit.tmp
  }
  names(alg.list) <- algs
  fit.c1[[i]] <- alg.list
}
names(fit.c1) <- be
readr::write_rds(fit.c1, "outputs/fits/all_fits.rds")
