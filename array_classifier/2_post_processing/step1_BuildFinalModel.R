###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

library(here)
source(here("array_classifier/2_post_processing/utils/utils.R"))

# import training data
xpn.dat <- import_study("data", "ov.afc1_xpn")
cbt.dat <- import_study("data", "ov.afc1_cbt")
save_dir <- "outputs/fits"

# batch effects and algorithms
be <- purrr::set_names(c("ov.afc1_xpn", "ov.afc1_cbt"))
algs <- purrr::set_names(c("adaboost", "rf", "mlr_ridge", "mlr_lasso"))

# fit algo
purrr::walk2(list(xpn.dat, cbt.dat), be, ~ {
  purrr::walk(algs, function(a) {
    train_final(.x, a, file.path(save_dir, paste0(.y, "_", a, ".rds")))
  })
})

# build list of all fits
fit.c1 <- purrr::map(be, function(b) {
  purrr::map(algs, function(a) {
    readr::read_rds(file.path(save_dir, paste0(b, "_", a, ".rds")))
  })
})
readr::write_rds(fit.c1, file.path(save_dir, "all_fits.rds"))
