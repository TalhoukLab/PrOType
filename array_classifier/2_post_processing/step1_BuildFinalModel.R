###############################################################
################ STEP 1: Fit Models on Cut 1 ##################
###############################################################

library(here)
source(here("array_classifier/2_post_processing/utils/utils.R"))

# batch effects and algorithms
be <- purrr::set_names(c("ov.afc1_cbt", "ov.afc1_xpn"))
algs <- purrr::set_names(c("adaboost", "rf", "mlr_ridge", "mlr_lasso"))

# import training data
all.dat <- purrr::map(be, import_study, dir = "data")
save_dir <- "outputs/fits"

# fit algo
purrr::walk2(all.dat, be, ~ {
  purrr::walk(algs, function(a) {
    cat("Walking: ", a, "\n")
    print(dim(.x))
    .x <- t(.x)
    colnames(.x) <- make.names(colnames(.x))
    fit <- splendid::classification(
      data = .x[, -1],
      # data = make.names(.x[, -1]),
      class = .x[, 1],
      algorithms = a,
      standardize = FALSE
    )
    cat("Saving to:", file.path(save_dir, paste0(.y, "_", a, ".rds")), "\n")
    readr::write_rds(fit, file.path(save_dir, paste0(.y, "_", a, ".rds")))
    fit
  })
})

# build list of all fits
cat("Reading files")
fit.c1 <- purrr::map(be, function(b) {
  purrr::map(algs, function(a) {
    readr::read_rds(file.path(save_dir, paste0(b, "_", a, ".rds")))
  })
})
readr::write_rds(fit.c1, file.path(save_dir, "all_fits.rds"))
