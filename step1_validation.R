###############################################################
###### STEP 1: Fit Models on Cut 1 and Validate on Cut 2 ######
###############################################################

suppressPackageStartupMessages({
  source("Rscripts/validation_plots.R")
  source("Rscripts/validation.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

set.seed(2017)



# import cut 1 data
cut1.dat <- c("ov.afc1_xpn", "ov.afc1_cbt")
cut1 <- purrr::map(cut1.dat, function(x) {
  if(grepl("c1", x))
  {
    import_study("./data/", study = x, hc.normalize = TRUE, is.test.set = FALSE) 
  } else {
    import_study("./data/", study = x, hc.normalize = TRUE, is.test.set = TRUE)
  }
}) %>% set_names(cut1.dat)

# import cut 2 data
cut2.dat <- c("ov.afc2_xpn", "ov.afc2_cbt")
cut2 <- purrr::map(cut2.dat, function(x) {
  if(grepl("c1", x))
  {
    import_study("./data/", study = x, hc.normalize = TRUE, is.test.set = FALSE) 
  } else {
    import_study("./data/", study = x, hc.normalize = TRUE, is.test.set = TRUE)
  }
}) %>% set_names(cut2.dat)

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

# predict fits on cut 2
pred.c2 <- purrr::pmap(list(fit.c1, cut2, cut2.dat), function(x, y, a) {
  purrr::map2(x, algos, function(z, b) {
    # uncomment below to write to file
    #pred <- predict_study(z, new.dat = y);
    #readr::write_rds(pred, path = paste0("outputs/predictions/", a, "_", b, ".rds"));
    #return(pred)
    predict_study(z, new.dat = y);
  })
})

# evaluate cut 2
names.ls <- purrr::map(cut1.dat, function(x) {
  study.extract <- stringr::str_sub(x, nchar(x) - 2, nchar(x))
  purrr::map(algos, function(y) {
    paste(study.extract, y, sep = ".")
  })
}) %>% purrr::flatten() %>% as.character()
eval.c2 <- purrr::pmap(list(
  pred.c2, cut2, cut2.dat, cut1.dat
), function(x, y, a, b) {
  purrr::map2(x, algos, function(z, d) {
    eval(z, as.factor(y$y), train.name = b, test.name = a) %>%
      data.table::setattr(., "study", b) %>%
      data.table::setattr(., "algo", d)
  })
}) %>% purrr::flatten() %>% set_names(names.ls)
readr::write_rds(eval.c2, "outputs/evals/cut2_eval.rds")

# visualize evaluation results
plot.c2.noThreshold <- plot_evals("outputs/evals/cut2_eval.rds", 
                                  plot.title = "Cut 2 Validation - No Threshold", 
                                  save = TRUE, print = FALSE, threshold = FALSE, 
                                  algs = c("mlr_ridge", "mlr_lasso"))

plot.c2.threshold <- plot_evals("outputs/evals/cut2_eval.rds", 
                                plot.title = "Cut 2 Validation - Threshold", 
                                save = TRUE, print = FALSE, threshold = TRUE, 
                                algs = c("adaboost", "rf"))
