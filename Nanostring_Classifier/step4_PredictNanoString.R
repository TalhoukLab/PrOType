###############################################################
################ STEP 2: Predict All Nanostring ###############
###############################################################

suppressPackageStartupMessages({
  source("utils/PredictNanoString.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

set.seed(2017)



# import nanostring
train.dat <- prep_data("ov.afc1_xpn", dir = "data/")
nstring.batches <- load_nanostring("data/", train.dat$npcp[,-1])

# import cut 1 fits
fit.c1 <- readr::read_rds("outputs/fits/all_fits.rds")

# predict nanostring
pred.nano <- purrr::map2(fit.c1, names(fit.c1), function(x, y) {
  purrr::map2(x, names(x), function(z, k) {
    # uncomment below to write to file
    #bc <- stringr::str_sub(y, nchar(y) - 2, nchar(y))
    #fname <- paste0("outputs/predictions/nstring_allbatches_", bc, "_", k, ".rds")
    #preds <- predict_nstring(z, nstring.batches)
    #readr::write_rds(preds, path = fname)
    #return(preds)
    predict_nstring(z, nstring.batches)
  })
})

# write to file
readr::write_rds(pred.nano, "outputs/predictions/nstring_allbatches.rds")
