###############################################################
################ STEP 4: Predict All Nanostring ###############
###############################################################

library(here)
source(here("nanostring_classifier/utils/utils.R"))

set.seed(2017)

# load vancouver model
Van_mod <- readr::read_rds("outputs/fits/ov.afc1_xpn_adaboost.rds")
genes <- Van_mod$names

# load nanostring data
nsdat <- load_nanostring("data", genes)

# predict nanostring data
pred.nano <- predict(Van_mod, nsdat, type = "class") %>%
  data_frame(ottaID = rownames(nsdat), preds = .)

# write to file
readr::write_rds(pred.nano, "outputs/predictions/nstring_allbatches.rds")
