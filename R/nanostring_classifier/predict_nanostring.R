###############################################################
################ STEP 4: Predict All Nanostring ###############
###############################################################

library(here)
source(here("nanostring_classifier/utils/utils.R"))

set.seed(2017)

# load vancouver model
Van_mod <- readr::read_rds(paste0(output_dir, "/fits/", dataset, "_adaboost.rds"))
genes <- Van_mod$names

# load nanostring data
nsdat <- load_nanostring(dir = "array_classifier/2_post_processing/data/", genes)

# predict nanostring data
cli::cat_line("predicting nanostring data\n")
pred.nano <- predict(Van_mod, nsdat, type = "class") %>%
  data_frame(ottaID = rownames(nsdat), preds = .)

# write to file
readr::write_rds(pred.nano, paste0(output_dir, "/predictions/nstring_allbatches.rds"))