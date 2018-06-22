###############################################################
################# STEP 3: Validate Nanostring #################
###############################################################

library(here)
source(here("nanostring_classifier/utils/utils.R"))

set.seed(2017)

# import cut 1 fits
fit.c1 <- readr::read_rds(paste0(output_dir, "/fits/ov.afc1_cbt_adaboost.rds"))

# import overlapping data
map <- get_mapping(dir = "array_classifier/2_post_processing/data/")
overlap.nstring <- get_nstring_overlap(dir = "array_classifier/2_post_processing/data/", map = map)

# predict overlap nstring
pred.overlap.nstring <- predict_overlap(fit.c1, overlap.nstring)

# import adaboost predictions from validation step
pred.overlap.array <- transpose(read_rds(paste0(output_dir, "/predictions/pred_overlap_array.rds")))$adaboost[[1]]

# combine overlapping array and nstring
overlap <- combine(pred.overlap.array, overlap.nstring, pred.overlap.nstring)

# evaluate overlap results
eval.overlap <- purrr::transpose(evaluate_all(overlap))
cat(paste0(output_dir, "/evals/", dataset, "_adaboost.rds"), "\n")
readr::write_rds(eval.overlap, paste0(output_dir, "/evals/", dataset, "_adaboost.rds"))