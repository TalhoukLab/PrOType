###############################################################
################# STEP 3: Validate Nanostring #################
###############################################################

suppressPackageStartupMessages({
  source("utils/utils.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

set.seed(2017)


# import cut 1 fits
fit.c1 <- readr::read_rds("outputs/fits/ov.afc1_cbt_adaboost.rds")

# import overlapping data
map <- get_mapping() 
overlap.nstring <- get_nstring_overlap(map = map)

# predict overlap nstring
pred.overlap.nstring <- predict_overlap(fit.c1, overlap.nstring)

# import adaboost predictions from validation step
pred.overlap.array <- readr::read_rds(
  "outputs/predictions/pred_overlap_array.rds"
  )$ov.afc1_xpn$adaboost

# combine overlapping array and nstring
overlap <- combine(pred.overlap.array, overlap.nstring, pred.overlap.nstring)

# evaluate overlap results
eval.overlap <- evaluate_all(overlap) %>%
  purrr::transpose(.)
readr::write_rds(
  eval.overlap,
  paste0("outputs/evals/ov.afc1_xpn_adaboost.rds")
)

# visualize evaluation results
#eval.plot <- plot_evals_noCBT(
#  paste0("outputs/evals/xpn_adaboost.rds"),
#  plot.title = "XPN Adaboost",
#  save = TRUE, print = FALSE,
#  alg = "adaboost"
#)
