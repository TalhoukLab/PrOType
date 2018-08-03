# Load and map samples and genes between array and NanoString
library(here)
source(here::here("R/CrossPlatform/map.R"))

input_dir <- file.path(outputDir, "GeneSelection/output/TrainingC1")

# Read in the model fits
fits <- readr::read_rds(file.path(input_dir, "rf_alternate_c1.rds"))

overlap.pred.nstring.2 <-
  splendid::prediction(fits[[2]], overlap_nstring_dat, class = NULL)
overlap.pred.array.2 <-
  splendid::prediction(fits[[2]], overlap_array_dat, class = NULL)

overlap.pred.nstring.3 <-
  splendid::prediction(fits[[3]], overlap_nstring_dat, class = NULL)
overlap.pred.array.3 <-
  splendid::prediction(fits[[3]], overlap_array_dat, class = NULL)

conf2 <- caret::confusionMatrix(overlap.pred.nstring.2, overlap.pred.nstring.2)
conf3 <- caret::confusionMatrix(overlap.pred.nstring.3, overlap.pred.nstring.3)

write_rds(conf2, file.path(outputDir, "CrossPlatform/output/overlap_pred_nstring_2.rds"))
write_rds(conf3, file.path(outputDir, "CrossPlatform/output/overlap_pred_nstring_3.rds"))
