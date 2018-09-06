# Load and map samples and genes between array and NanoString
source(here::here("R/6-cross_platform/0-map.R"))

# Read in the model fits
fits <- readRDS(file.path(outputDir, "gene_selection", "TrainingC1",
                          "rf_alternate_c1.rds"))
output_dir <- file.path(outputDir, "cross_platform", "predictions")

overlap.pred.nstring.2 <-
  splendid::prediction(fits[[2]], overlap_nstring_dat)
overlap.pred.array.2 <-
  splendid::prediction(fits[[2]], overlap_array_dat)

overlap.pred.nstring.3 <-
  splendid::prediction(fits[[3]], overlap_nstring_dat)
overlap.pred.array.3 <-
  splendid::prediction(fits[[3]], overlap_array_dat)

conf2 <- caret::confusionMatrix(overlap.pred.nstring.2, overlap.pred.nstring.2)
conf3 <- caret::confusionMatrix(overlap.pred.nstring.3, overlap.pred.nstring.3)

saveRDS(conf2, file.path(output_dir, "overlap_pred_nstring_2.rds"))
saveRDS(conf3, file.path(output_dir, "overlap_pred_nstring_3.rds"))
