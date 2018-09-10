# Load Parameters
n_genes <- 55
alg <- "rf"
grm <- "CTHRC1"

# Load utility functions
`%>%` <- magrittr::`%>%`
input_dir <- "assets/data/nstring"
GS_output_dir <- file.path(outputDir, "gene_selection", "build_final_model")
GS_training_dir <- "R/5-gene_selection/scripts"
GS_training_files <- c(
  "define.R",
  "train.R",
  "bootstrap.R",
  "evaluate.R",
  "summary.R",
  "analysis.R",
  "final.R",
  "utils.R"
)
purrr::walk(here::here(GS_training_dir, GS_training_files), source)

# Load data----
# Load the NanoString data and prediction labels
nsdat <- load_nanostring()
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Define training set (cut1)
train <- define_batch(preds_new, nsdat, batch = "b1")
train_dat <- train$dat
train_lab <- train$lab

# Define overlap
overlap <- define_overlap(preds_new, nsdat)
overlap_dat <- overlap$dat
overlap_lab <- overlap$lab

# Define test 1 (cut2 excluding overlap)
test1 <- define_batch(preds_new, nsdat, batch = "b2")
test1_lab <- test1$lab
test1_dat <- test1$dat

# Define test 2 (cut3)
test2 <- define_batch(preds_new, nsdat, batch = "b3")
test2_lab <- test2$lab
test2_dat <- test2$dat

# Define test 3 (cut4)
test3 <- define_batch(preds_new, nsdat, batch = "b4")
test3_lab <- test3$lab
test3_dat <- test3$dat

# Build the final model----
cli::cat_line("Build the final model with top ", n_genes, " genes")
x <- sl_data(train_dat)
y <- sl_class(train_lab, x)
sum_freq <- read.csv(file.path(outputDir, "gene_selection", "sum_freq",
                              "overall_freqs.csv"),
                    stringsAsFactors = FALSE)
final_glist <- sum_freq %>%
  dplyr::arrange(dplyr::desc(rfFreq), dplyr::desc(lassoFreq)) %>%
  dplyr::pull(genes) %>%
  make.names() %>%
  purrr::discard(~ . %in% grm) %>%
  head(n_genes)
final_model <- splendid::classification(x[, final_glist], y, algorithms = alg, seed_alg = 2018)

# Test on Overlapping Samples----
cli::cat_line("Testing the final model on the ", nrow(overlap_dat), " overlapping samples")
x.new <- sl_data(overlap_dat)
y.new <- sl_class(overlap_lab, x.new)

overlap_lab$prediction <- splendid::prediction(final_model, x.new[, final_glist], y.new)
overlap_eval <- caret::confusionMatrix(overlap_lab$prediction, overlap_lab$Adaboost.xpn)

# Test on Cut 2----
cli::cat_line("Testing the final model on the ", nrow(test1_dat), " cut 2 samples")
x.new <- sl_data(test1_dat)
y.new <- sl_class(test1_lab, x.new)

test1_lab$prediction <- splendid::prediction(final_model, x.new[, final_glist], y.new)
test1_eval <- caret::confusionMatrix(test1_lab$prediction, test1_lab$Adaboost.xpn)

# Test on Cut 3----
cli::cat_line("Testing the final model on the ", nrow(test2_dat), " cut 3 samples")
x.new <- sl_data(test2_dat)
y.new <- sl_class(test2_lab, x.new)

test2_lab$prediction <- splendid::prediction(final_model, x.new[, final_glist], y.new)
test2_eval <- caret::confusionMatrix(test2_lab$prediction, test2_lab$Adaboost.xpn)

# Test on Cut 4----
cli::cat_line("Testing the final model on the ", nrow(test3_dat), " cut 4 samples")
x.new <- sl_data(test3_dat)
y.new <- sl_class(test3_lab, x.new)

test3_lab$prediction <- splendid::prediction(final_model, x.new[, final_glist], y.new)
test3_eval <- caret::confusionMatrix(test3_lab$prediction, test3_lab$Adaboost.xpn)

# Predict all NanoString with Final Model----
x.new <- sl_data(nsdat)

preds_new_cons <- preds_new %>%
  dplyr::select(ottaID, cut = Batch, all_array = Adaboost.xpn, TCGA = TCGA.Predicted.Subtype, published) %>%
  dplyr::mutate(consensus = ifelse(all_array == TCGA, all_array, ""))

Final_Predictions <- data.frame(
  preds_new_cons,
  prediction = predict(final_model, x.new[, final_glist]),
  predict(final_model, x.new[, final_glist], type = "prob")
) %>%
  dplyr::mutate(final = ifelse(
    consensus == "",
    as.character(prediction),
    as.character(consensus)
  ))

write.csv(Final_Predictions, file.path(GS_output_dir, "Final_Predictions.csv"))
write.csv(final_glist, file.path(GS_output_dir, "final_glist.csv"))
saveRDS(final_model, file.path(GS_output_dir, "final_model.rds"))

# Predict ARL NanoString samples----
x.arl.raw <- readxl::read_excel(file.path(input_dir, "Nanostring_ARL-all samples-all gnes_20180607.xlsx"))
x.arl <- prepare_samples(x.arl.raw)
arl_predictions <- predict_samples(final_model, x.arl)
readr::write_csv(arl_predictions, file.path(GS_output_dir, "arl_predictions.csv"))

# Compare NanoString and ARL predictions
pred_compare <- dplyr::inner_join(Final_Predictions, arl_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 140/140 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical

# Predict additional NanoString samples----

# BRO_HET_test
nanostring_data_BRO_HET_test_20160915 <- readxl::read_excel(file.path(input_dir, "nanostring data_BRO HET.test_20160915.xlsx"))
x.bro_het_test <- prepare_samples(nanostring_data_BRO_HET_test_20160915)
bro_het_test_predictions <- predict_samples(final_model, x.bro_het_test)
readr::write_csv(bro_het_test_predictions, file.path(GS_output_dir, "bro_het_test_predictions.csv"))

# Compare NanoString and BRO_HET_test predictions
pred_compare <- dplyr::inner_join(Final_Predictions, bro_het_test_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 28/28 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical

# cell_lines
nanostring_data_cell_lines_20160915 <- readxl::read_excel(file.path(input_dir, "nanostring data_cell lines_20160915.xlsx"))
x.cell_lines <- prepare_samples(nanostring_data_cell_lines_20160915)
cell_lines_predictions <- predict_samples(final_model, x.cell_lines)
readr::write_csv(cell_lines_predictions, file.path(GS_output_dir, "cell_lines_predictions.csv"))

# Compare NanoString and cell_lines predictions
pred_compare <- dplyr::inner_join(Final_Predictions, cell_lines_predictions, by = "ottaID")
nrow(pred_compare) # zero overlapping samples

# LAX_VAN_OM_test
nanostring_data_LAX_VAN_OM_test_20160915 <- readxl::read_excel(file.path(input_dir, "nanostring data_LAX VAN OM.test_20160915.xlsx"))
x.lax_van_om_test <- prepare_samples(nanostring_data_LAX_VAN_OM_test_20160915)
lax_van_om_test_predictions <- predict_samples(final_model, x.lax_van_om_test)
readr::write_csv(lax_van_om_test_predictions, file.path(GS_output_dir, "lax_van_om_test_predictions.csv"))

# Compare NanoString and LAX_VAN_OM_test predictions
pred_compare <- dplyr::inner_join(Final_Predictions, lax_van_om_test_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 48/48 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical

# ARL_paired_samples
nanostring_data_ARL_paired_samples_20160915 <- readxl::read_excel(file.path(input_dir, "nanostring data_ARL paired samples_20160915.xlsx"))
x.arl_paried_samples <- prepare_samples(nanostring_data_ARL_paired_samples_20160915)
arl_paired_samples_predictions <- predict_samples(final_model, x.arl_paried_samples)
readr::write_csv(arl_paired_samples_predictions, file.path(GS_output_dir, "arl_paired_samples_predictions.csv"))

# Compare NanoString and ARL_paired_samples predictions
pred_compare <- dplyr::inner_join(Final_Predictions, arl_paired_samples_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 26/26 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical

# Rep_BIO_samples
nanostring_data_Rep_BIO_samples_20160916 <- readxl::read_excel(file.path(input_dir, "nanostring data_Rep.BIO samples_20160916.xlsx"))
x.rep_bio_samples <- prepare_samples(nanostring_data_Rep_BIO_samples_20160916)
rep_bio_samples_predictions <- predict_samples(final_model, x.rep_bio_samples)
readr::write_csv(rep_bio_samples_predictions, file.path(GS_output_dir, "rep_bio_samples_predictions.csv"))

# Compare NanoString and Rep_BIO_samples predictions
pred_compare <- dplyr::inner_join(Final_Predictions, rep_bio_samples_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 38/38 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical

# Replicates_and_Xsite_samples
nanostring_data_replicates_and_Xsite_20160915 <- readxl::read_excel(file.path(input_dir, "nanostring data_replicates and Xsite_20160915.xlsx"))
x.replicates_and_Xsite <- prepare_samples(nanostring_data_replicates_and_Xsite_20160915)
replicates_and_Xsite_predictions <- predict_samples(final_model, x.replicates_and_Xsite)
readr::write_csv(replicates_and_Xsite_predictions, file.path(GS_output_dir, "replicates_and_Xsite_predictions.csv"))

# Compare NanoString and Replicates_and_Xsite_samples
pred_compare <- dplyr::inner_join(Final_Predictions, replicates_and_Xsite_predictions, by = "ottaID")
dplyr::summarize(pred_compare, agree = sum(predicted == prediction)) # 120/120 agree
identical(pred_compare$predicted, pred_compare$prediction) # verify identical
