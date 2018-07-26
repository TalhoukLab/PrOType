# Clear the deck----
rm(list = ls())
# Load packages----
library(here)
suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(splendid)
  library(caret)
  library(cli)
  library(readxl)
})
source(here("GeneSelection/scripts/utils.R"))
GS_training_dir <- "GeneSelection/training"
GS_training_files <- c(
  "consensus.R",
  "train.R",
  "bootstrap.R",
  "evaluate.R",
  "summary.R",
  "analysis.R"
)
walk(here(GS_training_dir, GS_training_files), source)
# Parameters----
n_genes <- 55
alg <- "rf"
grm <- "CTHRC1"

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
# Load the average frequency across all studies
# sumFreq <- read.csv(file.path(output_dir, "output/sumFreq/overallFreqs.csv"),
#                    stringsAsFactors = FALSE) %>%
#  arrange(desc(`rfFreq`))
# sumFreq <- read.csv("/Users/atalhouk/Repositories/NanoString/HGSCS/Outputs/GeneSelection/overallFreqs2.csv",
#                     stringsAsFactors = FALSE) %>%
#   arrange(desc(`rfFreq`))
sumFreq <- read.csv("/Users/dchiu/Documents/Misc/Derek/sumFreq/overallFreqs.csv",
                    stringsAsFactors = FALSE) %>%
  arrange(desc(rfFreq), desc(lassoFreq))

cli::cat_line("Build the final model with top ", n_genes, " genes")
x <- sl_data(train_dat)
y <- sl_class(train_lab, x)
genes <- get_genes(train_dat)
rf_genes <- make.names(sumFreq$genes)
final_glist <- head(rf_genes[!rf_genes %in% grm], n_genes) # Final gene list
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
  select(ottaID, cut = Batch, all_array = Adaboost.xpn, TCGA = TCGA.Predicted.Subtype, published) %>%
  mutate(consensus = ifelse(all_array == TCGA, all_array, ""))

Final_Predictions <- data.frame(
  preds_new_cons,
  prediction = predict(final_model, x.new[, final_glist]),
  predict(final_model, x.new[, final_glist], type = "prob")
) %>%
  mutate(final = ifelse(
    consensus == "",
    as.character(prediction),
    as.character(consensus)
  ))

write.csv(Final_Predictions, "Final_Predictions.csv")
write.csv(final_glist, "final_glist.csv")
write_rds(final_model, "final_model.rds")

# Predict ARL samples
x.arl.raw <- read_excel("raw_data/Nanostring_ARL-all samples-all gnes_20180607.xlsx")

x.arl <- x.arl.raw %>%
  `colnames<-`(make.names(colnames(.))) %>%
  as.data.frame() %>%
  column_to_rownames("OTTA.ID") %>%
  select_if(is.numeric)

arl_predictions <- predict(final_model, x.arl) %>%
  enframe(name = "ottaID", value = "ARL") %>%
  cbind(predict(final_model, x.arl, type = "prob")) %>%
  as_tibble()
write_csv(arl_predictions, "arl_predictions.csv")

# Compare NanoString and ARL predictions
pred_compare <- inner_join(Final_Predictions, arl_predictions, by = "ottaID")

summarize(pred_compare, agree = sum(ARL == prediction)) # 140/140 agree
identical(pred_compare$ARL, pred_compare$prediction) # verify identical
