# Predict NanoString data using all-array model ---------------------------

# Load utility functions
source(here::here("pipeline/4-nanostring_classifier/utils.R"))

# Load all-array (Vancouver) model
alg <- "adaboost"
aa_model <-
  readRDS(file.path(outputDir, "post_processing", "fits",
                    paste0(trainSet, "_", alg, ".rds")))
genes <- aa_model[["names"]]

# Load full nanostring data
cli::cat_line("Loading full nanostring data")
nsdat <- load_nstring_all(dir = "data/nstring", genes = genes)

# Predict full nanostring data
cli::cat_line("Predicting full nanostring data using all-array model")
pred <- splendid::prediction(aa_model, nsdat)

# Predictions
aa_pred <- tibble::tibble(ottaID = rownames(nsdat), preds = pred) %>%
  `attr<-`("batch", attr(nsdat, "batch"))
readr::write_csv(aa_pred, file.path(outputDir, "nanostring", "predictions",
                                    "aa_pred.csv"))

# Probabilities
aa_probs <- as.data.frame(attr(pred, "prob")) %>%
  tibble::rownames_to_column("ottaID") %>%
  tibble::as_tibble()
readr::write_csv(aa_probs, file.path(outputDir, "nanostring", "predictions",
                                     "aa_probs.csv"))
