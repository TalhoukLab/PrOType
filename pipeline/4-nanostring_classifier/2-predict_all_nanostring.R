# Predict all nanostring data ---------------------------------------------

# Load utility functions
source(here::here("pipeline/4-nanostring_classifier/utils.R"))

# Load vancouver model
alg <- "adaboost"
van_mod <-
  readRDS(file.path(outputDir, "post_processing", "fits",
                    paste0(trainSet, "_", alg, ".rds")))
genes <- van_mod[["names"]]

# Load all nanostring data
cli::cat_line("Loading all nanostring data")
nsdat <- load_nstring_all(dir = "data/nstring", genes = genes)

# Predict all nanostring data
cli::cat_line("Predicting all nanostring data")
pred_nano <- splendid::prediction(van_mod, nsdat) %>%
  tibble::tibble(ottaID = rownames(nsdat), preds = .) %>%
  `attr<-`("batch", attr(nsdat, "batch"))
saveRDS(pred_nano,
        file.path(outputDir, "nanostring", "predictions",
                  "nstring_all_batches.rds"))
