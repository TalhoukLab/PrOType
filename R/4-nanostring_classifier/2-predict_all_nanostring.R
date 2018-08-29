# Predict all nanostring data ---------------------------------------------

# Load utility functions
source(here::here("R/nanostring_classifier/utils/utils.R"))

# Load vancouver model
alg <- "adaboost"
van_mod <-
  readRDS(file.path(outputDir, "fits", paste0(trainSet, "_", alg, ".rds")))
genes <- van_mod[["names"]]

# Load all nanostring data
cli::cat_line("Loading all nanostring data")
nsdat <- load_nstring_all(dir = "assets/data/nstring", genes = genes)

# Predict all nanostring data
cli::cat_line("Predicting all nanostring data")
pred_nano <- splendid::prediction(van_mod, nsdat) %>%
  tibble::tibble(ottaID = rownames(nsdat), preds = .) %>%
  `attr<-`("batch", attr(nsdat, "batch"))
saveRDS(pred_nano,
        file.path(outputDir, "predictions", "nstring_all_batches.rds"))
