# Gene Selection setup ----------------------------------------------------

# Load utility functions
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

# Load NanoString data and prediction labels
nsdat <- load_nanostring()
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Define consensus training set (cut1)
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab
