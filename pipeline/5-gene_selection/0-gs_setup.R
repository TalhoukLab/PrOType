# Gene Selection setup ----------------------------------------------------

# Load utility functions
source(here::here("pipeline/5-gene_selection/utils.R"))

# Load NanoString data and prediction labels
nsdat <- load_nanostring()
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Define consensus training set (cut1)
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab
