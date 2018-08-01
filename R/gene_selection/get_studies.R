GS_training_dir <- "R/gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R",
  "evaluate.R",
  "summary.R",
  "analysis.R"
)
walk(here::here(GS_training_dir, GS_training_files), source)

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab

# Get studies
studies <- unique(train_dat$site)
print(studies)
