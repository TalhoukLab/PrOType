GS_training_dir <- "R/gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R"
)
walk(here(GS_training_dir, GS_training_files), source)

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat

cli::cat_line("Processing Bootstrap")
runProcessBoot(file.path(output_dir, "output", "training"), study, train_dat, B, algs)
cli::cat_line("Finished processing bootstrap")

