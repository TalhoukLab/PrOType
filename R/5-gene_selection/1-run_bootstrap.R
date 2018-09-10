GS_training_dir <- "R/5-gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R"
)
purrr::walk(here::here(GS_training_dir, GS_training_files), source)

# Load data
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab

cli::cat_line("Getting Data")
x <- sl_data(train_dat, study, "training")
y <- sl_class(train_lab, x)
genes <- get_genes(train_dat)
ntop <- 100
seed_boot <- 2018
seed_alg <- 2018

cli::cat_line("Running Bootstrap")
train_model(
  file.path(outputDir, "gene_selection", "training"),
  study,
  x,
  y,
  genes,
  B,
  ntop,
  alg,
  seed_boot,
  seed_alg,
  shouldCompute
)
cli::cat_line("Finished Bootstrap")
