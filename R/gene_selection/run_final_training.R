GS_training_dir <- "R/gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R"
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

cli::cat_line("Getting Data")
x <- sl_data(train_dat, study, "training")
y <- sl_class(train_lab, x)

seed_boot <- 2018
seed_alg <- 2018

cli::cat_line("Processing Bootstrap")

sumFreq <- readr::read_csv(file.path(output_dir, "output/sumFreq",
                                     paste0(study, "_sumFreq.csv")),
                           col_types = readr::cols())

classify_top_genes(x,
                   y,
                   sumFreq,
                   file.path(output_dir, "output", "finalTraining"),
                   study,
                   seed_alg,
                   alg)
cli::cat_line("Finished processing bootstrap")
