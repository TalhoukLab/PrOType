GS_training_dir <- "R/5-gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R"
)
purrr::walk(here::here(GS_training_dir, GS_training_files), source)

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat

cli::cat_line("Summarizing ", study, " bootstrap frequencies")
fnames <- list.files(
  path = file.path(outputDir, "gene_selection", "boot_freq"),
  pattern = paste0(study, "_freq_(", paste(algs, collapse = "|"), ")"),
  full.names = TRUE
)
sum_freq <- fnames %>%
  purrr::map(readr::read_csv, col_types = readr::cols()) %>%
  purrr::reduce(dplyr::inner_join, by = "genes") %>%
  dplyr::arrange(dplyr::desc(rfFreq))
readr::write_csv(sum_freq, file.path(output_dir, "gene_selection", "sum_freq",
                                     paste0(study, "_sum_freq.csv")))
cli::cat_line("Completed ", study, " summary frequencies")
