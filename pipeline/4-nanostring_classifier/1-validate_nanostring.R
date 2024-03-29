# Validate overlapping nanostring data ------------------------------------

# Load utility functions
source(here::here("pipeline/4-nanostring_classifier/utils.R"))

# Import cut 1 fits on top models
all_fits <- list.files(file.path(outputDir, "post_processing", "fits"),
                       full.names = TRUE) %>%
  purrr::set_names(stringr::str_remove(basename(.), paste0(trainSet, "_"))) %>%
  purrr::set_names(tools::file_path_sans_ext(names(.))) %>%
  purrr::map(readRDS)

# Import overlap nanostring samples (remove 1 case without ottaID match)
cli::cat_line("Importing overlap nanostring samples")
overlaps <- load_overlap(dir = "data/nstring", keep_pub = TRUE) %>%
  dplyr::filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
data_overlap_nstring <-
  import_nstring_overlap(dir = "data/nstring",
                         osamples = overlaps[["ottaID"]])

# Predict overlap nstring samples and combine with published
cli::cat_line("Predicting overlap nanostring samples")
pred_dir <- file.path(outputDir, "nanostring", "predictions")
pred_overlap_nstring <- all_fits %>%
  purrr::map(splendid::prediction, data = data_overlap_nstring) %>%
  purrr::map(join_published_nstring, overlap = overlaps) %>%
  purrr::iwalk(~ saveRDS(.x, file.path(
    pred_dir, paste0("pred_overlap_nstring_", .y, ".rds")
  )))
saveRDS(pred_overlap_nstring, file.path(pred_dir, "pred_overlap_nstring.rds"))

# Import overlap array predictions
pred_overlap_array <- readRDS(file.path(outputDir, "post_processing",
                                        "predictions",
                                        "pred_overlap_array.rds"))

# Combine overlapping array and nstring predictions
pred_all <- purrr::map2(pred_overlap_array, pred_overlap_nstring, combine_pred)

# Evaluate all overlap results
cli::cat_line("Evaluating overlap array and nanostring predictions")
eval_dir <- file.path(outputDir, "nanostring", "evals")
eval_overlap_all <- purrr::map(pred_all, evaluate_all) %>%
  purrr::iwalk(~ saveRDS(.x, file.path(
    eval_dir, paste0("eval_overlap_all_", .y, ".rds")
  )))
saveRDS(eval_overlap_all, file.path(eval_dir, "eval_overlap_all.rds"))

# Evaluate consensus results
cli::cat_line("Evaluating consensus and published predictions")
eval_consensus <- purrr::map(pred_all, evaluate_consensus) %>%
  purrr::iwalk(~ saveRDS(.x, file.path(
    eval_dir, paste0("eval_consensus_", .y, ".rds")
  )))
saveRDS(eval_consensus, file.path(eval_dir, "eval_consensus.rds"))
