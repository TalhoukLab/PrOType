# Validate overlapping array data -----------------------------------------

# Load utility functions
source(here::here("R/3-post_processing/utils/utils.R"))

# Import cut 1 fits on top models
all_fits <- list.files(file.path(outputDir, "post_processing", "fits"),
                       full.names = TRUE) %>%
  purrr::set_names(stringr::str_remove(basename(.), paste0(trainSet, "_"))) %>%
  purrr::set_names(tools::file_path_sans_ext(names(.))) %>%
  purrr::map(readRDS)

# Import overlap array samples (remove 1 case without ottaID match)
cli::cat_line("Importing overlap array samples")
overlaps <- load_overlap(dir = "assets/data/nstring") %>%
  dplyr::filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
data_overlap_array <-
  import_array_overlap(dir = "assets/data/array",
                       osamples = overlaps[["sampleID"]])

# Predict overlap array samples and combine with published
cli::cat_line("Predicting overlap array samples")
pred_dir <- file.path(outputDir, "post_processing", "predictions")
pred_overlap_array <- all_fits %>%
  purrr::map(splendid::prediction, data = data_overlap_array) %>%
  purrr::map(join_published_array, overlap = overlaps) %>%
  purrr::iwalk(~ saveRDS(.x, file.path(
    pred_dir, paste0("pred_overlap_array_", .y, ".rds")
  )))
saveRDS(pred_overlap_array, file.path(pred_dir, "pred_overlap_array.rds"))

# Evaluate overlap array predictions using splendid and caret
cli::cat_line("Evaluating overlap array predictions")
eval_dir <- file.path(outputDir, "post_processing", "evals")
eval_overlap_array <- pred_overlap_array %>%
  purrr::map(evaluate_array) %>%
  purrr::iwalk(~ saveRDS(.x, file.path(
    eval_dir, paste0("eval_overlap_array_", .y, ".rds")
  )))
saveRDS(eval_overlap_array, file.path(eval_dir, "eval_overlap_array.rds"))
