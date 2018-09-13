`%>%` <- magrittr::`%>%`

# Read in all iv_summary and row bind
x <- list.files(
  path = file.path(outputDir, "supervised", "iv_summary"),
  full.names = TRUE,
  recursive = TRUE
) %>%
  grep(pattern = "(?<!threshold|COMBINED)\\.rds",
       perl = TRUE,
       value = TRUE) %>%
  purrr::map(readRDS) %>%
  purrr::map_df(~ dplyr::mutate_at(., "batch_correction", as.character)) %>%
  dplyr::mutate_at("batch_correction", as.factor)

# Sanity check the top accuracies per algorithm
top_accuracy <- x %>%
  dplyr::filter(measure == "accuracy") %>%
  dplyr::arrange(dplyr::desc(percentile_50)) %>%
  dplyr::distinct()

# Write out iv_summary combined
saveRDS(
  object = x,
  file = file.path(outputDir, "supervised", "iv_summary", "iv_summary_COMBINED.rds")
)
