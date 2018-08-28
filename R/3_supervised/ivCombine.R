`%>%` <- magrittr::`%>%`

# Read in all iv_summary and row bind
x <- list.files(
  path = file.path(outputDir, "supervised", "summary"),
  pattern = "iv_summary.*",
  full.names = TRUE,
  recursive = TRUE
) %>%
  purrr::map_df(readRDS)

# Sanity check the top accuracies per algorithm
top_accuracy <- x %>%
  dplyr::filter(measure == "accuracy") %>%
  dplyr::arrange(dplyr::desc(percentile_50)) %>%
  dplyr::distinct()

# Write out iv_summary combined
saveRDS(
  object = x,
  file = file.path(outputDir, "supervised", "summary", "iv_summary_COMBINED.rds")
)
