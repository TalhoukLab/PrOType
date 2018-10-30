source(here::here("assets/utils.R"))

# Read in all iv_summary
x <- list.files(
  path = file.path(outputDir, "supervised", "summary"),
  full.names = TRUE,
  recursive = TRUE
) %>%
  grep(pattern = "(?<!threshold|COMBINED)\\.rds",
       perl = TRUE,
       value = TRUE) %>%
  purrr::set_names(basename(dirname(.))) %>%
  purrr::map(readRDS) %>%
  purrr::map(dplyr::mutate_at, "batch_correction", as.character)

# Sanity check the top accuracies per algorithm for each dataset
top_accuracy <- x %>%
  purrr::map(~ {
    dplyr::filter(., measure == "accuracy") %>%
      dplyr::arrange(dplyr::desc(percentile_50)) %>%
      dplyr::distinct()
  })

# Write out iv_summary combined
ivs_combined <- x %>%
  dplyr::bind_rows() %>%
  dplyr::mutate_at("batch_correction", as.factor)
saveRDS(
  object = ivs_combined,
  file = file.path(outputDir, "supervised", "summary", "iv_summary_COMBINED.rds")
)
