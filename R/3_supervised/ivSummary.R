# create internal validation table
library(magrittr)

model <- basename(grep("Model", list.dirs(file.path(outputDir, "supervised", "reduce")), value = TRUE))
df <- list.files(
  path = file.path(outputDir, "supervised", "train_eval"),
  pattern = "train_eval",
  full.names = TRUE
) %>%
  readRDS() %>%
  purrr::imap_dfr(~ {
    t(.x) %>%
      as.data.frame() %>%
      purrr::set_names(paste0("percentile_", gsub("%", "", names(.)))) %>%
      tibble::rownames_to_column("measure") %>%
      tibble::add_column(mod = .y, model, .before = 1) %>%
      tidyr::separate(col = model,
                      into = c("normalization", "data", "batch_correction"),
                      sep = "_") %>%
      dplyr::select(-data, -batch_correction, batch_correction) %>%
      dplyr::mutate(normalization = gsub("Model-", "", normalization) %>%
                      ifelse(. == "", "None", .)) %>%
      dplyr::mutate_at(c("normalization", "measure", "batch_correction"),
                       as.factor)
  }) %>%
  tibble::as_tibble()

# write results to file
readr::write_rds(df, file.path(outputDir, "supervised", "summary", dataset, paste0("iv_summary_", dataset, ".rds")))
readr::write_rds(df, file.path(outputDir, "supervised", "summary", dataset, paste0("iv_summary_", dataset, "_threshold.rds")))
