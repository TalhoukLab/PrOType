# create internal validation table
library(magrittr)

df <- list.files(file.path(outputDir, "supervised", "train"), recursive = TRUE, pattern = "*_train_*") %>%
  grep("Model", ., value = TRUE) %>%
  purrr::map(~ readRDS(file.path(outDir, "supervised", "train", .))) %>%
  purrr::set_names(
    list.files(file.path(outputDir, "supervised", "train"), recursive = TRUE, pattern = "*_train_*") %>%
      grep("Model", ., value = TRUE) %>%
      dirname() %>%
      basename()
  ) %>%
  purrr::modify_depth(2, ~ {
    data.frame(measure = rownames(t(.)), t(.)) %>%
      purrr::set_names(c("measure", "percentile_50", "percentile_5", "percentile_95"))
  }) %>%
  purrr::imap(~ purrr::map(.x, function(x) {
      data.frame(normalization = .y, x, stringsAsFactors = FALSE)
    })) %>%
  purrr::map(function(x) {
      purrr::imap(x, ~ data.frame(mod = .y, .x, stringsAsFactors = FALSE))
    }) %>%
  purrr::map_df(dplyr::bind_rows) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    batch_correction = purrr::map_chr(
      strsplit(normalization, split = "_"), ~ .[3]) %>%
      as.factor(),
    normalization = purrr::map_chr(
      strsplit(normalization, split = "_"), ~ .[1]) %>%
      substr(start = 7, stop = nchar(.)) %>%
      ifelse(. == "", "None", .) %>%
      as.factor()
  )

# write results to file
readr::write_rds(df, file.path(outputDir, "iv_summary", "summary", dataset, paste0("iv_summary_", dataset, ".rds")))
readr::write_rds(df, file.path(outputDir, "iv_summary", "summary", dataset, paste0("iv_summary_", dataset, "_threshold.rds")))