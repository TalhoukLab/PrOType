# create internal validation table
df <- list.files(fdir, recursive = TRUE, pattern = "*_train_*") %>%
  grep("Model", ., value = TRUE) %>%
  purrr::map(~ readRDS(paste0(fdir, "/", .))) %>%
  purrr::set_names(
    list.files(fdir, recursive = TRUE, pattern = "*_train_*") %>%
      grep("Model", ., value = TRUE) %>%
      dirname() %>%
      basename()
  ) %>%
  purrr::modify_depth(2, ~ {
    data.frame(measure = rownames(t(.)), t(.)) %>%
      purrr::set_names(c("measure", "percentile_50", "percentile_5", "percentile_95"))
  }) %>%
  purrr::imap(~ {
    purrr::modify_depth(.x, 1, ~ data.frame(normalization = .y, .x))
  }) %>%
  purrr::modify_depth(1, ~ purrr::imap(~ data.frame(mod = .y, .x))) %>%
  purrr::map(~ data.table::rbindlist(.)) %>%
  data.table::rbindlist() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    batch_correction = purrr::map(
      strsplit(as.character(normalization), split = "_"), ~ .[3]) %>%
      unlist() %>%
      as.factor(),
    normalization = purrr::map(
      strsplit(as.character(normalization), split = "_"), ~ .[1]) %>%
      substr(start = 7, stop = nchar(.)) %>%
      unlist() %>%
      as.factor()
  ) %>%
  dplyr::mutate(
    normalization = ifelse(normalization == "", "None", as.character(normalization)) %>%
      as.factor()
  )

# write results to file
readr::write_rds(df, sdir)
