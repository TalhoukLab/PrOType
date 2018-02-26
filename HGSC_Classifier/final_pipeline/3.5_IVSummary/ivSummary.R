# import dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  require(data.table)
})

# create internal validation table
df <- list.files(fdir, recursive = TRUE, pattern = "*_train_*") %>%
  grep("Model", ., value = TRUE) %>%
  purrr::map(., ~readRDS(paste0(fdir, "/", .x))) %>%
  purrr::set_names({
    list.files(fdir, recursive = TRUE, pattern = "*_train_*") %>%
      grep("Model", ., value = TRUE) %>%
      dirname() %>% basename()
  }) %>%
  purrr::modify_depth(., 2, function(x) {
    data.frame(measure = rownames(t(x)), t(x)) %>%
      set_names(c("measure", "percentile_50", "percentile_5", "percentile_95"))
  }) %>%
  purrr::map2(., names(.), function(x, y) {
    purrr::modify_depth(x, 1, ~data.frame(normalization = y, .x))
  }) %>%
  purrr::modify_depth(., 1, function(x) {
    purrr::map2(x, names(x), ~data.frame(mod = .y, .x))
  }) %>%
  purrr::map(., ~data.table::rbindlist(.x)) %>%
  data.table::rbindlist() %>% as_tibble() %>%
  mutate(
    batch_correction = purrr::map(
      strsplit(as.character(normalization), split = "_"), ~.x[3]) %>%
      unlist %>% as.factor
  ) %>%
  mutate(
    normalization = purrr::map(
      strsplit(as.character(normalization), split = "_"), ~.x[1]) %>% 
      substr(start = 7, stop=nchar(.)) %>% unlist %>% as.factor
  ) %>%
  mutate(
    normalization = ifelse(normalization == "", "None", as.character(normalization)) %>%
      as.factor()
  )

# write results to file
readr::write_rds(df, sdir)
