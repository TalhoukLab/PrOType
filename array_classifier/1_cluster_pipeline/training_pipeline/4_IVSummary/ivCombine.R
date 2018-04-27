# command line arguments
args <- commandArgs(trailingOnly = TRUE)

list.files(args[1], recursive = TRUE, pattern = "iv_summary_ov*") %>%
  grep("iv_summary_ov.*", ., value = TRUE) %>%
  purrr::map(~ readRDS(file.path(args[1], .))) %>%
  data.table::rbindlist() %>%
  readr::write_rds(file.path(args[1], "iv_summary_COMBINED.rds"))
