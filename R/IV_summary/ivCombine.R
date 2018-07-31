# command line arguments
library(magrittr)
args <- commandArgs(trailingOnly = TRUE)

cli::cat_line("Combining files\n")
list.files(file.path(outputDir, ndat, paste0("data_pr_", ndat)), recursive = TRUE, pattern = "iv_summary_ov*") %>%
  grep("iv_summary_ov.*", ., value = TRUE) %>%
  purrr::map(~ readRDS(file.path(args[1], .))) %>%
  data.table::rbindlist() %>%
  readr::write_rds(file.path(args[1], "iv_summary_COMBINED.rds"))
