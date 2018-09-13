`%>%` <- magrittr::`%>%`
# Evaluations merged by algorithm
files <- list.files(
  path = file.path(outputDir, "supervised", "reduce",
                   paste0(mname, "_", dataset)),
  pattern = paste0("_train_eval_", dataset),
  full.names = TRUE
)
# Sort first to fourth, then flatten list
eval_all <- files %>%
  `[`(c(1, 3, 4, 2)) %>%
  purrr::map(readRDS) %>%
  purrr::flatten()
# Write all evaluations merged
saveRDS(
  eval_all,
  file.path(outputDir, "supervised", "train_eval",
            paste0("train_eval_", dataset, ".rds"))
)
