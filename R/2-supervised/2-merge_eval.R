`%>%` <- magrittr::`%>%`
# All evaluation files
eval_files <- algs %>%
  purrr::map(
    list.files,
    path = file.path(outputDir, "supervised", "train_eval", dataset),
    full.names = TRUE
  )
# Compute median + 95% CI of evaluations within algorithm and merge
eval_merged <- eval_files %>%
  purrr::map(~ purrr::map(., readRDS)) %>%
  purrr::map(purrr::transpose) %>%
  purrr::flatten() %>%
  purrr::map(~ apply(data.frame(.), 1, quantile, c(0.5, 0.05, 0.95),
                     na.rm = TRUE))
# Write all evaluations merged
saveRDS(
  eval_merged,
  file.path(outputDir, "supervised", "merge_eval",
            paste0("merge_eval_", dataset, ".rds"))
)
