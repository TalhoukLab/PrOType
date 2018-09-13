# `%>%` <- magrittr::`%>%`
# # Evaluations merged by algorithm
# files <- list.files(
#   path = file.path(outputDir, "supervised", "reduce",
#                    paste0(mname, "_", dataset)),
#   pattern = paste0("_train_eval_", dataset),
#   full.names = TRUE
# )
# # Sort first to fourth, then flatten list
# eval_all <- files %>%
#   `[`(c(1, 3, 4, 2)) %>%
#   purrr::map(readRDS) %>%
#   purrr::flatten()
# # Write all evaluations merged
# saveRDS(
#   eval_all,
#   file.path(outputDir, "supervised", "train_eval",
#             paste0("train_eval_", dataset, ".rds"))
# )
`%>%` <- magrittr::`%>%`
# All evaluation files
eval_files <- algs %>%
  purrr::map(
    list.files,
    path = file.path(outputDir, "supervised", "train",
                     paste0(mname, "_", dataset)),
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
  file.path(outputDir, "supervised", "train_eval",
            paste0("train_eval_", dataset, "2.rds"))
)
