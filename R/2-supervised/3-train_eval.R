# Inputs ------------------------------------------------------------------
`%>%` <- magrittr::`%>%`

cli::cat_line("Processing:", dataset)

f <- list.files(path = file.path(outputDir, "supervised", "reduce", paste0(mname, "_", dataset)),
                  pattern = paste0("_train_eval_", dataset, "(_threshold)?.rds"),
                  full.names = TRUE) %>%
    `[`(c(1, 3, 4, 2))
res <- purrr::map(f, readRDS) %>% purrr::flatten()

saveRDS(res, file.path(outputDir, "supervised", "train_eval", paste0("train_eval_", dataset, ".rds")))
