# Inputs ------------------------------------------------------------------
library(magrittr)

cli::cat_line("Processing:", dataset)

f <- list.files(path = paste0(fdir, ndat[i], "/", mname, "_", ndat[i]),
                  pattern = paste0("_train_eval_", ndat[i], "(_threshold)?.rds"),
                  full.names = TRUE) %>%
    `[`(c(1, 3, 4, 2))
res <- purrr::map(f, readRDS) %>% purrr::flatten()

saveRDS(res, file.path(outputDir, "iv_summary", "train_eval", dataset, paste0("train_eval_", dataset, ".rds")))
