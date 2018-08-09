# Inputs ------------------------------------------------------------------
library(magrittr)

cli::cat_line("Processing:", dataset)

f <- list.files(file.path(outputDir, "supervised", "reduce", dataset),
                  pattern = paste0("_train_eval_", dataset, "(_threshold)?.rds"))
algs <- data.frame(f = f) %>%
    tidyr::separate(f, "algs", sep = "_", extra = "drop")
algs$algs[algs$algs == "second"] <- "svm"
f_Rfe <- grep("Rfe", f, value = TRUE)
f_rest <- f[!f %in% f_Rfe]

res <- purrr::map(file.path(outputDir, "supervised", "reduce", dataset, f_rest),
                  readRDS) %>%
    unlist(recursive = FALSE)

saveRDS(res, file.path(outputDir, "iv_summary", "train_eval", dataset, paste0("train_eval_", dataset, ".rds")))
