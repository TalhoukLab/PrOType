for (i in seq_along(ndat)) {
  cli::cat_line("Processing: ", ndat[i])
  f <- list.files(path = paste0(fdir, ndat[i], "/", mname, "_", ndat[i]),
                  pattern = paste0("_train_eval_", ndat[i], "(_threshold)?.rds"),
                  full.names = TRUE) %>%
    `[`(c(1, 3, 4, 2))
  res <- purrr::map(f, readRDS) %>% purrr::flatten()

  saveRDS(res, paste0(fdir, ndat[i], "/data_pr_", ndat[i], "/train_eval_", ndat[i], ".rds"))
}
