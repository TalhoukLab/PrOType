# Inputs ------------------------------------------------------------------

fdir <- fdir
ndat <- ndat
mname <- mname

for (i in seq_along(ndat)) {
  print(ndat[i])
  f <- grep(paste0("_train_eval_", ndat[i], ".rds"),
            list.files(paste0(fdir, ndat[i], "/", mname, "_", ndat[i], "/")),
            value = TRUE)
  print(f)
  algs <- data.frame(f = f) %>%
    tidyr::separate(f, "algs", sep = "_", extra = "drop")
  algs$algs[algs$algs == "second"] <- "svm"
  f_Rfe <- grep("Rfe", f, value = TRUE)
  f_rest <- f[!f %in% f_Rfe]

  res <- purrr::map(paste0(fdir, ndat[i], "/", mname, "_", ndat[i], "/", f_rest),
                  readRDS) %>%
    unlist(recursive = FALSE)

  saveRDS(res, paste0(fdir, ndat[i], "/data_pr_", ndat[i], "/train_eval_", ndat[i], ".rds"))
}
