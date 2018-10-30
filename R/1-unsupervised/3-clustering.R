source(here::here("R/1-unsupervised/utils.R"))

raw_name <- file.path(outputDir, "unsupervised", "clustering", "raw_clust",
                      dataset, paste0("E_", alg, s, "_", dataset))

if (file.exists(paste0(raw_name, ".rds")) && !shouldCompute) {
  cli::cat_line("File already exists, skipping")
  quit(status = 0)
}

cdat <- readRDS(file.path(outputDir, "unsupervised", "prep_data",
                          dataset, paste0("cdat_", dataset, ".rds")))
cc_args <- tibble::lst(
  data = cdat,
  nk = k,
  reps = 1,
  prep.data = "none",
  seed.data = as.integer(s),
  file.name = raw_name
)

E <- switch(
  alg,
  nmfbrunet = purrr::invoke(diceR::consensus_cluster, cc_args,
                            algorithms = "nmf", nmf.method = "brunet"),
  nmflee = purrr::invoke(diceR::consensus_cluster, cc_args,
                         algorithms = "nmf", nmf.method = "lee"),
  distalgs = purrr::invoke(diceR::consensus_cluster, cc_args,
                           algorithms = c("km", "pam"),
                           distance = c("eucl", "spear", "manh")),
  rest = purrr::invoke(diceR::consensus_cluster, cc_args,
                       algorithms = "block")
)

E_knn <- apply(E, 2:4, diceR::impute_knn, data = cdat, seed = 123)
saveRDS(E_knn, file.path(outputDir, "unsupervised", "clustering",
                         "imputed_clust", dataset,
                         paste0("E_knn_", alg, s, "_", dataset, ".rds")))
