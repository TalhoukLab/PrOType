`%>%` <- magrittr::`%>%`
# input fixed cdat,sfdir,r,k
# input variable algs, seed (s)

# Fixed Inputs: cdat, ndat, sfdir, ssclust
# Variable Inputs: algs, s

# Impute E to make E_knn and save to imputed directory
# pl_impute <- function(E, data, seed = 123, dir.name = ".") {
#   E_knn <- apply(E, 2:4, diceR::impute_knn, data = data, seed = seed)
#   saveRDS(E_knn, file.path(dir.name, paste0("E_knn_", algs, s, "_", dataset, ".rds")))
# }

# Fixed Inputs: ndat, sfdir, ssclust
# Variable Inputs: algs, s

# Compute the consensus Matrix
# pl_conmat <- function(E, dir.name = ".") {
#   conmat <- diceR::consensus_combine(E, element = "matrix") %>%
#     purrr::modify_depth(2, ~ Matrix::Matrix(., sparse = TRUE))
#   saveRDS(conmat, file.path(dir.name, paste0("CM_", algs, s, "_", dataset, ".rds")))
# }

# rds_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("rds_out_", dataset))
# imputed_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("imputed_clust_", dataset))

raw_path <- file.path(outputDir, "unsupervised", "clustering", "raw_clust", dataset)
imputed_path <- file.path(outputDir, "unsupervised", "clustering", "imputed_clust", dataset)
outputFile <- file.path(raw_path, paste0("E_", alg, s, "_", dataset))

# con_mat_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("con_mat_", dataset))

if (file.exists(paste0(outputFile, ".rds")) && !shouldCompute) {
  cli::cat_line("File already exists, skipping")
  quit(status = 0)
}

cdat <- readRDS(file.path(outputDir, "unsupervised", "prep_data", dataset,
                          paste0("cdat_", dataset, ".rds")))
cc_args <- tibble::lst(
  data = cdat,
  nk = k,
  reps = 1,
  prep.data = "none",
  seed.data = s,
  file.name = outputFile
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
saveRDS(E_knn, file.path(imputed_path, paste0("E_knn_", alg, s, "_", dataset, ".rds")))

# pl_impute(E = E, data = cdat, dir.name = imputed_path)
# pl_conmat(E = E, dir.name = consmat_path)
