# input fixed cdat,sfdir,r,k
# input variable algs, seed (s)

# Fixed Inputs: cdat, ndat, sfdir, ssclust
# Variable Inputs: algs, s

# Impute E to make E_knn and save to imputed directory
pl_impute <- function(E, data, seed = 123, dir.name = ".") {
  E_knn <- apply(E, 2:4, diceR::impute_knn, data = data, seed = seed)
  saveRDS(E_knn, file.path(sfdir, paste0("imputed_clust_", dataset), paste0("E_knn_", algs, s, "_", ndat, ".rds")))
}

# Fixed Inputs: ndat, sfdir, ssclust
# Variable Inputs: algs, s

# Compute the consensus Matrix
pl_conmat <- function(E, dir.name = ".") {
  conmat <- diceR::consensus_combine(E, element = "matrix")
  #conmat_sparse = purrr::map(conmat, ~ purrr::map(., ~ Matrix::Matrix(., sparse = TRUE)))
  saveRDS(conmat, file.path(dir.name, paste0("CM_", algs, s, "_", dataset, ".rds")))
}

rds_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("rds_out_", dataset))
con_mat_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("con_mat_", dataset))
imputed_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("imputed_dir_", dataset))

cdat<- readRDS(file.path(outputdir, "unsupervised", "prep_data", dataset, paste0("cdat_", dataset, ".rds")))
outputFile <- file.path(rds_dir, paste0("rds_out_", dataset), paste0(algs, s, "_", dataset))

cli::cat_line("Checking previous Input")
if (file.exists(paste0(outputFile, ".rds")) && !shouldCompute) {
      cli::cat_line("File already exists, skipping.\n")
      quit(status = 0)
}

r<-1

ssclust <- switch(algs,
nmfbrunet = {
        diceR::consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "brunet",
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    nmflee = {
        diceR::consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "lee",
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    distalgs = {
        diceR::consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("km", "pam"),
        distance = c("eucl", "spear", "manh"),
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    rest = {
        diceR::consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("block"), #,"ap"), #,"gmm"),
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    }
)

pl_impute(E=ssclust, data = cdat, dir.name = imputed_dir)
pl_conmat(E = ssclust, dir.name = con_mat_dir)
