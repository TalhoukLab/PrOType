# Fixed Inputs: ndat, sfdir, ssclust
# Variable Inputs: algs, s

#' Compute the consensus Matrix
pl_conmat <- function(E, dir.name = ".") {
  conmat <- diceR::consensus_combine(E, element = "matrix")
  conmat_sparse = purrr::map(conmat, ~ purrr::map(., ~ Matrix::Matrix(., sparse = TRUE)))
  saveRDS(conmat_sparse, file.path(dir.name, paste0("CM_", algs, s, "_", ndat, ".rds")))
}

pl_conmat(E = ssclust, dir.name = paste0(sfdir, "/con_mat_", ndat))
