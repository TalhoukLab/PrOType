# Fixed Inputs: cdat, ndat, sfdir, ssclust
# Variable Inputs: algs, s

#' Impute E to make E_knn and save to imputed directory
pl_impute <- function(E, data, seed = 123, dir.name = ".") {
	E_knn <- apply(E, 2:4, diceR::impute_knn, data = data, seed = seed)

	saveRDS(E_knn, paste0(sfdir,"/imputed_clust_", ndat, "/E_knn_", algs, s, "_", ndat, ".rds"))
}

pl_impute(E=ssclust, data = cdat, dir.name = paste0(sfdir, "/imputed_clust_", ndat))
