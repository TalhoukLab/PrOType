# Merge results together to form array
# Inputs: dir, reps, algs

library(magrittr)

multMerge <- function(algs, fnames, newdir) {
  #cat(algs, fnames, newdir, reps)
  # Separate the algorithms
  algF <- unique(grep(algs, fnames, value = TRUE))
  # Get the seeds
  cat("Getting seeds\n")
  temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
  seeds <- as.numeric(purrr::map_chr(temp, `[`, 1))
  error <- 0
  cat("Merging Seeds\n")
  # Merge the seeds within algorithm when all have completed
  if (!all(seq_len(reps) %in% seeds)) {
    cat(paste(algs, "failed:"))
    cat(which(!(seq_len(reps) %in% seeds)), "\n")
    error <- 1
  }
  # Merge rds_out
  cat("Merge rds out\n")
  lalgo <- lapply(paste0(newdir, algF), readRDS) %>% abind::abind(along = 2)
  dimnames(lalgo)[[2]] <- paste0("R", seeds)
  lalgo
}

# Merge the raw clustering
cat("Merging raw clustering\n")
fnames <- list.files(path = paste0(dir, "/rds_out_", ndat, "/")) %>%
  gtools::mixedsort()
newdir <- paste0(dir, "/rds_out_", ndat, "/")

cat("l_apply\n")
E <- lapply(algs, multMerge, fnames = fnames, newdir = newdir) %>%
  abind::abind(along = 3)
cat("Saving RDS\n")
saveRDS(E, file = paste0(dir, "/data_pr_", ndat, "/E_", ndat, ".rds"))

# Merge KNN_imputed clustering
cat("Merging KNN imputed clustering")
fnames <- list.files(path = paste0(dir, "/imputed_clust_", ndat, "/")) %>%
  gtools::mixedsort()
newdir <- paste0(dir, "/imputed_clust_", ndat, "/")

E_knn <- lapply(algs, multMerge, fnames = fnames, newdir = newdir) %>%
  abind::abind(along = 3)
saveRDS(E_knn, file = paste0(dir, "/data_pr_", ndat, "/E_knn_", ndat, ".rds"))

# Completed clustering
cat("Completing Clustering")
cdat <- readRDS(paste0(dir, "/data_pr_", ndat, "/cdat_", ndat, ".rds"))
Ecomp <- diceR::impute_missing(E_knn, data = cdat, nk = 4)
saveRDS(Ecomp, file = paste0(dir, "/data_pr_", ndat, "/Ecomp_", ndat, ".rds"))

rm(fnames, newdir, E, E_knn, Ecomp)
