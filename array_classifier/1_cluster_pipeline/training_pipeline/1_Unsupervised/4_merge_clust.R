# Merge results together to form array
# Inputs: data_directory, reps, algs

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
  fails <- NULL
  cat("Merging Seeds\n")
  # Merge the seeds within algorithm when all have completed
  if (!all(seq_len(reps) %in% seeds)) {
    cat(paste(algs, "failed:"))
    fails <- which(!(seq_len(reps) %in% seeds))
    cat(fails, "\n")
    error <- 1
  }
  # Merge rds_out
  cat("Merge rds out\n")
  # Combine reps that succeeded
  lalgo <- lapply(paste0(newdir, algF), readRDS) %>%
    abind::abind(along = 2)
  dimnames(lalgo)[[2]]<-paste0("R",seeds)


  if (is.null(fails)) {
    cat("Nothing failed, returning early...\n")
    return(lalgo)
  } else {
    # Create dummy array for failed reps
    fail_reps <- array(
      data = NA_real_,
      dim = replace(dim(lalgo), 2, length(fails)),
      dimnames = list(NULL, paste0("R", fails), NULL, NULL)
    )
    # Merge to make a filled array with `reps` columns
    lalgo_filled <- list(lalgo, fail_reps) %>%
      abind::abind(along = 2) %>%
      `[`(, order(colnames(.)), , 1, drop = FALSE)
    return(lalgo_filled)
  }
}

# Merge the raw clustering
cat("Merging raw clustering\n")
fnames <- list.files(path = paste0(data_directory, "/rds_out_", ndat, "/")) %>%
  gtools::mixedsort()
newdir <- paste0(data_directory, "/rds_out_", ndat, "/")

cat("l_apply\n")
E <- lapply(algs, multMerge, fnames = fnames, newdir = newdir) %>%
  abind::abind(along = 3)
cat("Saving RDS\n")
saveRDS(E, file = paste0(data_directory, "/data_pr_", ndat, "/E_", ndat, ".rds"))

# Merge KNN_imputed clustering
cat("Merging KNN imputed clustering")
fnames <- list.files(path = paste0(data_directory, "/imputed_clust_", ndat, "/")) %>%
  gtools::mixedsort()
newdir <- paste0(data_directory, "/imputed_clust_", ndat, "/")

E_knn <- lapply(algs, multMerge, fnames = fnames, newdir = newdir) %>%
  abind::abind(along = 3)
saveRDS(E_knn, file = paste0(data_directory, "/data_pr_", ndat, "/E_knn_", ndat, ".rds"))

# Completed clustering
cat("Completing Clustering\n")
cdat <- readRDS(paste0(data_directory, "/data_pr_", ndat, "/cdat_", ndat, ".rds"))
Ecomp <- diceR::impute_missing(E_knn, data = cdat, nk = 4)
saveRDS(Ecomp, file = paste0(data_directory, "/data_pr_", ndat, "/Ecomp_", ndat, ".rds"))

rm(fnames, newdir, E, E_knn, Ecomp)
