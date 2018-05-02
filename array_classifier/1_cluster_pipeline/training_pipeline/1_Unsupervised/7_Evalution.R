library(purrr)
library(diceR)

hc <- function(x, k, method = "average") {
  as.integer(stats::cutree(stats::hclust(stats::dist(x), method = method), k))
}

# Input-----
alldat <- dat

# Final Clustering ----
for (i in seq_along(alldat)) {
  ddir <- paste0(fdir, alldat[i], "/data_pr_", alldat[i])

  # Read in the data used for clustering
  cdat <- readRDS(paste0(ddir, "/cdat_", alldat[i], ".rds"))

  # Read in the consensus matrices
  cons.mat <- readRDS(paste0(ddir, "/Final_CM_", alldat[i], ".rds"))

  # Obtain HC from each algorithm and relabel
  cl.mat <- purrr::map(cons.mat, hc, k = 4) %>%
    lapply(diceR::relabel_class, ref.cl = .[[1]]) %>%
    data.frame()

  final <- data.frame(
    CSPA = readRDS(paste0(ddir, "/cons_CSPA_", alldat[i], ".rds")),
    kmodes = readRDS(paste0(ddir, "/cons_kmodes_", alldat[i], ".rds")),
    majority = readRDS(paste0(ddir, "/cons_majority_", alldat[i], ".rds")),
    cts = readRDS(paste0(ddir, "/cons_LCEcts_", alldat[i], ".rds")),
    srs = readRDS(paste0(ddir, "/cons_LCEsrs_", alldat[i], ".rds")),
    asrs = readRDS(paste0(ddir, "/cons_LCEasrs_", alldat[i], ".rds")),
    cl.mat
  )

  # relabel the elements of the data frame
  finalR <- final
  finalR[] <- apply(final, 2, diceR::relabel_class, ref.cl = final[, "majority"])

  # Cluster evaluate at this point
  ii <- diceR:::ivi_table(finalR, data)

  # Rank aggregate
  cr <- diceR:::consensus_rank(ii, n = 5)
  top <- cr$top.list
  ii <- ii[match(top, ii$Algorithms), ]
  finalR <- finalR[, top]

  saveRDS(finalR, paste0(ddir, "/all_clusts_", alldat[i], ".rds"))
  saveRDS(ii, paste0(ddir, "/ii_", alldat[i], ".rds"))
}
