library(purrr)
library(diceR)

hc <- function(x, k, method = "average") {
  as.integer(stats::cutree(stats::hclust(stats::dist(x), method = method), k))
}

# Input-----
dataset <- dat

# Final Clustering ----
ddir <- paste0(fdir, dataset, "/data_pr_", dataset)

# Read in the data used for clustering
cdat <- readRDS(paste0(ddir, "/cdat_", dataset, ".rds"))

# Read in the consensus matrices
cons.mat <- readRDS(paste0(ddir, "/Final_CM_", dataset, ".rds"))

# Obtain HC from each algorithm and relabel
cl.mat <- purrr::map(cons.mat, hc, k = 4) %>%
  lapply(diceR::relabel_class, ref.cl = .[[1]]) %>%
  data.frame()

cat(paste0(ddir, "/cons_CSPA_", dataset, ".rds"), "\n")

final <- data.frame(
  CSPA = readRDS(paste0(ddir, "/cons_CSPA_", dataset, ".rds")),
  kmodes = readRDS(paste0(ddir, "/cons_kmodes_", dataset, ".rds")),
  majority = readRDS(paste0(ddir, "/cons_majority_", dataset, ".rds")),
  cts = readRDS(paste0(ddir, "/cons_LCEcts_", dataset, ".rds")),
  srs = readRDS(paste0(ddir, "/cons_LCEsrs_", dataset, ".rds")),
  asrs = readRDS(paste0(ddir, "/cons_LCEasrs_", dataset, ".rds")),
  cl.mat
)

cat("Relabeling elements\n")
# relabel the elements of the data frame
finalR <- final
finalR[] <- apply(final, 2, diceR::relabel_class, ref.cl = final[, referenceClass])

cat("Evaluating clustering\n")
# Cluster evaluate at this point
ii <- diceR:::ivi_table(finalR, cdat)

cat("Ranking aggregates\n")
# Rank aggregate
# WHY IS ii[s_dbw] NaN?
cr <- diceR:::consensus_rank(ii, n = 5)
top <- cr$top.list
ii <- ii[match(top, ii$Algorithms), ]
finalR <- finalR[, top]

cat("Saving RDS\n")
saveRDS(finalR, paste0(ddir, "/all_clusts_", dataset, ".rds"))
saveRDS(ii, paste0(ddir, "/ii_", dataset, ".rds"))
print(readRDS(paste0(ddir, "/ii_", dataset, ".rds")))
