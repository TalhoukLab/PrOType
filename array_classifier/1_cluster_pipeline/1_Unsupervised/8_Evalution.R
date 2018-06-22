library(purrr)
library(diceR)

hc <- function(x, k, method = "average") {
  as.integer(stats::cutree(stats::hclust(stats::dist(x), method = method), k))
}


ivi_table <- function(cl.df, data) {
  ndata <- apply(data, 2, function(x) as.numeric(as.character(x)))
  data.frame(
    Algorithms = colnames(cl.df),
    cl.df %>% purrr::map_df(
      clusterCrit::intCriteria,
      traj = ndata,
      crit = c("Calinski_Harabasz", "Dunn", "PBM", "Tau", "Gamma", "C_index",
               "Davies_Bouldin", "McClain_Rao", "SD_Dis", "Ray_Turi", "G_plus",
               "Silhouette", "S_Dbw")),
    Compactness = cl.df %>% purrr::map_dbl(compactness, data = data),
    Connectivity = cl.df %>% purrr::map_dbl(
      ~ clValid::connectivity(Data = ndata, clusters = .))
  ) %>%
    dplyr::mutate_all(dplyr::funs(structure(., names = colnames(cl.df))))
}

consensus_rank <- function(ii, n) {
  # Extract internal indices from clusterCrit and remove NaN idx
  ii.cc <- ii %>%
    magrittr::extract(
      !names(.) %in% c("Algorithms", "Compactness", "Connectivity") &
        purrr::map_lgl(., ~ all(!is.na(.x)))
    )

  # Which algorithm is the best for each index?
  bests <- purrr::imap_int(ii.cc, clusterCrit::bestCriterion)
  max.bests <- ii.cc %>%
    magrittr::extract(purrr::map_int(., which.max) == bests) %>%
    magrittr::multiply_by(-1)
  min.bests <- ii.cc %>%
    magrittr::extract(purrr::map_int(., which.min) == bests) %>%
    cbind(ii[c("Compactness", "Connectivity")]) # these two need to be minimized

  # Determine trimmed ensemble using rank aggregation
  if (nrow(ii) <= n) {
    rank.matrix <- top.list <- NULL
  } else {
    rank.matrix <- cbind(max.bests, min.bests) %>%
      scale(center = FALSE, scale = TRUE) %>%
      as.data.frame() %>%
      purrr::map_df(~ ii$Algorithms[order(.x, sample(length(.x)))]) %>%
      t()
    top.list <- RankAggreg::RankAggreg(rank.matrix, ncol(rank.matrix),
                                       method = "GA", verbose = FALSE)$top.list
  }
  dplyr::lst(max.bests, min.bests, rank.matrix, top.list)
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
ii <- ivi_table(finalR, cdat)
ii <- list(ii)[[1]]

cat("Ranking aggregates\n")
# Rank aggregate
cr <- diceR:::consensus_rank(ii, n = 5)
top <- cr$top.list
ii <- ii[match(top, ii$Algorithms), ]
finalR <- finalR[, top]

cat("Saving RDS\n")
saveRDS(finalR, paste0(ddir, "/all_clusts_", dataset, ".rds"))
saveRDS(ii, paste0(ddir, "/ii_", dataset, ".rds"))
print(readRDS(paste0(ddir, "/ii_", dataset, ".rds")))
