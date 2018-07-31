library(purrr)
source(here::here("R/unsupervised/utils.R"))

# Input-----
dataset <- dat

# Final Clustering ----
ddir <- file.path(fdir, dataset, paste0("data_pr_", dataset))

# Read in the data used for clustering
cdat <- readRDS(file.path(ddir, paste0("cdat_", dataset, ".rds")))

# Read in the consensus matrices
cons.mat <- readRDS(file.path(ddir, paste0("Final_CM_", dataset, ".rds")))

# Obtain HC from each algorithm and relabel
cl.mat <- purrr::map(cons.mat, hc, k = 4) %>%
  lapply(diceR::relabel_class, ref.cl = .[[1]]) %>%
  data.frame()

final <- data.frame(
  CSPA =     readRDS( file.path(ddir, paste0("cons_CSPA_", dataset, ".rds"))),
  kmodes =   readRDS( file.path(ddir, paste0("cons_kmodes_", dataset, ".rds"))),
  majority = readRDS( file.path(ddir, paste0("cons_majority_", dataset, ".rds"))),
  cts =      readRDS( file.path(ddir, paste0("cons_LCEcts_", dataset, ".rds"))),
  srs =      readRDS( file.path(ddir, paste0("cons_LCEsrs_", dataset, ".rds"))),
  asrs =     readRDS( file.path(ddir, paste0("cons_LCEasrs_", dataset, ".rds"))),
  cl.mat
)

cli::cat_line("Relabeling elements")
# relabel the elements of the data frame
finalR <- final
finalR[] <- apply(final, 2, diceR::relabel_class, ref.cl = final[, referenceClass])

cli::cat_line("Evaluating clustering")
# Cluster evaluate at this point
ii <- diceR:::ivi_table(finalR, cdat)
ii <- list(ii)[[1]]

cli::cat_line("Ranking aggregates")

# Rank aggregate
cr <- diceR:::consensus_rank(ii, n = 5)
top <- cr$top.list
ii <- ii[match(top, ii$Algorithms), ]
finalR <- finalR[, top]

cli::cat_line("Saving RDS")
saveRDS(finalR, file.path(ddir, paste0("all_clusts_", dataset, ".rds")))
saveRDS(ii,     file.path(ddir, paste0("ii_", dataset, ".rds")))
print(ii)
