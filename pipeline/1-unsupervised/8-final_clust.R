source(here::here("pipeline/1-unsupervised/utils.R"))

# Final Clustering ----
ddir <- file.path(outputdir, "unsupervised", "consensus", dataset)

# Read in the data used for clustering
cdat <- readRDS(file.path(outputdir, "unsupervised", "prep_data", dataset, paste0("cdat_", dataset, ".rds")))

# Read in the consensus matrices
cons.mat <- readRDS(file.path(outputdir, "unsupervised", "data_pr", dataset, paste0("Final_CM_", dataset, ".rds")))

# Obtain HC from each algorithm and relabel
cl.mat <- purrr::map(cons.mat, hc, k = 4) %>%
  lapply(diceR::relabel_class, ref.cl = .[[1]]) %>%
  data.frame()

final <- data.frame(
  CSPA =     readRDS(file.path(ddir, paste0("cons_CSPA_", dataset, ".rds"))),
  kmodes =   readRDS(file.path(ddir, paste0("cons_kmodes_", dataset, ".rds"))),
  majority = readRDS(file.path(ddir, paste0("cons_majority_", dataset, ".rds"))),
  cts =      readRDS(file.path(ddir, paste0("cons_LCEcts_", dataset, ".rds"))),
  srs =      readRDS(file.path(ddir, paste0("cons_LCEsrs_", dataset, ".rds"))),
  asrs =     readRDS(file.path(ddir, paste0("cons_LCEasrs_", dataset, ".rds"))),
  cl.mat
)

# relabel the elements of the data frame
cli::cat_line("Relabeling elements")
finalR <- final
finalR[] <-
  apply(final, 2, diceR::relabel_class, ref.cl = final[, referenceClass])

# evaluate clustering with internal validity indices
cli::cat_line("Evaluating clustering")
ii <- diceR:::ivi_table(finalR, cdat)

# rank aggregate
cli::cat_line("Applying rank aggregation")
set.seed(1)
cr <- diceR:::consensus_rank(ii, n = 5)
top <- cr$top.list
ii <- ii[match(top, ii$Algorithms), ]
finalR <- finalR[, top]
rownames(finalR) <- rownames(cdat)

# write to file
cli::cat_line("Saving RDS")
saveRDS(finalR, file.path(outputdir, "unsupervised", "final", dataset, paste0("all_clusts_", dataset, ".rds")))
saveRDS(ii,     file.path(outputdir, "unsupervised", "final", dataset, paste0("ii_", dataset, ".rds")))
