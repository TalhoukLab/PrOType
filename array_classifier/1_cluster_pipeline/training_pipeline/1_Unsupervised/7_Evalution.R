library(diceR)

#' Parallel evaluation
pl_evaluate <- function(data, nk, n = 5) {
  fs::dir_create("evaluate")
  # Read in the consensus matrices
  cons.mat <- fs::dir_ls("conmat") %>%
    purrr::map(readRDS)
  cl.mat <- readRDS("merged/conmat.rds") %>%
    purrr::map(~ hc(stats::dist(.), k = nk)) %>%
    purrr::map_df(relabel_class, ref.cl = .[[1]])
  final <- fs::dir_ls("consensus") %>%
    purrr::map_df(readRDS) %>%
    dplyr::mutate_all(as.integer) %>%
    purrr::set_names(gsub(".*cons_(.*).rds", "\\1", names(.))) %>%
    cbind(cl.mat)

  # relabel the elements of the data frame
  finalR <- final
  finalR[] <- apply(final, 2, relabel_class, ref.cl = final[, "majority"])

  # Cluster evaluate at this point
  ii <- ivi_table(finalR, data)

  # Separate algorithms into those from clusterCrit (main), and (others)
  cr <- consensus_rank(ii, n = n)
  top <- cr$top.list
  ii <- ii[match(top, ii$Algorithms), ]
  finalR <- finalR[, top]
  saveRDS(ii, "evaluate/ii.rds")
  saveRDS(finalR, "evaluate/all_clusts.rds")

  # Add CC and Pr summaries to Final output
  Final <- finalR %>%
    dplyr::select(top[seq_len(n)]) %>%
    cbind(apply(., 1, function(x) {
      CC <- names(which.max(table(x)))
      Pr <- table(x)[CC] / length(x)
      list(CC = as.integer(CC), Pr = Pr)
    }) %>%
      dplyr::bind_rows())
  saveRDS(Final, "evaluate/FinalR.rds")
}

# Save internal indices, and final clustering summary object
pl_evaluate(cdat, nk = 4)
