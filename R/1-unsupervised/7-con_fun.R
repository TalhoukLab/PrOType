source(here::here("R/1-unsupervised/utils.R"))

# Read in completed E and consensus matrices
Ecomp <- readRDS(file.path(outputDir, "unsupervised", "data_pr", dataset,
                           paste0("Ecomp_", dataset, ".rds")))
CM <- readRDS(file.path(outputDir, "unsupervised", "data_pr", dataset,
                        paste0("Final_CM_", dataset, ".rds")))

# Obtain HC from each algorithm and relabel for LCE
cl.mat <- purrr::map(CM, hc, k = k) %>%
  lapply(diceR::relabel_class, ref.cl = .[[1]]) %>%
  data.frame() %>%
  as.matrix()

# Consensus Function
Consensus <- switch(
  cons.funs,
  CSPA = CM %>%
    Reduce(`+`, .) %>%
    magrittr::divide_by(length(CM)) %>%
    hc(k = 4),
  kmodes = diceR::k_modes(Ecomp),
  majority = diceR::majority_voting(Ecomp),
  LCEcts = hc(diceR::cts(cl.mat, dc = 0.8), k = k),
  LCEsrs = hc(diceR::srs(cl.mat, dc = 0.8, R = 10), k = k),
  LCEasrs = hc(diceR::asrs(cl.mat, dc = 0.8), k = k)
)

saveRDS(Consensus, file.path(outputDir, "unsupervised", "consensus", dataset,
                             paste0("cons_", cons.funs, "_", dataset, ".rds")))
