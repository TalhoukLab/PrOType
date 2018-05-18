library(diceR)
library(magrittr)

# inputs: ndat, cons.funs, k, data_directory

hc <- function(x, k, method = "average") {
  as.integer(stats::cutree(stats::hclust(stats::dist(x), method = method), k))
}

# Read in completed E and consensus matrices
Ecomp <- readRDS(file.path(data_directory, paste0("Ecomp_", ndat, ".rds")))
CM <- readRDS(file.path(data_directory, paste0("Final_CM_", ndat, ".rds")))

# Obtain HC from each algorithm and relabel for LCE
cl.mat <- purrr::map(CM, hc, k = 4) %>%
  lapply(relabel_class, ref.cl = .[[1]]) %>%
  data.frame() %>%
  as.matrix()

# Consensus Function
Consensus <- switch(
  cons.funs,
  CSPA = CM %>%
    Reduce(`+`, .) %>%
    magrittr::divide_by(length(CM)) %>%
    hc(k = 4),
  kmodes = k_modes(Ecomp),
  majority = majority_voting(Ecomp),
  LCEcts = hc(cts(cl.mat, dc = 0.8), k = 4),
  LCEsrs = hc(srs(cl.mat, dc = 0.8, R = 10), k = 4),
  LCEasrs = hc(asrs(cl.mat, dc = 0.8), k = 4)
)

saveRDS(Consensus, paste0(data_directory, "/cons_", cons.funs, "_", ndat, ".rds"))
