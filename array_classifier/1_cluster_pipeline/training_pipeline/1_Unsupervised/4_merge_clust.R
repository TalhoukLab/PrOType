library(diceR)

#' `pl_merge_clust` returns a raw merged cluster array, a knn-imputed cluster
#' array, and a completely imputed (using majority voting) cluster array all
#' saved to file.
pl_merge_clust <- function(data, nk = 4) {
  E <- pl_merge_clust_impl("raw")
  Eknn <- pl_merge_clust_impl("imputed")
  Ecomp <- impute_missing(Eknn, data, nk = nk)
  fs::dir_create("merged")
  dplyr::lst(E, Eknn, Ecomp) %>%
    purrr::set_names(paste0("merged/", names(.), ".rds")) %>%
    purrr::iwalk(saveRDS)
}

pl_merge_clust_impl <- function(path) {
  out <- fs::dir_ls(path) %>%
    purrr::map(readRDS) %>%
    split(purrr::map_chr(purrr::map(., dimnames), 3)) %>%
    purrr::map(abind::abind, along = 2) %>%
    abind::abind(along = 3)
  dimnames(out)[[2]] <- paste0("R", seq_along(dimnames(out)[[2]]))
  out
}

# Constructs the objects E, Eknn, and Ecomp from `dice` and saves to file
pl_merge_clust(cdat)
