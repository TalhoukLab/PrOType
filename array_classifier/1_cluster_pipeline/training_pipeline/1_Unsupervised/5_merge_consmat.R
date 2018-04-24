library(diceR)

#' `pl_merge_conmat` returns a list of consensus matrices.
pl_merge_conmat <- function() {
  fs::dir_create("merged")
  conmat <- fs::dir_ls("conmat") %>%
    purrr::map(readRDS) %>% {
      lapply(rapply(., enquote, how = "unlist"), eval)
    } %>%
    split(stringr::str_split_fixed(names(.), "\\.", n = 4)[, 4]) %>%
    lapply(pl_merge_conmat_impl)
  meta_conmat <- pl_merge_conmat_impl(conmat)
  dplyr::lst(conmat, meta_conmat) %>%
    purrr::set_names(paste0("merged/", names(.), ".rds")) %>%
    purrr::iwalk(saveRDS)
}

pl_merge_conmat_impl <- function(x) {
  Reduce("+", purrr::map(x, ~ . / length(x)))
}
