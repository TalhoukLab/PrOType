library(diceR)

#' Read in raw data, impute and save to imputed directory
pl_impute <- function(data, seed = 123) {
  fs::dir_create("imputed")
  fs::dir_ls("raw") %>%
    purrr::map(readRDS) %>%
    purrr::map(apply, 2:4, impute_knn, data = data, seed = seed) %>%
    purrr::set_names(gsub("raw", "imputed", names(.))) %>%
    purrr::iwalk(saveRDS)
}

pl_impute(ssclust)
