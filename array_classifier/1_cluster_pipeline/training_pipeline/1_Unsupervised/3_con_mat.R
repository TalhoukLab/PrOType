library(diceR)

#' Read in imputed data and create consensus matrices, then save to conmat dir
pl_conmat <- function() {
  fs::dir_create("conmat")
  fs::dir_ls("imputed") %>%
    purrr::map(readRDS) %>%
    purrr::map(consensus_combine, element = "matrix") %>%
    purrr::set_names(gsub("imputed", "conmat", names(.))) %>%
    purrr::iwalk(saveRDS)
}

pl_conmat()
