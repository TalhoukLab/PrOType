# need dir algs s
library(magrittr)
library(Matrix)

multMergeCM <- function(algs, fnames, newdir) {
  # Separate the algorithms
  algF <- unique(grep(algs, fnames, value = TRUE))
  MetaConsMat <- paste0(newdir, algF) %>% # Read the files
    purrr::map(readRDS) %>% # Sum the entries of the list
    purrr::transpose() %>%
    purrr::map(~ Reduce(`+`, .))
}

fnames <- list.files(path = paste0(dir, "/con_mat_merged_", ndat)) %>%
  gtools::mixedsort() %>%
  grep(pattern = "^[[:digit:]]", x = ., value = TRUE)
newdir <- paste0(dir, "/con_mat_merged_", ndat, "/")
consmatF <- lapply(algs, multMergeCM, fnames = fnames, newdir = newdir) %>%
    unlist(recursive = FALSE)
ifile <- paste0(dir, "/data_pr_", ndat, "/Final_CM_", ndat, ".rds")
saveRDS(consmatF, ifile)
