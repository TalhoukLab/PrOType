# need data_directory algs s
library(magrittr)

multMergeCM <- function(algs, fnames, newdir) {
  # Separate the algorithms
  algF <- unique(grep(algs, fnames, value = TRUE))
  MetaConsMat <- paste0(newdir, algF) %>% # Read the files
    purrr::map(readRDS) %>% # Sum the entries of the list
    purrr::transpose() %>%
    purrr::map(~ Reduce(`+`, .))
}

if (merge == "partial") {
  # list all the files in the path
  fnames <- list.files(path = paste0(data_directory, "/rds_out_", ndat)) %>%
    gtools::mixedsort()
  part <- (r * c - (c - 1)):(r * c)
  algF <- unique(grep(algs, fnames, value = TRUE))
  # Get the seeds
  temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
  seeds <- as.numeric(purrr::map_chr(temp, `[`, 1))
  part_complete<-seeds[seeds %in% part]
  cat("MergeConstmat:", part_complete, seeds, "Done")
  consmat <- paste0(data_directory, "/con_mat_", ndat, "/CM_", algs, part_complete,"_", ndat,".rds") %>%
    lapply(readRDS) %>%
    purrr::set_names(part_complete) %>%
    lapply("[[", as.character(k)) %>%
    purrr::transpose() %>%
    purrr::map(~ Reduce(`+`, .))
  ifile <- paste0(data_directory, "/con_mat_", ndat, "/", r, "_", algs, "_consmat_", ndat, ".rds")
  saveRDS(consmat, ifile)
} else {
  fnames <- list.files(path = paste0(data_directory, "/con_mat_", ndat)) %>%
    gtools::mixedsort() %>%
    grep(pattern = "^[[:digit:]]", x = ., value = TRUE)
  newdir <- paste0(data_directory, "/con_mat_", ndat, "/")
  consmatF <- lapply(algs, multMergeCM, fnames = fnames, newdir = newdir) %>%
    unlist(recursive = FALSE)
  ifile <- paste0(data_directory, "/data_pr_", ndat, "/Final_CM_", ndat, ".rds")
  saveRDS(consmatF, ifile)
}
