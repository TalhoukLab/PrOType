# need dir algs s
`%>%` <- magrittr::`%>%`

multMergeCM <- function(algs, fnames, newdir) {
  # Separate the algorithms
  algF <- unique(grep(algs, fnames, value = TRUE))
  MetaConsMat <- file.path(newdir, algF) %>% # Read the files
    purrr::map(readRDS) %>% # Sum the entries of the list
    purrr::transpose() %>%
    purrr::map(~ Reduce(`+`, .))
}
con_mat_merged_dir <- file.path(outputdir, "unsupervised", "merge", paste0("con_mat_merged_", dataset))


fnames <- list.files(path = con_mat_merged_dir) %>%
  gtools::mixedsort() %>%
  grep(pattern = "^[[:digit:]]", x = ., value = TRUE)
consmatF <- lapply(algs, multMergeCM, fnames = fnames, newdir = con_mat_merged_dir) %>%
    unlist(recursive = FALSE)
saveRDS(consmatF, file.path(outputdir, "unsupervised", "merge", paste0("data_pr_", dataset), paste0("Final_CM_", dataset, ".rds")))
