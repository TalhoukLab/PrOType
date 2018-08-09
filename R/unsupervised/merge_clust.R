# Merge results together to form array
# Inputs: dir, reps, algs

library(magrittr)

multMerge <- function(algs, fnames, newdir) {
  #cli::cat_line(algs, fnames, newdir, reps)
  # Separate the algorithms
  algF <- unique(grep(algs, fnames, value = TRUE))
  # Get the seeds
  cli::cat_line("Getting seeds")
  temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
  seeds <- as.numeric(purrr::map_chr(temp, `[`, 1))
  error <- 0
  cli::cat_line("Merging Seeds")
  # Combine reps that succeeded
  lalgo <- lapply(paste0(newdir, algF), readRDS) %>%
    purrr::set_names(paste0("R", seeds)) %>%
    abind::abind(along = 2)
  # Merge the seeds within algorithm when all have completed
  if (!all(seq_len(reps) %in% seeds)) {
    cli::cat_line(paste(algs, "failed:"))
    fails <- which(!(seq_len(reps) %in% seeds))
    cli::cat_line(fails)
    error <- 1

    # Create dummy array for failed reps
    fail_reps <- array(
      data = NA_real_,
      dim = replace(dim(lalgo), 2, length(fails)),
      dimnames = list(NULL, paste0("R", fails), NULL, NULL)
    )
  } else {
    fail_reps <- NULL
  }
  # Merge to make a filled array with `reps` columns
  lalgo_filled <- list(lalgo, fail_reps) %>%
    abind::abind(along = 2) %>%
    `[`(, order(colnames(.)), , 1, drop = FALSE)
  lalgo_filled
}

# Merge the raw clustering
cli::cat_line("Merging raw clustering")
regex_str <- paste0(algs, seq_len(reps), dataset, sep = "|")
rds_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("rds_out_", dataset))
imputed_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("imputed_dir_", dataset))

fnames <- list.files(path = rds_dir, pattern = regex_str) %>%
  gtools::mixedsort()

cli::cat_line("l_apply")
E <- lapply(algs, multMerge, fnames = fnames, newdir = rds_dir) %>%
  abind::abind(along = 3)
cli::cat_line("Saving RDS")
saveRDS(E, file = file.path(outputdir, "unsupervised", "merge", paste0("data_pr_", dataset), paste0("E_", dataset, ".rds")))

# Merge KNN_imputed clustering
cli::cat_line("Merging KNN imputed clustering")
fnames <- list.files(path = imputed_dir) %>%
  gtools::mixedsort()

E_knn <- lapply(algs, multMerge, fnames = fnames, newdir = imputed_dir) %>%
  abind::abind(along = 3)
saveRDS(E_knn, file = file.path(outputdir, "unsupervised", "merge", paste0("data_pr_", dataset), paste0("E_knn_", dataset, ".rds")))

# Completed clustering
cli::cat_line("Completing Clustering")
cdat<- readRDS(file.path(outputdir, "unsupervised", "prep_data", dataset, paste0("cdat_", dataset, ".rds")))
Ecomp <- diceR::impute_missing(E_knn, data = cdat, nk = k)
saveRDS(Ecomp, file = file.path(outputdir, "unsupervised", "merge", paste0("data_pr_", dataset), paste0("Ecomp_", dataset, ".rds")))
cli::cat_line("Completed Clustering")