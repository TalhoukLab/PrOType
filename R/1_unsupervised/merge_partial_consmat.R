library(magrittr)

# list all the files in the path
fnames <- list.files(path = file.path(outputdir, "unsupervised", "clustering", paste0("rds_out_", dataset))) %>%
  gtools::mixedsort()
part <- (r * c - (c - 1)):(r * c)
algF <- unique(grep(algs, fnames, value = TRUE))
# Get the seeds
temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
seeds <- as.numeric(purrr::map_chr(temp, `[`, 1))
part_complete <- seeds[seeds %in% part]

con_mat_dir <- file.path(outputdir, "unsupervised", "clustering", paste0("con_mat_", dataset))
consmat <- file.path(con_mat_dir, paste0("CM_", algs, part_complete,"_", dataset, ".rds")) %>%
  lapply(readRDS) %>%
  purrr::set_names(part_complete) %>%
  lapply("[[", as.character(k)) %>%
  purrr::transpose() %>%
  purrr::map(~ Reduce(`+`, .))

saveRDS(consmat, file.path(outputdir, "unsupervised", "merge", paste0("con_mat_merged_", dataset), paste0(r, "_", algs, "_consmat_", dataset, ".rds")))
cli::cat_line("Finished Writing to file.")
