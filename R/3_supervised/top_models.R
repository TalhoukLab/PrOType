library(magrittr)
source(here::here("assets/utils.R"))

# Training data
dat_tr <- "ov.afc1"

# Choose kmodes clustering algorithm by best consensus
top_cl_alg <- "kmodes"

# Choose batch correction method by highest variance explained by clustering
top_bcm <- list.files(
  path = file.path(outputDir, "evals"),
  pattern = "pvca",
  full.names = TRUE
) %>%
  purrr::set_names(purrr::map_chr(strsplit(basename(.), split = "_"), 2)) %>%
  purrr::map_dbl(~ readRDS(.)[["dat"]][, 3]) %>%
  which.max() %>%
  names()

# Choose top 5 supervised learning algorithms by rank aggregated evaluations
top_sl_algs <- list.files(path = file.path(outputDir, "iv_summary", "ci_sup_lrn"),
                          pattern = "sup_lrn",
                          full.names = TRUE) %>%
  read.csv(row.names = 1) %>%
  names()

# Choose seed
seed <- 2018

# Import full normalized data and class labels
dat <- file.path(outDir, "genemapping", dataSet, paste0("npcp-hcNorm_", dat_tr, ".rds")) %>%
  readRDS() %>%
  `rownames<-`(stringr::str_sub(rownames(.), end = -8)) %>%
  `colnames<-`(make.names(colnames(.)))
y <- file.path(outputdir, "unsupervised", "final", paste(dat_tr, top_bcm, sep = "_"), paste0("all_clusts_", paste(dat_tr, top_bcm, sep = "_"), ".rds")) %>%
  readRDS() %>%
  dplyr::select(labs = top_cl_alg) %>%
  dplyr::inner_join(build_mapping(paste(dat_tr, top_bcm, sep = "_")), by = "labs") %>%
  dplyr::pull(labels) %>%
  make.names()

# Refit on full data
all_fits <- top_sl_algs %>%
  purrr::set_names() %>%
  purrr::map(splendid::classification, data = dat, class = y, seed_alg = seed)

# Save full model fits
purrr::iwalk(all_fits,
      ~ saveRDS(.x, file.path(
        outputDir, "fits",
        paste0(dat_tr, "_", top_bcm, "_", .y, ".rds")
      )))
