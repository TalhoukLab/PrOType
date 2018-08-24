# Fit top models on full cut1 data ----------------------------------------

# Load packages and utility functions
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
dat_path <- file.path(
  outputDir,
  paste(dat_tr, top_bcm, sep = "_"),
  paste("data_pr", dat_tr, top_bcm, sep = "_")
)

top_sl_algs <- list.files(path = dat_path,
                          pattern = "sup_lrn",
                          full.names = TRUE) %>%
  read.csv(row.names = 1) %>%
  names()

# Choose seed
seed <- readRDS(file.path("assets/data/seed.rds"))

# Import full normalized data and class labels
dat <- file.path(dat_path, paste0("npcp-hcNorm_", dat_tr, "_", top_bcm, ".rds")) %>%
  readRDS() %>%
  `rownames<-`(stringr::str_sub(rownames(.), end = -8)) %>%
  `colnames<-`(make.names(colnames(.)))
y <- file.path(dat_path, paste0("all_clusts_", dat_tr, "_", top_bcm, ".rds")) %>%
  readRDS() %>%
  dplyr::select(labs = top_cl_alg) %>%
  dplyr::inner_join(build_mapping(paste(dat_tr, top_bcm, sep = "_")), by = "labs") %>%
  dplyr::pull(labels) %>%
  make.names()

# Refit on full data
all_fits <- top_sl_algs %>%
  purrr::list_along() %>%
  purrr::set_names(top_sl_algs)
for (a in seq_along(top_sl_algs)) {
  .Random.seed <- seed
  alg <- top_sl_algs[a]
  cli::cat_line("Classifying full training data with ", alg)
  all_fits[[a]] <- splendid::classification(data = dat, class = y,
                                            algorithms = alg)
}

# Save full model fits
purrr::iwalk(all_fits,
             ~ saveRDS(.x, file.path(
               outputDir, "fits",
               paste0(dat_tr, "_", top_bcm, "_", .y, ".rds")
             )))
