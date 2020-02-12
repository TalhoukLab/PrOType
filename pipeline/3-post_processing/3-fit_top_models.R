# Fit top models on full cut1 data ----------------------------------------

# Load utility functions
source(here::here("pipeline/3-post_processing/utils.R"))

# Choose kmodes clustering algorithm
top_cl_alg <- "kmodes"

# Training data
dat_tr <- "ov.afc1"

# Choose batch effect correction by highest variance explained by clustering
# Use XPN if pvca not precomputed because of computation time
pvca_files <- list.files(
  path = file.path(outputDir, "post_processing", "evals"),
  pattern = "pvca",
  full.names = TRUE
)
if (length(pvca_files) == 0) {
  top_bcm <- "xpn"
} else {
  top_bcm <- pvca_files %>%
    purrr::set_names(purrr::map_chr(strsplit(basename(.), split = "_"), 2)) %>%
    purrr::map_dbl(~ readRDS(.)[["dat"]][, 3]) %>%
    which.max() %>%
    names()
}

# Training data for best batch effect correction
dataset <- paste0(dat_tr, "_", top_bcm)

# Choose top 5 supervised learning algorithms by rank aggregated evaluations
top_sl_algs <- list.files(
  path = file.path(outputDir, "supervised", "top_ci"),
  pattern = dataset,
  full.names = TRUE
) %>%
  read.csv(row.names = 1) %>%
  names()

# Choose seed
seed <- readRDS(file.path(dataDir, "seed.rds"))

# Import full normalized data and class labels
dat <- file.path(outputDir, "unsupervised", "map_genes", dataset,
                 paste0("npcp-hcNorm_", dat_tr, "_", top_bcm, ".rds")) %>%
  readRDS() %>%
  `rownames<-`(stringr::str_sub(rownames(.), end = -8)) %>%
  `colnames<-`(make.names(colnames(.)))
y <- file.path(outputDir, "unsupervised", "final", dataset,
               paste0("all_clusts_", dat_tr, "_", top_bcm, ".rds")) %>%
  readRDS() %>%
  dplyr::select(labs = tidyselect::all_of(top_cl_alg)) %>%
  dplyr::inner_join(build_mapping(dataset), by = "labs") %>%
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
               outputDir, "post_processing", "fits",
               paste0(dataset, "_", .y, ".rds")
             )))
