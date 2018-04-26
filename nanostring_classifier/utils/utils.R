# Dependencies ----

# load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

source(here("assets/utils.R"))

# Functions ----

#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
get_mapping <- function(dir = "data") {
  # TCGA overlap
  tcga.mapped <- file.path(dir, "TCGA_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv() %>%
    dplyr::select(
      sampleID = TCGA,
      ottaID = `OTTA-ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # GSE overlap
  gse.mapped <- file.path(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv() %>%
    dplyr::select(
      sampleID = GSE9891,
      ottaID = `OTTA ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # combine & drop NAs
  dplyr::bind_rows(tcga.mapped, gse.mapped) %>%
    dplyr::filter(published != "n/a")
}

#********************************************************************
# Import nanostring data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
get_nstring_overlap <- function(dir = "data", map) {
  nanostring <- file.path(dir, "nanostring_aocs_tcga.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::inner_join(map, ., by = "ottaID") %>%
    dplyr::select(-c(sampleID, published)) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("ottaID")
  nanostring
}

#********************************************************************
# Simple predict function to take it a fit and predict on new.data
#********************************************************************
predict_overlap <- function(fit, new.data) {
  splendid::prediction(
    mod = fit,
    data = new.data,
    class = seq_len(nrow(new.data))
  ) %>%
    data.table::setattr("sampleID", rownames(new.data))
}

#********************************************************************
# Combine nanostring, its predictions and array, then join with
# mapping table. Return table containing predicted labels on
# nanostring and array in addition to published labels.
#********************************************************************
combine <- function(mapped.dat, nstring.overlap, nstring.pred) {
  overlap <- nstring.overlap %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::transmute(ottaID, nstring = nstring.pred) %>%
    dplyr::inner_join(mapped.dat, ., by = "ottaID") %>%
    dplyr::select(sampleID, ottaID, published, array, nstring)
  overlap
}

#********************************************************************
# Return list of evaluation measures. Output is required for ploting.
#********************************************************************
evaluate_all <- function(x) {
  published_vs_array <- list(
    splendid::evaluation(x$published, x$array),
    caret::confusionMatrix(x$array, x$published)
  )
  published_vs_nstring <- list(
    splendid::evaluation(x$published, x$nstring),
    caret::confusionMatrix(x$nstring, x$published)
  )
  array_vs_nstring <- list(
    splendid::evaluation(x$array, x$nstring),
    caret::confusionMatrix(x$nstring, x$array)
  )
  tibble::lst(published_vs_array, published_vs_nstring, array_vs_nstring)
}

#********************************************************************
# Prepare data for cross-platform to Nanostring
#********************************************************************
prep_data <- function(dataSet, dir = "data") {
  subdir <- paste0("data_pr_", dataSet) # subdirectory

  # read npcp normalized by housekeeping genes
  npcp.tmp <- file.path(dir, subdir, paste0("npcp-hcNorm_", dataSet, ".rds")) %>%
    readr::read_rds() %>%
    `rownames<-`(stringr::str_sub(rownames(.), end = -8)) # process rownames

  # read in diceR cluster labels (hgsc subtypes)
  train.class.tmp <- file.path(dir, subdir, paste0("all_clusts_", dataSet, ".rds")) %>%
    readr::read_rds() %>%
    dplyr::select(labs = 1) %>%
    dplyr::inner_join(build_mapping(dataSet), by = "labs") %>%
    dplyr::pull(labels)

  # read in required prep files
  inclusion <- readr::read_csv(file.path(dir, "inclusion.csv"))
  original <- read.csv(file.path(dir, "Subtype_Original.csv"))
  labels <- read.csv(file.path(dir, "NS_final_labels.csv"))

  # only keep post == 1
  keep <- inclusion %>%
    dplyr::mutate(Label = gsub("\\.|\\+", "_", Label)) %>%
    dplyr::filter(post == 1) %>%
    dplyr::pull(Label) %>%  # This is both cut 1 and 2
    magrittr::is_in(rownames(npcp.tmp), .)
  npcp <- data.frame(lab = train.class.tmp[keep], npcp.tmp[keep, ])
  tibble::lst(npcp, original, labels)
}

#********************************************************************
# Load Nanostring data - all batches, aocs & tcga
#********************************************************************
load_nanostring <- function(dir = "data", genes) {
  # format gene data for extraction from nstring
  npcp <- data.frame(t(genes))
  colnames(npcp) <- genes

  # import batch 1 nanostring
  b1 <- file.path(dir, "nanostring_classifier_data_batch1_20170217_updated.csv") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(batch = "b1")

  # import batch 2 nanostring
  b2 <- file.path(dir, "nanostring_classifier_data_batch2_20170221.csv") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(batch = "b2")

  # import batch 3 nanostring
  b3 <- file.path(dir, "nanostring_classifier_data_batch3_20170307_updated_NCO.csv") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(batch = "b3")

  # import batch 4 nanostring
  b4 <- file.path(dir, "nanostring_classifier_data_batch4_20170512.csv") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(batch = "b4")

  # combine into list
  test.dat <- tibble::lst(b1, b2, b3, b4)

  # extract intersecting genes
  matched <- test.dat %>%
    purrr::map_df(`[`, c("OTTA.ID", genes, "batch")) %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    na.omit() %>%
    data.table::setattr("batch", .$batch) %>%
    dplyr::select(-batch)
  matched
}

#********************************************************************
# Predict fit on nanostring data
#   fit: model fit returned from train()
#   nstring: nanostring data on which to predict
#********************************************************************
predict_nstring <- function(fit, nstring) {
  pred <- splendid::prediction(
    mod = fit,
    data = nstring,
    class = rep(NA, nrow(nstring))
  ) %>%
    data.table::setattr("ottaID", rownames(nstring)) %>%
    data.table::setattr("batch", attr(nstring, "batch"))
  pred
}
