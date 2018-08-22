# Dependencies ----

# load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

source(here("assets/utils.R"))

# Functions ----

#********************************************************************
# Import nanostring data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
get_nstring_overlap <- function(dir = "data", osamples) {
  overlap_nanostring <- file.path(dir, "nanostring_aocs_tcga.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::filter(ottaID %in% osamples) %>%
    tibble::column_to_rownames("ottaID") %>%
    magrittr::extract(match(osamples, rownames(.)), )
  overlap_nanostring
}

# Join nanostring predictions with published overlapping nanostring samples
join_overlap_nstring <- function(pred, mapping) {
  pred %>%
    tibble::tibble(ottaID = rownames(attr(., "prob")), nstring = .) %>%
    dplyr::inner_join(mapping, ., by = "ottaID")
}

#********************************************************************
# Combine nanostring, its predictions and array, then join with
# mapping table. Return table containing predicted labels on
# nanostring and array in addition to published labels.
#********************************************************************
combine <- function(mapped.dat, nstring.overlap, nstring.pred) {
  mapped.dat$published <- factor(make.names(mapped.dat$published))
  overlap <- nstring.overlap %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::transmute(ottaID, nstring = nstring.pred) %>%
    dplyr::inner_join(mapped.dat, ., by = "ottaID") %>%
    dplyr::select(sampleID, ottaID, published, array, nstring)
  overlap
}

combine_pred <- function(overlap_array, overlap_nstring) {
  dplyr::inner_join(overlap_array, overlap_nstring,
                    by = c("sampleID", "ottaID", "published"))
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
  npcp <- data.frame(lab = train.class.tmp[keep], npcp.tmp[keep, ],
                     check.names = FALSE)
  tibble::lst(npcp, original, labels)
}

#********************************************************************
# Load Nanostring data - all batches, aocs & tcga
#********************************************************************
load_nanostring <- function(dir = "data", genes) {
  # format gene data for extraction from nstring
  npcp <- data.frame(t(genes))
  colnames(npcp) <- genes
  genes <- make.names(genes)

  # nanostring data filenames
  batches <- file.path(dir, c(
    "nanostring_classifier_data_batch1_20170217_updated.csv",
    "nanostring_classifier_data_batch2_20170221.csv",
    "nanostring_classifier_data_batch3_20170307_updated_NCO.csv",
    "nanostring_classifier_data_batch4_20170512.csv"
  )) %>% purrr::set_names(gsub(".*(b)atch([0-9]+).*", "\\1\\2", .))

  # import batch 1-4 nanostring and combine into list
  test.dat <- batches %>%
    purrr::imap(~ dplyr::mutate(readr::read_csv(.x, col_types = readr::cols()), batch = .y)) %>%
    purrr::map(~ magrittr::set_colnames(., make.names(colnames(.))))

  # extract intersecting genes
  matched <- test.dat %>%
    purrr::map_df(`[`, c("OTTA.ID", genes, "batch")) %>%
    as.data.frame() %>%
    na.omit() %>%
    `rownames<-`(NULL) %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    data.table::setattr("batch", .$batch) %>%
    dplyr::select(-batch)
  str(matched)
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
