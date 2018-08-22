# Current NanoString utility functions -------------------------------

# Load packages and project wide utility functions
library(purrr)
source(here::here("assets/utils.R"))

#' Import nanostring validation data and filter for overlapping samples
#'
#' Uses "nanostring_aocs_tcga" to filter for overlapping samples.
#'
#' @param dir directory with "nanostring_aocs_tcga" data
#' @param osamples character vector of overlapping samples. Given as "ottaID"
#'   column from `load_overlap()`
import_nstring_overlap <- function(dir = "data", osamples) {
  overlap_nanostring <- file.path(dir, "nanostring_aocs_tcga.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::filter(ottaID %in% osamples) %>%
    tibble::column_to_rownames("ottaID") %>%
    magrittr::extract(match(osamples, rownames(.)), )
  overlap_nanostring
}

#' Join published labels with overlap nanostring predictions
#'
#' @param pred predicted overlap nanostring labels
#' @param overlap tibble of overlap sampleID, ottaID, and published labels
join_published_nstring <- function(pred, overlap) {
  pred %>%
    tibble::tibble(ottaID = rownames(attr(., "prob")), nstring = .) %>%
    dplyr::inner_join(overlap, ., by = "ottaID")
}

#' Combine overlap predictions for array and nanostring with published
#'
#' @param overlap_array tibble of predicted overlap array labels with published
#'   labels
#' @param overlap_nstring tibble of predicted overlap nanostring labels with
#'   published labels
combine_pred <- function(overlap_array, overlap_nstring) {
  dplyr::inner_join(overlap_array, overlap_nstring,
                    by = c("sampleID", "ottaID", "published"))
}

#' Evaluate all combinations of predicted labels
#'
#' Evaluate published vs overlap array labels, published vs nanostring array
#' labels, and overlap array vs overlap nanostring labels
#'
#' @param data tibble of published labels, predicted overlap array labels, and
#'   predicted overlap nanostring labels. Uses output from `combine_pred()`.
evaluate_all <- function(data) {
  p <- data[["published"]]
  a <- data[["array"]]
  n <- data[["nstring"]]
  list(
    published_vs_array = list(
      metrics = splendid::evaluation(p, a),
      confmat = caret::confusionMatrix(a, p)
    ),
    published_vs_nstring = list(
      metrics = splendid::evaluation(p, n),
      confmat = caret::confusionMatrix(n, p)
    ),
    array_vs_nstring = list(
      metrics = splendid::evaluation(a, n),
      confmat = caret::confusionMatrix(n, a)
    )
  )
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
