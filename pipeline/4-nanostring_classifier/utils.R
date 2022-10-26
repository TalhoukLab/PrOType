# NanoString utility functions -------------------------------

# Load packages and project wide utility functions
source(here::here("assets/utils.R"))


# 1 - Validate Nanostring -------------------------------------------------

#' Import nanostring validation data and filter for overlapping samples
#'
#' Uses "nanostring_aocs_tcga" to filter for overlapping samples.
#'
#' @param dir directory with "nanostring_aocs_tcga" data
#' @param osamples character vector of overlapping samples. Given as "ottaID"
#'   column from `load_overlap()`
import_nstring_overlap <- function(dir = "data", osamples) {
  overlap_nanostring <- file.path(dir, "nanostring_aocs_tcga.rds") %>%
    readRDS() %>%
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
    dplyr::inner_join(overlap, ., by = "ottaID") %>%
    dplyr::mutate(nstring = `attributes<-`(nstring, attributes(pred)))
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
  is_pub <- !is.na(p)

  pp <- p[is_pub]
  ap <- a[is_pub]
  attributes(ap) <- attributes(a)
  attr(ap, "prob") <- attr(ap, "prob")[is_pub, ]
  attr(ap, "class.thres") <- attr(ap, "class.thres")[is_pub]

  np <- n[is_pub]
  attributes(np) <- attributes(n)
  attr(np, "prob") <- attr(np, "prob")[is_pub, ]
  attr(np, "class.thres") <- attr(np, "class.thres")[is_pub]

  list(
    published_vs_array = list(
      metrics = splendid::evaluation(pp, ap),
      confmat = caret::confusionMatrix(ap, pp)
    ),
    published_vs_nstring = list(
      metrics = splendid::evaluation(pp, np),
      confmat = caret::confusionMatrix(np, pp)
    ),
    array_vs_nstring = list(
      metrics = splendid::evaluation(a, n),
      confmat = caret::confusionMatrix(n, a)
    )
  )
}

#' Evaluate consensus and published
#'
#' Evaluate array and nanostring consensus labels with published labels
#'
#' @param data tibble of published labels, predicted overlap array labels, and
#'   predicted overlap nanostring labels. Uses output from `combine_pred()`.
evaluate_consensus <- function(data) {
  data <- data %>%
    dplyr::filter(array == nstring) %>%
    dplyr::transmute(published, consensus = array)
  p <- data[["published"]]
  c <- data[["consensus"]]
  list(
    published_vs_consensus = list(
      confmat = caret::confusionMatrix(c, p)
    )
  )
}


# 2 - Predict All Nanostring ----------------------------------------------

#' Load all nanostring data
#'
#' Load nanostring data from all batches and extract intersecting genes
#' from model
#'
#' @param dir directory where all batches of nanostring data are
#' @param genes genes used in the top model chosen
load_nstring_all <- function(dir = "data", genes) {
  # format gene data for extraction from nstring
  genes <- make.names(genes)

  # nanostring data filenames
  batches <- file.path(dir, c(
    "nanostring_classifier_data_batch1_20170217_updated.csv",
    "nanostring_classifier_data_batch2_20170221.csv",
    "nanostring_classifier_data_batch3_20170307_updated_NCO.csv",
    "nanostring_classifier_data_batch4_20170512.csv"
  ))

  # import batch 1-4 nanostring and combine into list
  nstring_list <- batches %>%
    purrr::set_names(gsub(".*(b)atch([0-9]+).*", "\\1\\2", .)) %>%
    purrr::imap(
      ~ .x %>%
        readr::read_csv(col_types = readr::cols()) %>%
        dplyr::mutate(batch = .y) %>%
        magrittr::set_colnames(make.names(colnames(.)))
    )

  # extract intersecting genes and coerce to data frame; store batch as attr
  nstring_df <- nstring_list %>%
    purrr::map_df(`[`, c("OTTA.ID", genes, "batch")) %>%
    as.data.frame() %>%
    na.omit() %>%
    magrittr::set_rownames(NULL) %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    `attr<-`("batch", .[["batch"]]) %>%
    dplyr::select(-batch)
  nstring_df
}
