# Dependencies ----

# load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
})

# Functions ----

# build mapping
build_mapping <- function(train.set) {
  # label mapping
  labs <- seq_len(4)
  if (train.set == "ov.afc1_cbt") {
    data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    stop("No valid training set specified")
  }
}

#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
get_mapping <- function(dir = "data/") {
  # TCGA overlap
  tcga.mapped <- paste0(dir, "TCGA_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv() %>%
    dplyr::select(
      sampleID = TCGA,
      ottaID = `OTTA-ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # GSE overlap
  gse.mapped <- paste0(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv") %>%
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
get_nstring_overlap <- function(dir = "data/", map) {
  nanostring <- paste0(dir, "nanostring_aocs_tcga.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::inner_join(map, ., by = "ottaID") %>%
    dplyr::select(-c(sampleID, published)) %>%
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
    data.frame(nstring = nstring.pred) %>%
    dplyr::select(ottaID, nstring) %>%
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
    caret::confusionMatrix(x$published, x$array)
  )
  published_vs_nstring <- list(
    splendid::evaluation(x$published, x$nstring),
    caret::confusionMatrix(x$published, x$nstring)
  )
  array_vs_nstring <- list(
    splendid::evaluation(x$array, x$nstring),
    caret::confusionMatrix(x$array, x$nstring)
  )
  tibble::lst(published_vs_array, published_vs_nstring, array_vs_nstring)
}

#********************************************************************
# Prepare data for cross-platform to Nanostring
#********************************************************************
prep_data <- function(dataSet, dir = "./") {
  # build mapping table
  map <- build_mapping(dataSet)

  # read npcp normalized by housekeeping genes
  npcp.tmp <- paste0(dir, "data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds") %>%
    readr::read_rds()

  # process rownames
  # i.e. OV_TCGA_FEAST_TCGA_31_1946_01_CEL_gz => OV_TCGA_FEAST_TCGA_31_1946_01
  # if the above is not an issue, comment out these lines
  charDrop <- function(x) stringr::str_sub(x, 0, nchar(x) - 7)
  npcp.tmp <- npcp.tmp %>%
    data.frame(sampleID = rownames(.), .) %>%
    dplyr::mutate(sampleID = charDrop(as.character(sampleID))) %>%
    tibble::column_to_rownames("sampleID")

  # read in diceR cluster labels (hgsc subtypes)
  train.class.tmp <- paste0(dir, "data_pr_", dataSet, "/all_clusts_", dataSet, ".rds") %>%
    readr::read_rds() %>%
    magrittr::extract(, 1) %>%
    data.frame(labs = .) %>%
    dplyr::inner_join(map, by = "labs") %>%
    magrittr::use_series("labels")

  # read in required prep files
  point_to_underscore <- function(x) stringr::str_replace(as.character(x), pattern = "\\.", replacement = "_")
  plus_to_underscore <- function(x) stringr::str_replace(as.character(x), pattern = "\\+", replacement = "_")
  inclusion <- readr::read_csv(paste0(dir, "inclusion.csv")) %>%
    dplyr::mutate(Label = point_to_underscore(Label),
                  Label = plus_to_underscore(Label))
  keep <- inclusion %>%
    dplyr::filter(post == 1) %>%
    dplyr::select(Label) # This is both cut 1 and 2
  overlap <- inclusion %>%
    dplyr::filter(post == 3) %>%
    dplyr::select(Label)
  original <- read.csv(paste0(dir, "Subtype_Original.csv"), stringsAsFactors = TRUE)
  labels <- read.csv(paste0(dir, "NS_final_labels.csv"), stringsAsFactors = TRUE)
  npcp <- npcp.tmp[rownames(npcp.tmp) %in% keep$Label, ]
  train.class <- train.class.tmp[rownames(npcp.tmp) %in% keep$Label]

  df <- data.frame(lab = train.class, npcp)
  list(
    npcp = df,
    original = original,
    labels = labels
  )
}

#********************************************************************
# Load Nanostring data - all batches, aocs & tcga
#********************************************************************
load_nanostring <- function(dir, genes) {
  # format gene data for extraction from nstring
  npcp <- data.frame(t(genes))
  colnames(npcp) <- genes

  # import batch 1 nanostring
  b1 <- paste0(dir,"nanostring_classifier_data_batch1_20170217_updated.csv") %>%
    read.csv() %>%
    data.table::setattr("batch", "b1")
  b1.hk <- b1 %>%
    magrittr::extract(colnames(npcp)) %>%
    data.frame() %>%
    data.table::setattr("ottaID", b1$OTTA.ID)

  # import batch 2 nanostring
  b2 <- paste0(dir,"nanostring_classifier_data_batch2_20170221.csv") %>%
    read.csv() %>%
    data.table::setattr("batch", "b2")
  b2.hk <- b2 %>%
    magrittr::extract(colnames(npcp)) %>%
    data.frame() %>%
    data.table::setattr("ottaID", b2$OTTA.ID)

  # import batch 3 nanostring
  b3 <- paste0(dir,"nanostring_classifier_data_batch3_20170307_updated_NCO.csv") %>%
    read.csv() %>%
    data.table::setattr("batch", "b3")
  b3.hk <- b3 %>%
    magrittr::extract(colnames(npcp)) %>%
    data.frame() %>%
    data.table::setattr("ottaID", b3$OTTA.ID)

  # import batch 4 nanostring
  b4 <- paste0(dir,"nanostring_classifier_data_batch4_20170512.csv") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    data.table::setattr("batch", "b4")
  b4.hk <- b4 %>%
    magrittr::extract(colnames(npcp)) %>%
    data.frame() %>%
    data.table::setattr("ottaID", b4$OTTA.ID)

  # combine into list
  test.dat <- tibble::lst(b1, b2, b3, b4)

  # find intersecting genes
  genes <- colnames(npcp)

  # extract intersecting genes
  matched <- test.dat %>%
    purrr::map(function(x) x %>%
                 dplyr::select(c("OTTA.ID", genes)) %>%
                 dplyr::mutate(batch = as.factor(attr(., "batch")))) %>%
    dplyr::bind_rows() %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    na.omit() %>%
    data.table::setattr("batch", .$batch) %>%
    dplyr::select(-batch)
  matched
}

#********************************************************************
# Train model on prepared data
#   x.process: data.frame with genes as columns and rownames as
#              sampleIDs
#   alg: algorithm to fit to x.processed. See splendid for eligible
#        models.
#********************************************************************
train <- function(x.processed, alg) {
  splendid::classification(
    data = x.processed[, -1],
    class = x.processed[, 1],
    algorithms = alg,
    standardize = FALSE
  )
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
    class = rep(NA, nrow(nstring)),
    threshold = 0
  ) %>%
    data.table::setattr("ottaID", rownames(nstring)) %>%
    data.table::setattr("batch", attr(nstring, "batch"))
  df <- data.frame(ottaID = rownames(nstring), pred = pred)
  pred
}
