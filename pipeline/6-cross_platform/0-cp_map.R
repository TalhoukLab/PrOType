# Load utility functions
source(here::here("pipeline/5-gene_selection/utils.R"))
nstring_path <- "data/nstring"
array_path <- "data/array/OverlapSet"

# Load data
# Load prediction labels and find overlapping samples where there is agreement
preds <-
  read.csv(file.path(nstring_path, "predictions.csv"),
           stringsAsFactors = FALSE) %>%
  dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0))

# Read in the mapping to the array
# TCGA overlap
tcga.mapped <-
  readr::read_csv(file.path(nstring_path, "TCGA_sampleIDs_OTTA-Mapped.csv"),
                  col_types = readr::cols()) %>%
  dplyr::select(c(
    sampleID = TCGA,
    ottaID = `OTTA-ID`,
    published = `MOL-SUBTYPE-NAME (published)`
  ))

# GSE overlap
gse.mapped <-
  readr::read_csv(file.path(nstring_path, "GSE9891_sampleIDs_OTTA-Mapped.csv"),
                  col_types = readr::cols()) %>%
  dplyr::select(c(
    sampleID = GSE9891,
    ottaID = `OTTA ID`,
    published = `MOL-SUBTYPE-NAME (published)`
  ))

# Asked Susan why this case is missing~
# overlap$ottaID[!(overlap$ottaID %in% preds$ottaID)]

# Combine & drop NAs, merge with predictions
overlap_lab <- dplyr::bind_rows(tcga.mapped, gse.mapped) %>%
  dplyr::inner_join(preds, by = "ottaID")

# get the array data
overlap_array_dat <-
  rbind(
    readRDS(file.path(array_path, "validation_gse.rds")),
    readRDS(file.path(array_path, "validation_tcga.rds"))
  ) %>%
  dplyr::rename(PD.L1 = PD.1) %>%
  tibble::rownames_to_column("sampleID") %>%
  dplyr::inner_join(overlap_lab, ., by = "sampleID") %>%
  dplyr::arrange(ottaID)

# get the nstring data they are only in b1 and b2
overlap_nstring_dat <- rbind(
  read.csv(file.path(
    nstring_path, "nanostring_classifier_data_batch1_20170217_updated.csv")),
  read.csv(file.path(
    nstring_path, "nanostring_classifier_data_batch2_20170221.csv"))
) %>%
  dplyr::rename(ottaID = OTTA.ID) %>%
  dplyr::mutate(ottaID = as.character(ottaID)) %>%
  dplyr::inner_join(overlap_lab, ., by = "ottaID") %>%
  dplyr::arrange(ottaID)

if (!all(overlap_nstring_dat$ottaID == overlap_array_dat$ottaID)) {
  stop("Overlap data orrder doesn't match")
}
