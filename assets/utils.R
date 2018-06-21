# build mapping
build_mapping <- function(train.set) {
  # label mapping
  labs <- seq_len(4)
  if (train.set == "ov.afc1_cbt") {
    data.frame(labs, labels = c("C1-MES", "C5-PRO", "C4-DIF", "C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("No valid training set specified")
  }
}

#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
get_mapping <- function(dir = "data") {
  # TCGA overlap
  tcga.mapped <- file.path(dir, "TCGA_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::select(
      sampleID = TCGA,
      ottaID = `OTTA-ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # GSE overlap
  gse.mapped <- file.path(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv(col_types = readr::cols()) %>%
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
