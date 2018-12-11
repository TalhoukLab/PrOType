#' Attach pipe for global project use
`%>%` <- magrittr::`%>%`

#' Build label mapping
#'
#' Map numeric labels to HGSC subtypes for each batch effect correction method
#'
#' @param train.set name of training set
build_mapping <- function(train.set) {
  labs <- seq_len(4)
  if (grepl("cbt", train.set)) {
    data.frame(labs, labels = c("C1-MES", "C5-PRO", "C4-DIF", "C2-IMM"))
  } else if (grepl("xpn", train.set)) {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("No valid training set specified")
  }
}

#' Load overlapping samples with their published class labels
#'
#' Combines TCGA and GSE overlapping samples. Samples with published label
#' "n/a" can be removed.
#'
#' @param dir directory for mapped TCGA and GSE overlapping samples
#' @param keep_pub logical; if `TRUE`, published samples are kept
load_overlap <- function(dir = "data", keep_pub = FALSE) {
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

  # combine & conditionally drop published cases
  dplyr::bind_rows(tcga.mapped, gse.mapped) %>%
    dplyr::filter(if (keep_pub) TRUE else mapped[["published"]] != "n/a") %>%
    dplyr::mutate(published = forcats::fct_recode(make.names(published),
                                                  NULL = "n.a"))
}

should_compute <- function(force_recompute, scriptdir, output_file) {
  target_file <- file.path(scriptdir, "file_cache.rds")
  has_changed <- FALSE
  if (file.exists(target_file)) {
    lck <- filelock::lock(target_file)
    file_cache <- readRDS(target_file)
    has_changed <- tools::md5sum(output_file) != file_cache[target_file]
    lck <- filelock::unlock(target_file)
  }
  return(force_recompute || has_changed)
}

update_cache <- function(scriptdir, output_file) {
  target_file <- file.path(scriptdir, "file_cache.rds")
  if (file.exists(target_file)) {
    lck <- filelock::lock(target_file)
    file_cache <- readRDS(target_file)
    file_cache[target_file] <- tools::md5sum(output_file)
    saveRDS(file_cache, target_file)
    lck <- filelock::unlock(target_file)
  } else {
    file_cache <- list()
    file_cache[target_file] <- tools::md5sum(output_file)
    saveRDS(file_cache, target_file)
  }
}
