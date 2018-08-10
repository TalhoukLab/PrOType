# Probe Mapping C2 --------------------------------------------------------

# Code to compute cluster centroids based on diceR and map probes to genes
# Load packages
library(hgug4112a.db)
library(hgu133plus2.db)

# Read in original data
fdat <- testSet
ndat <- gsub("ov.af", "", fdat)
dat_o <- readRDS(file.path(outputDir, fdat,
                           paste0("data_pr_", fdat),
                           paste0("cdat_", fdat, ".rds")))

# Obtain predictions
clusts <- readRDS(file.path(outputDir, "predictions", paste0(fdat, ".rds")))
dat <- clusts %>%
  purrr::map(~ {
    d <- data.frame(C1 = ., dat_o)
    clusts <- data.frame(Sample = rownames(dat_o), C1 = .)
    full <- data.frame(t(dat_o))
    full$Probe <- rownames(full)
    centroids <- d %>%
      reshape2::melt(id = "C1") %>%
      dplyr::group_by(variable, C1) %>%
      dplyr::summarise(median = median(value)) %>%
      reshape2::dcast(variable ~ C1, median, value.var = "median")
    tibble::lst(full, centroids, clusts)
  }) %>%
  purrr::transpose()

# Probe mapping
centroid_dat <- dat[["centroids"]] %>%
  purrr::map(~ {
    ID <- trimws(substring(as.character(.$variable), 2, 50))
    AFX_lab <- AnnotationDbi::select(hgu133plus2.db, ID, "SYMBOL") %>%
      dplyr::filter(!is.na(SYMBOL), !duplicated(PROBEID))
    .$Gene <- AFX_lab$SYMBOL[match(ID, AFX_lab$PROBEID)]
    .
  })

full_dat <- dat[["full"]] %>%
  purrr::map(~ {
    AFX_lab <- AnnotationDbi::select(hgu133plus2.db, .$Probe, "SYMBOL") %>%
      dplyr::filter(!is.na(SYMBOL), !duplicated(PROBEID))
    .$Gene <- AFX_lab$SYMBOL[match(.$Probe, AFX_lab$PROBEID)]
    .
  })

# Write Full Data (preserve row names with write.csv)
purrr::iwalk(full_dat, ~ {
  write.csv(.x, file.path(outputDir, "evals", paste0("full_data_", .y, "_all.csv")))
})

# Write Full Data clusters
purrr::iwalk(dat$clusts, ~ {
  readr::write_csv(.x, file.path(outputDir, "evals", paste0("full_data_", .y, "_clusters.csv")))
})

# Write Centroid Data
purrr::iwalk(centroid_dat, ~ {
  readr::write_csv(.x, file.path(outputDir, "evals", paste0("centroid_data_", .y, ".csv")))
})
