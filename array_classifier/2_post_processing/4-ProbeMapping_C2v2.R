# Code to compute cluster centroids based on DiceR and map probes to genes
# Note that if tidyverse is loaded code will not run

# TODO:// Where do we get these libraries?
library("hgug4112a.db")
library("hgu133plus2.db")
library(RColorBrewer)
library(AnnotationDbi)
library(magrittr)

# Read in  ----
# data
# TODO:// Make this not a list.. (with a different name?)
fdat <- c(trainSet)
# TODO:// make this dynamic?
ndat <- c("c2_xpn")
dat_o <- purrr::map(fdat, ~ readRDS(file.path(outputDir, ., paste0("data_pr_", .), "cdat_", ., ".rds"))})
        %>% magrittr::set_names(ndat)

# obtain predictions
clusts <- readRDS(file.path(outputDir, "predictions", paste0(ndat, ".rds")))

dat <- purrr::map2(clusts, dat_o, function(x, y) {
  C1 <- x
  d <- data.frame(C1, y)
  t <- y %>% t(.) %>% data.frame(.)
  t2 <- data.frame(rownames(y), C1)
  t$Probe <- rownames(t)
  d2 <- d %>%
    reshape2::melt(., id = "C1") %>%
    dplyr::group_by(variable, C1) %>%
    dplyr::summarise(median = median(value)) %>%
    reshape2::dcast(., variable ~ C1, median)
  return(list(full = t, centroids = d2, clusts = t2))
}) %>% purrr::transpose(.)

# Probe mapping
centroid_dat <- purrr::map(dat$centroids, function(x) {
  ID <- trimws(substring(as.character(x$variable), 2, 50))
  AFX_lab <- AnnotationDbi::select(
    hgu133plus2.db, ID,
    c("SYMBOL")
  ) %>%
    dplyr::filter(!is.na(SYMBOL)) %>%
    dplyr::filter(!duplicated(PROBEID))
  x$Gene <- AFX_lab$SYMBOL[match(ID, AFX_lab$PROBEID)]
  return(x)
})


full_dat <- purrr::map(dat$full, function(x) {
  AFX_lab <- AnnotationDbi::select(
    hgu133plus2.db, x$Probe,
    c("SYMBOL")
  ) %>%
    dplyr::filter(!is.na(SYMBOL)) %>%
    dplyr::filter(!duplicated(PROBEID))
  x$Gene <- AFX_lab$SYMBOL[match(x$Probe, AFX_lab$PROBEID)]
  return(x)
})


# Write Full Data
purrr::map2(full_dat, names(full_dat), function(x, y) {
  write.csv(x, file.path(outputDir, "evals", paste0("full_data/", y, "_all.csv")))
})

purrr::map2(dat$clusts, names(dat$clusts), function(x, y) {
  write.csv(x, file.path(outputDir, "evals", paste0("full_data/", y, "-clusters.csv")))
})

# Write Centroid Data
purrr::map2(centroid_dat, names(centroid_dat), function(x, y) {
  write.csv(x, file.path(outputDir, "evals", paste0("centroid_data_", y, ".csv")))
})
