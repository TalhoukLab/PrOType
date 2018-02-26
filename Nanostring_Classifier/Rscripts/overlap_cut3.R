# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  require(randomForest)
})


# Functions ----

get_mapping <- function(dir = "nanostring/")
#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
{
  # TCGA overlap
  tcga.mapped <- readr::read_csv(paste0(dir, "TCGA_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(sampleID = TCGA, ottaID = `OTTA-ID`, published = `MOL-SUBTYPE-NAME (published)`))
  
  # GSE overlap
  gse.mapped <- readr::read_csv(paste0(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(sampleID = GSE9891, ottaID = `OTTA ID`, published = `MOL-SUBTYPE-NAME (published)`))
  
  # combine & drop NAs
  map <- bind_rows(tcga.mapped, gse.mapped) %>% 
    filter(published != "n/a")
  
  return(map)
}

import_array <- function(dir = "nanostring/", map)
#********************************************************************
# Import array data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
{
  # GSE
  validation.gse <- readr::read_rds(paste0(dir, "ValidationSet/validation_gse.rds")) %>%
    tibble::rownames_to_column("sampleID")
  
  # TCGA
  validation.tcga <- readr::read_rds(paste0(dir, "ValidationSet/validation_tcga.rds")) %>%
    tibble::rownames_to_column("sampleID")
  
  # combine and match with mapping table
  validation.set <- bind_rows(validation.gse, validation.tcga) %>%
    inner_join(map, ., by = "sampleID") %>%
    select(-c(ottaID, published)) %>%
    tibble::column_to_rownames("sampleID")
  
  return(validation.set)
}

get_overlap <- function(array, pred, map)
#********************************************************************
# Combine array with predictions and and join with mapping table
#********************************************************************
{
  # join the validation set
  overlap <- array %>%
    tibble::rownames_to_column("sampleID") %>%
    data.frame(., array = pred) %>%
    inner_join(., map, by = "sampleID") %>%
    select(sampleID, ottaID, published, array) %>%
    filter(published != "n/a") %>%
    mutate(published = as.factor(published))
  return(overlap)
}

get_nstring_overlap <- function(dir = "nanostring/", map)
#********************************************************************
# Import nanostring data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
{
  nanostring <- readr::read_rds(paste0(dir, "nanostring_aocs_tcga.rds")) %>%
    tibble::rownames_to_column("ottaID") %>%
    inner_join(map, ., by = "ottaID") %>%
    select(-c(sampleID, published)) %>%
    tibble::column_to_rownames("ottaID")
  return(nanostring)
}

predict_overlap <- function(fit, new.data)
#********************************************************************
# Simple predict function to take it a fit and predict on new.data
#********************************************************************
{
  pred <- splendid::prediction(mod = fit, 
                               data = new.data, 
                               class = 1:nrow(new.data), 
                               threshold = 0) %>%
    data.table::setattr(., "sampleID", rownames(new.data))
  return(pred)
}

combine <- function(mapped.dat, nstring.overlap, nstring.pred)
#********************************************************************
# Combine nanostring, its predictions and array, then join with 
# mapping table. Return table containing predicted labels on
# nanostring and array in addition to published labels.
#********************************************************************
{
  overlap <- nstring.overlap %>%
    tibble::rownames_to_column("ottaID") %>%
    data.frame(., nstring = nstring.pred) %>%
    select(ottaID, nstring) %>%
    inner_join(mapped.dat, ., by = "ottaID") %>%
    select(sampleID, ottaID, published, array, nstring)
  return(overlap)
}

evaluate_results <- function(x)
#********************************************************************
# Return list of evaluation measures. Output is required for ploting.
#********************************************************************
{
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
  
  return(list(
    published_vs_array = published_vs_array,
    published_vs_nstring = published_vs_nstring,
    array_vs_nstring = array_vs_nstring
  ))
}
