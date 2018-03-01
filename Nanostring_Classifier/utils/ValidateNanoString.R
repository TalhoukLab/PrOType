# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  require(randomForest)
})


# Functions ----

get_mapping <- function(dir = "data/")
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

get_nstring_overlap <- function(dir = "data/", map)
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


evaluate_nstring <- function(x)
  #********************************************************************
  # Return list of evaluation measures. Output is required for ploting.
  #********************************************************************
{
  published_vs_nstring <- list(
    splendid::evaluation(x$published, x$nstring),
    caret::confusionMatrix(x$published, x$nstring)
  )
  
  array_vs_nstring <- list(
    splendid::evaluation(x$array, x$nstring),
    caret::confusionMatrix(x$array, x$nstring)
  )
  
  return(list(
    published_vs_nstring = published_vs_nstring,
    array_vs_nstring = array_vs_nstring
  ))
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

evaluate_all <- function(x)
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
