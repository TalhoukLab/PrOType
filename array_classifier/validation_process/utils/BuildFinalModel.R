# Dependencies ----

suppressPackageStartupMessages({
  require(tidyverse)
  require(splendid)
  require(caret)
})


# Functions ----

import_study <- function(dir, study = "ov.afc1_cbt", hc.normalize = TRUE, is.test.set = FALSE)
  #********************************************************************
  # Import specified study with housekeeping normalization or not.
  #     is.test.set: required to specify format if test set should be
  #                  returned.
  #********************************************************************
{
  # specify npcp to use 
  if(hc.normalize)
  {
    hc <- "-hcNorm"
  } else {
    hc <- ""
  }
  
  # import the npcp and diceR labels
  dat <- readr::read_rds(paste0(dir, "data_pr_", study, "/npcp", hc, "_", study, ".rds"))
  rownames(dat) <- rownames(dat) %>% stringr::str_sub(0, nchar(.) - 7)
  labs <- readr::read_rds(paste0(dir, "data_pr_", study, "/all_clusts_", study, ".rds"))$kmodes
  
  # if test set, perform mapping
  if(!is.test.set)
  {
    mapping <- build_mapping(study)
    labs.mapped <- data.frame(labs = labs) %>%
      inner_join(., mapping, by = "labs") %>%
      select(labels)
    df <- data.frame(y = labs.mapped$labels, dat)
  } else {
    df <- data.frame(y = labs, dat)
  }
  
  return(df)
}


select_top_algos <- function(dir, study, n_top = 1)
  #********************************************************************
  # Select the top algorithms returned from supervised pipeline
  #     n_top: the number of top algos you want to return.
  #********************************************************************
{
  dat <- readr::read_csv(paste0(dir, "data_pr_", study, "/sup_lrn_", study, ".csv"))
  top_algs <- dat %>%
    tibble::column_to_rownames(., var = "X1") %>%
    .[,1:n_top] %>%
    data.table::setattr(., "alg", colnames(.))
  return(top_algs)
}

train <- function(x.processed, alg)
  #********************************************************************
  # train a given algorithm on a given training set
  #   x.processed: training set
  #   alg: algorithm of interest (see splendid docs for further details)
  #********************************************************************
{
  x <- x.processed[,-1]
  lab <- x.processed[,1]
  fit <- splendid::classification(data = x, class = lab, algorithms = alg, standardize = FALSE)
  
  return(fit)
}

