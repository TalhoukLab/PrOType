# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  source("utils/build_mapping.R")
})

# Functions ----

prep_data <- function(dataSet, dir = "./")
  #********************************************************************
  # Prepare data for cross-platform to Nanostring
  #********************************************************************
{
  # build mapping table
  map <- build_mapping(dataSet)
  
  # read npcp normalized by housekeeping genes
  npcp.tmp <- readr::read_rds(paste0(dir, "data_pr_", dataSet, "/npcp-hcNorm_",dataSet,".rds"))
  
  # process rownames
  # i.e. OV_TCGA_FEAST_TCGA_31_1946_01_CEL_gz => OV_TCGA_FEAST_TCGA_31_1946_01
  # if the above is not an issue, comment out these lines
  charDrop <- function(x) stringr::str_sub(x, 0, nchar(x) - 7)
  npcp.tmp <- npcp.tmp %>% data.frame(sampleID = rownames(.), .) %>%
    mutate(sampleID = charDrop(as.character(sampleID))) %>%
    tibble::column_to_rownames(., "sampleID")
  
  # read in diceR cluster labels (hgsc subtypes)
  train.class.tmp <- readr::read_rds(paste0(dir, "data_pr_", dataSet, "/all_clusts_",dataSet,".rds"))[,1] %>% 
    data.frame(labs = .) %>%
    dplyr::inner_join(map, by = "labs") %>% .$labels
  
  # read in required prep files
  point_to_underscore <- function(x) stringr::str_replace(as.character(x), pattern = "\\.", replacement = "_")
  plus_to_underscore <- function(x) stringr::str_replace(as.character(x), pattern = "\\+", replacement = "_")
  inclusion <- readr::read_csv(paste0(dir, "inclusion.csv")) %>% 
    mutate(Label = point_to_underscore(Label)) %>%
    mutate(Label = plus_to_underscore(Label))
  keep <- inclusion %>% filter(post==1) %>% select(Label) # This is both cut 1 and 2
  overlap <- inclusion %>% filter(post==3) %>% select(Label) 
  original <- read.csv(paste0(dir, "Subtype_Original.csv"), stringsAsFactors = TRUE) 
  labels <- read.csv(paste0(dir, "NS_final_labels.csv"), stringsAsFactors = TRUE)
  npcp <- npcp.tmp[rownames(npcp.tmp) %in% keep$Label,]
  train.class <- train.class.tmp[rownames(npcp.tmp) %in% keep$Label]
  
  df <- data.frame(lab = train.class, npcp)
  
  return(list(
    npcp = df,
    original = original,
    labels = labels
  ))
}

load_nanostring <- function(dir, npcp)
  #********************************************************************
  # Load Nanostring data - all batches, aocs & tcga
  #********************************************************************
{
  # import batch 1 nanostring
  b1 <- read.csv(paste0(dir,"nanostring_classifier_data_batch1_20170217_updated.csv")) %>%
    data.table::setattr(., 'batch', "b1")
  b1.hk <- b1 %>% 
    magrittr::extract(colnames(npcp)) %>% 
    data.frame(.)
  data.table::setattr(b1.hk, 'ottaID', b1$OTTA.ID)
  
  # import batch 2 nanostring
  b2 <- read.csv(paste0(dir,"nanostring_classifier_data_batch2_20170221.csv")) %>%
    data.table::setattr(., 'batch', "b2")
  b2.hk <- b2 %>% 
    magrittr::extract(colnames(npcp)) %>% 
    data.frame(.)
  data.table::setattr(b2.hk, 'ottaID', b2$OTTA.ID)
  
  # import batch 3 nanostring
  b3 <- read.csv(paste0(dir,"nanostring_classifier_data_batch3_20170307_updated_NCO.csv")) %>%
    data.table::setattr(., 'batch', "b3")
  b3.hk <- b3 %>% 
    magrittr::extract(colnames(npcp)) %>% 
    data.frame(.)
  data.table::setattr(b3.hk, 'ottaID', b3$OTTA.ID)
  
  # import batch 4 nanostring
  b4 <- read.csv(paste0(dir,"nanostring_classifier_data_batch4_20170512.csv"), header = TRUE, stringsAsFactors = FALSE) %>%
    data.table::setattr(., 'batch', "b4") 
  b4.hk <- b4 %>% 
    magrittr::extract(colnames(npcp)) %>% 
    data.frame(.)
  data.table::setattr(b4.hk, 'ottaID', b4$OTTA.ID)
  
  # combine into list
  test.dat <- list(
    b1 = b1,
    b2 = b2,
    b3 = b3,
    b4 = b4
  )
  
  # find intersecting genes
  genes <- colnames(npcp)
  
  # extract intersecting genes
  matched <- purrr::map(test.dat, function(x) x %>% 
                          select(c("OTTA.ID", genes)) %>%
                          mutate(batch = as.factor(attr(.,"batch")))) %>%
    bind_rows() %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    na.omit() %>%
    data.table::setattr(., "batch", .$batch) %>%
    select(-batch)
  
  return(matched)
}

train <- function(x.processed, alg)
  #********************************************************************
  # Train model on prepared data
  #   x.process: data.frame with genes as columns and rownames as
  #              sampleIDs
  #   alg: algorithm to fit to x.processed. See splendid for eligible
  #        models.
  #********************************************************************
{
  x <- x.processed[,-1]
  lab <- x.processed[,1]
  fit <- splendid::classification(data = x, class = lab, algorithms = alg, standardize = FALSE)
  return(fit)
}

predict_nstring <- function(fit, nstring)
  #********************************************************************
  # Predict fit on nanostring data
  #   fit: model fit returned from train()
  #   nstring: nanostring data on which to predict
  #********************************************************************
{
  pred <- splendid::prediction(mod = fit, data = nstring, class = rep(NA, nrow(nstring)), threshold = 0) %>%
    data.table::setattr(., name = "ottaID", value = rownames(nstring)) %>%
    data.table::setattr(., "batch", attr(nstring, "batch"))
  df <- data.frame(ottaID = rownames(nstring), pred = pred)
  return(pred)
}

