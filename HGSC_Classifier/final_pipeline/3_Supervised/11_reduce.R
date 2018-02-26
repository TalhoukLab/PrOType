#' Mapping to Nanostring
#' 
#' Last updated on 30/10/2017 by Dustin Johnson
#'
#' This script evaluates all the models that were trained on 100 bootstrap samples 
#' and saves the output in a list of data.frame with the median across all bootstrap 
#' samples for each data set. i.e. 24 indices, 11 algorithms, 100 bootstraps.
#'
#' @param dataSet is a data frame with feature columns and samples as rows
#' @param alg an algorithm supported by Splendid 
#' @param outDir directory specifying the output location
#' @return a data frame containing median and 95% confidence interval


reduce_supervised <- function(dataSet, alg, outDir, fname = "Model")
{
  # import dependencies
  suppressPackageStartupMessages({
    require(tidyverse)
  })
  
  reduceBoot <- function(dataSet, alg, outDir)
  {
    # grep files in directory matching pattern
    files.in <- paste("c1", alg, sep="_") %>% 				  
      grep(.,list.files(paste0(outDir, dataSet, "/", fname, "_", dataSet, "/")), value=TRUE)
    
    # import data into memory
    files.read <- purrr::map(paste0(outDir, dataSet, "/", fname, "_", dataSet, "/", files.in), readr::read_rds) %>% 
      purrr::transpose(.)
    
    # compute median + 95% confidence interval
    dat.out <- purrr::transpose(files.read$evals) %>% 
      purrr::map(function(x) data.frame(x) %>% apply(., 1, quantile, c(0.5, 0.05, 0.95), na.rm = TRUE))
    
    return(dat.out)
  }
  
  # Execute reduceBoot
  reduced <- reduceBoot(dataSet, alg, outDir)
  
  # write to file
  readr::write_rds(reduced, paste0(outDir, dataSet, "/", fname, "_", dataSet, "/", alg, "_train_eval_", dataSet, ".rds"))
}
