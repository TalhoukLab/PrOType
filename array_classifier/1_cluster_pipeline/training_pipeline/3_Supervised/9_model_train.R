#' Train Splendid ensemble algorithm
#' 
#' Last updated on 30/10/2017 by Dustin Johnson
#'
#' Normalizes a given training set and trains the splendid ensemble
#'
#' @param dataSet is a data frame with feature columns and samples as rows
#' @param algs an algorithm supported by Splendid 
#' @reps an integer specifying the index of the bootstrap repeat
#' @inDir directory containing the data inputs
#' @outDir directory specifying the output location
#' @normalize.by how you would like to normalize the data. "None" does not normalize the data, "genes" normalizes gene-wise, "samples" normalizes sample-wise.
#' @param min.var the minimum variability at which you are willing to accept a given factor
#' @param norm.type the type of normalization performed: "conventional" or "robust". See Splendid for further details.
#' @param ... any other inputs relevent to Splendid
#' @return a list of model fits from Spendid

train_supervised <- function(dataSet, algs, reps, inDir, outDir, is.housekeeping.normalized = FALSE, normalize.by = "None", min.var = 0.5, threshold = 0, norm.type = "conventional", fname = "Model",  ...)
{
  # import dependencies
  suppressPackageStartupMessages({
    require(splendid)
    require(tidyverse)
  })
  
  # import training data
  if(!is.housekeeping.normalized)
  {
    npcp <- readr::read_rds(paste0(inDir,"/data_pr_", dataSet,"/npcp_",dataSet,".rds"))
  } else {
    npcp <- readr::read_rds(paste0(inDir, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds")) 
  }

  class <- readr::read_rds(paste0(inDir,"/data_pr_", dataSet,"/all_clusts_", dataSet,".rds"))
  class.train <- class[,1]
  
  # normalization
  if(normalize.by == "None")
  {
    data.train <- diceR::prepare_data(npcp, scale = FALSE, min.var = minVar, ...) %>% as.data.frame()
  } else if(normalize.by == "Genes") {
    data.train <- diceR::prepare_data(npcp, scale = TRUE, min.var = minVar, type = norm.type, ...) %>%
	as.data.frame()
  } else if(normalize.by == "Samples") {
    data.train <- diceR::prepare_data(t(npcp), scale = TRUE, min.var = minVar, type = norm.type, ...) %>%
      t() %>% as.data.frame()
  }
  
  # train algorithms
  reps <- as.integer(reps)
  sm <- switch(algs,
               first = splendid_model(data.train,class.train, n=1, seed = reps, 
                                      algorithms = c("lda", "rf","pam","mlr_lasso"), #xgboost
                                      rfe=FALSE, threshold=threshold),
               second = splendid_model(data.train,class.train, n=1, seed = reps, 
                                       algorithms = "svm", 
                                       rfe=FALSE, threshold=threshold),
               third = splendid_model(data.train,class.train, n=1, seed = reps, 
                                      algorithms = c("knn","adaboost"), #"nnet"
                                      rfe=FALSE, threshold=threshold),
               fourth = splendid_model(data.train,class.train, n=1, seed = reps, 
                                       algorithms = c("nbayes","mlr_ridge"), 
                                       rfe=FALSE, threshold=threshold),
               ldaRfe = splendid_model(data.train,class.train, n=1, seed = reps, 
                                       algorithms = "lda", 
                                       rfe=TRUE, threshold=threshold),
	       rfRfe = splendid_mtdel(data.train,class.train, n=1, seed = reps, 
                                      algorithms = "rf", 
                                      rfe=TRUE, threshold=threshold),
               lassoRfe = splendid_model(data.train,class.train, n=1, seed = reps, 
                                         algorithms = "mlr_lasso", 
                                         rfe=TRUE, threshold=threshold),
               svmRfe = splendid_model(data.train,class.train, n=1, seed = reps,
                                       algorithms = "svm",
                                       rfe = TRUE, threshold=threshold)
  )

  # write to file
  readr::write_rds(sm, paste0(outDir, "/", fname, "_", dataSet, "/c1_", algs, reps, "_", dataSet,".rds"))
}
