###############################################################
############# STEP 3: Overlap Validation (Cut 3) ##############
###############################################################

suppressPackageStartupMessages({
  source("Rscripts/NanostringEvals.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

# import cut 1 fits
fit.c1 <- readr::read_rds("outputs/fits/all_fits.rds")

# consider overlapping cases
mapping <- get_mapping() 

# import validation array data
array <- import_array(mapping = mapping)

# predict overlap array
pred.overlap.array <- purrr::map2(fit.c1, names(fit.c1), function(x, y) {
  purrr::map2(x, names(x), function(z, k) {
    # use commented lines to print results to file
    #bc <- stringr::str_sub(y, nchar(y) - 2, nchar(y))
    #fname <- paste0("outputs/predictions/array_", bc, "_", k, ".rds")
    #preds <- predict_overlap(z, nstring.batches)
    #readr::write_rds(preds, path = fname)
    #return(preds)
    predict_overlap(z, overlap.array)
  })
}) %>% purrr::modify_depth(., 2, function(x) {
  get_overlap(overlap.array, x, map)
})

preds <- predict(fit.c1$ridge_xpn,newdata = array$all.array, type="class")

caret::confusionMatrix(preds,factor(mapping$overall$published))

 predict(fit.c1$adaboost_xpn,newdata = array$aocs.array, type="class") %>% 
   list(caret::confusionMatrix(factor(mapping$aocs$published), .),
        splendid::evaluation(factor(mapping$overall$published),.))


# evaluate overlap results
eval.overlap <- purrr::modify_depth(overlap, 2, function(x) {
  evaluate_results(x)
}) %>% purrr::map(., function(x) {
  purrr::transpose(x)
}) %>% purrr::transpose(.)


names.ls <- purrr::map(eval.overlap, function(x) {
  sname <- names(x)
  purrr::map2(x, sname, function(y, z) {
    study.extract <- stringr::str_sub(z, nchar(z) - 2, nchar(z))
    aname <- names(y)
    paste(study.extract, aname, sep = ".")
  }) %>% purrr::flatten()
})

evals.all <- purrr::pmap(list(
  eval.overlap, names.ls, names(eval.overlap)
), function(x, y, z) {
  named <- purrr::flatten(x) %>% purrr::set_names(y)
  readr::write_rds(named, paste0("outputs/evals/", z, ".rds"))
  return(named)
})


# visualize evaluation results
eval.plots <- purrr::map2(evals.all, names(evals.all), function(x, y) {
  pname <- paste0("outputs/evals/", y, ".rds")
  ptitle <- stringr::str_split(y, "_") %>% 
    purrr::map(., ~paste(.[1], .[2], .[3], sep = " "))
  p.ls <- plot_evals_noCBT(
    pname, plot.title = paste0(ptitle), 
    save = TRUE, print = FALSE
    )
  return(p.ls)
})


