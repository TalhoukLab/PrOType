###############################################################
############# STEP 3: Overlap Validation (Cut 3) ##############
###############################################################

suppressPackageStartupMessages({
  source("Rscripts/validation_plots.R")
  source("Rscripts/overlap_cut3.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

set.seed(2017)



# import cut 1 fits
fit.c1 <- readr::read_rds("outputs/fits/all_fits.rds")

# import overlapping data
map <- get_mapping() %>% filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
overlap.array <- import_array(map = map)
overlap.nstring <- get_nstring_overlap(map = map)

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

# predict overlap nstring
pred.overlap.nstring <- purrr::map2(fit.c1, names(fit.c1), function(x, y) {
  purrr::map2(x, names(x), function(z, k) {
    # use commented lines to print results to file
    #bc <- stringr::str_sub(y, nchar(y) - 2, nchar(y))
    #fname <- paste0("outputs/predictions/nstring_overlap_", bc, "_", k, ".rds")
    #preds <- predict_overlap(z, nstring.batches)
    #readr::write_rds(preds, path = fname)
    #return(preds)
    predict_overlap(z, overlap.nstring)
  })
}) 

# combine overlapping array and nstring
overlap <- purrr::map2(pred.overlap.array, pred.overlap.nstring, function(x, y) {
  purrr::map2(x, y, function(z, k) {
    combine(z, overlap.nstring, k)
  })
})

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


