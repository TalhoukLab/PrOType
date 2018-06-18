###############################################################
################## STEP 2: Array Validation ##################
###############################################################

library(here)
library(magrittr)

source(here("array_classifier/2_post_processing/utils/utils.R"))

set.seed(2017)
preds_dir <- paste0(output_dir, "predictions")
evals_dir <- paste0(output_dir, "evals")

# import cut 1 fits
fit.c1 <- readr::read_rds(paste0(output_dir, "fits/all_fits.rds"))

# import overlapping data
map <- get_mapping() %>%
  dplyr::filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
overlap.array <- import_array(map = map)

# predict overlap array
pred.overlap.array <- fit.c1 %>%
  purrr::imap(~ {
    purrr::imap(.x, function(z, k) {
      bc <- stringr::str_sub(.y, nchar(.y) - 2)
      fname <- file.path(preds_dir, paste0("array_", bc, "_", k, ".rds"))
      preds <- predict_overlap(z, overlap.array)
      readr::write_rds(preds, path = fname)
      return(preds)
    })
  }) %>%
  purrr::modify_depth(2, ~ get_overlap(overlap.array, ., map))
readr::write_rds(pred.overlap.array,
                 file.path(preds_dir, "pred_overlap_array.rds"))

# evaluate overlap results
eval.overlap <- pred.overlap.array %>%
  purrr::modify_depth(2, evaluate_array) %>%
  purrr::map(purrr::transpose) %>%
  purrr::transpose()

# create naming list structure
names.ls <- eval.overlap %>%
  purrr::map(~ {
    purrr::imap(., function(y, z) {
      study.extract <- stringr::str_sub(z, nchar(z) - 2)
      paste(study.extract, names(y), sep = ".")
    }) %>%
      purrr::flatten()
  })

# name overlapping evaluation results list
evals.all <- list(eval.overlap, names.ls, names(eval.overlap)) %>%
  purrr::pmap(~ {
    named <- purrr::set_names(purrr::flatten(..1), ..2)
    readr::write_rds(named, file.path(evals_dir, paste0(..3, ".rds")))
    return(named)
  })

# visualize evaluation results
eval.plots <- names(evals.all) %>%
  purrr::map(~ plot_evals_noCBT(
    dir = file.path(evals_dir, paste0(., ".rds")),
    plot.title = gsub("_", " ", .),
    save = TRUE,
    print = FALSE
  ))
