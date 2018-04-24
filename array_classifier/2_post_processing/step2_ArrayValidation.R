###############################################################
################## STEP 2: Array Validation ##################
###############################################################

library(here)
source(here("array_classifier/2_post_processing/utils/utils.R"))

set.seed(2017)

# import cut 1 fits
fit.c1 <- readr::read_rds("outputs/fits/all_fits.rds")

# import overlapping data
map <- get_mapping() %>%
  dplyr::filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
overlap.array <- import_array(map = map)

# predict overlap array
pred.overlap.array <- fit.c1 %>%
  purrr::imap(~ {
    purrr::imap(.x, function(z, k) {
      bc <- stringr::str_sub(.y, nchar(.y) - 2, nchar(.y))
      fname <- paste0("outputs/predictions/array_", bc, "_", k, ".rds")
      preds <- predict_overlap(z, overlap.array)
      readr::write_rds(preds, path = fname)
      return(preds)
    })
  }) %>%
  purrr::modify_depth(2, ~ get_overlap(overlap.array, ., map))
readr::write_rds(pred.overlap.array, "outputs/predictions/pred_overlap_array.rds")

# evaluate overlap results
eval.overlap <- pred.overlap.array %>%
  purrr::modify_depth(2, evaluate_array) %>%
  purrr::map(purrr::transpose) %>%
  purrr::transpose()

# create naming list structure
names.ls <- eval.overlap %>%
  purrr::map(~ {
    purrr::imap(., function(y, z) {
      study.extract <- stringr::str_sub(z, nchar(z) - 2, nchar(z))
      aname <- names(y)
      paste(study.extract, aname, sep = ".")
    }) %>%
      purrr::flatten()
  })

# name overlapping evaluation results list
evals.all <- list(eval.overlap, names.ls, names(eval.overlap)) %>%
  purrr::pmap(~ {
    named <- purrr::flatten(..1) %>% purrr::set_names(..2)
    readr::write_rds(named, paste0("outputs/evals/", ..3, ".rds"))
    return(named)
  })

# visualize evaluation results
eval.plots <- purrr::imap(evals.all, function(x, y) {
  pname <- paste0("outputs/evals/", y, ".rds")
  ptitle <- stringr::str_split(y, "_") %>%
    purrr::map(~ paste(.[1], .[2], .[3], sep = " "))
  p.ls <- plot_evals_noCBT(
    pname,
    plot.title = paste0(ptitle),
    save = TRUE,
    print = FALSE
  )
  p.ls
})
