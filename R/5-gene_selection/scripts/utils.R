`%>%` <- magrittr::`%>%`

mkdir <- function(dir) {
  if (!dir.exists(dir)) {
    cli::cat_line("Directory ", shQuote(dir), " does not exist, creating now...")
    dir.create(dir, recursive = TRUE)
  }
  dir
}

get_genes <- function(data) {
  purrr::discard(colnames(data), ~ . %in% c("OTTA.ID", "site", "cut"))
}

match_alg <- function(alg) {
  switch(alg, lasso = "mlr_lasso", rf = "rf", ada = "adaboost")
}

#******************************************************************
# Load Nanostring data - all batches
# Inputs:
# the directory of the Nanostring data
#*****************************************************************
load_nanostring <- function(cut = "all") {
  clinical_vars <- 3:37
  nstring_dir <- "assets/data/nstring"
  # import cut 1 nanostring
  c1 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch1_20170217_updated.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut1")

  # import batch 2 nanostring
  c2 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch2_20170221.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut2")

  # import batch 3 nanostring
  c3 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch3_20170307_updated_NCO.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut3")

  # import batch 4 nanostring
  c4 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch4_20170512.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars, -ARL_block) %>%
    dplyr::mutate(cut = "cut4")

  # combine into list and row bind
  tibble::lst(c1, c2, c3, c4) %>%
    dplyr::bind_rows() %>%
    `names<-`(make.names(names(.)))
}

# Load published subtype data (and merged with prediction labels)
load_prediction_labels <- function(nsdat) {
  cli::cat_line("Loading Prediction Labels")
  preds <- read.csv(here::here("assets/data/nstring/predictions.csv"),
                    stringsAsFactors = FALSE)
  if (!all(nsdat$`OTTA.ID` %in% preds$ottaID)) {
    stop("Missing OTTA ID cases")
  }
  published <- read.csv(here::here("assets/data/nstring/Subtype_Original.csv"),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(ottaID, published) %>%
    dplyr::mutate_at("published", make.names)
  preds_new <- dplyr::left_join(preds, published, by = "ottaID") %>%
    dplyr::mutate_at(c("Adaboost.xpn", "Array.Pred.Subtype",
                       "TCGA.Predicted.Subtype"), make.names)
  tibble::lst(published, preds_new)
}

#******************************************************************
#Function to extract the number of times the gene is in the TOP 100
# Inputs: fit output from splendid_model, the algorithm, and the full set of genes
# output: counts from the bootstrap repeats indicating number of times a gene is in the top 100
#*****************************************************************
gene_freq <- function(fit, genes, B) {
  alg <- names(fit)
  res <- switch(
    alg,
    adaboost = {
      order.g <- fit[[alg]] %>%
        purrr::map(~ {
          .$names <- make.names(.$names)
          maboost::varplot.maboost(.,
                                   plot.it = FALSE,
                                   type = "scores",
                                   max.var.show = 454)
        })
      freq.g <- order.g %>%
        purrr::map_dfc(~ ifelse(genes %in% names(tail(., 100)), 1, 0)) %>%
        dplyr::transmute(adaFreq = purrr::pmap_dbl(., sum))
    },
    mlr_lasso = {
      order.g <- fit[[alg]] %>%
        purrr::map(~ {
          purrr::map2(.[["glmnet.fit"]][["beta"]],
                      which(.[["glmnet.fit"]][["lambda"]] %in% .[["lambda.1se"]]),
                      ~ names(which(.x[, .y] != 0)))
        }) %>%
        purrr::map(Reduce, f = union)
      freq.g <- order.g %>%
        purrr::map_dfc(~ ifelse(genes %in% ., 1, 0)) %>%
        dplyr::transmute(lassoFreq = purrr::pmap_dbl(., sum))
    },
    rf = {
      order.g <- fit[[alg]] %>%
        purrr::map(~ {
          if (inherits(., "randomForest")) {
            importance <- .[["importance"]]
          } else if (inherits(., "train")) {
            importance <- .[["finalModel"]][["importance"]]
          }
          data.frame(importance) %>%
            tibble::rownames_to_column("topVars") %>%
            dplyr::arrange(dplyr::desc(MeanDecreaseGini))
        })
      freq.g <- order.g %>%
        purrr::map_dfc(~ ifelse(genes %in% head(.[["topVars"]], 100), 1, 0)) %>%
        dplyr::transmute(rfFreq = purrr::pmap_dbl(., sum))
    })
  res / B
}

#******************************************************************
#Function to determine which genes were used in training
# Inputs: model fit and algorithm
# output: gene list
#*****************************************************************
which_genes <- function(fits, algo) {
  genes <- switch(
    algo,
    mlr_lasso = fits$glmnet.fit$beta[[1]]@Dimnames[[1]],
    rf = if (inherits(fits, "randomForest")) {
      rownames(fits$importance)
    } else if (inherits(fits, "train")) {
      rownames(fits$finalModel$importance)
    },
    adaboost = fits$names
  )
  make.names(genes)
}
