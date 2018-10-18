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
  nstring_dir <- "data/nstring"
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
  preds <- read.csv(here::here("data/nstring/predictions.csv"),
                    stringsAsFactors = FALSE)
  if (!all(nsdat$`OTTA.ID` %in% preds$ottaID)) {
    stop("Missing OTTA ID cases")
  }
  published <- read.csv(here::here("data/nstring/Subtype_Original.csv"),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(ottaID, published) %>%
    dplyr::mutate_at("published", make.names)
  preds_new <- dplyr::left_join(preds, published, by = "ottaID") %>%
    dplyr::mutate_at(c("Adaboost.xpn", "Array.Pred.Subtype",
                       "TCGA.Predicted.Subtype"), make.names)
  tibble::lst(published, preds_new)
}

# Gene Frequency
# Number of times a gene is chosen in the top 100 out of B bootstraps
gene_freq <- function(fit, alg, genes, B, ntop = 100) {
  alg_name <- match_alg(alg)
  boot_tops <- switch(
    alg_name,
    adaboost = {
      fit[[alg_name]] %>%
        purrr::map(
          maboost::varplot.maboost,
          plot.it = FALSE,
          type = "scores",
          max.var.show = ntop
        ) %>%
        purrr::map(names)
    },
    mlr_lasso = {
      fit[[alg_name]] %>%
        purrr::map(~ {
          purrr::map2(.[["glmnet.fit"]][["beta"]],
                      which(.[["glmnet.fit"]][["lambda"]] %in% .[["lambda.1se"]]),
                      ~ names(which(.x[, .y] != 0)))
        }) %>%
        purrr::map(purrr::reduce, union)
    },
    rf = {
      fit[[alg_name]] %>%
        purrr::map(~ {
          if (inherits(., "randomForest")) {
            importance <- .[["importance"]]
          } else if (inherits(., "train")) {
            importance <- .[["finalModel"]][["importance"]]
          }
          importance %>%
            magrittr::extract(order(., decreasing = TRUE), ) %>%
            head(ntop) %>%
            names()
        })
    }
  )
  boot_tops %>%
    purrr::map_dfc(~ ifelse(genes %in% ., 1 / B, 0)) %>%
    dplyr::transmute(!!dplyr::sym(paste0(alg, "Freq")) := purrr::pmap_dbl(., sum)) %>%
    data.frame(genes, .)
}

# Bootstrap frequencies for each class in lasso
lasso_freq <- function(fit, genes) {
  by_class_tops <- fit %>%
    purrr::pluck("models", "mlr_lasso") %>%
    purrr::map(~ {
      purrr::map2(.[["glmnet.fit"]][["beta"]],
                  which(.[["glmnet.fit"]][["lambda"]] %in% .[["lambda.1se"]]),
                  ~ names(which(.x[, .y] != 0)))
    })
  by_class <- by_class_tops %>%
    purrr::map(function(x)
      purrr::map_dfc(x, function(y)
        ifelse(genes %in% y, 1 / length(.), 0))) %>%
    purrr::reduce(`+`) %>%
    data.frame(genes, .)
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
