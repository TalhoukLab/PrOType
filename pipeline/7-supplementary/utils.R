#' Confusion matrix metrics
#' @param x an object of class confusionMatrix
#' @param metrics overall or by-class metrics?
#' @param digits number of digits to round to
confmat_metrics <- function(cm, metrics = c("overall", "byclass"), digits = 2) {
  switch(
    match.arg(metrics),
    overall = {
      k <- suppressWarnings(DescTools::CohenKappa(cm[["table"]], conf.level = 0.95))
      cm %>%
        broom::tidy(by_class = FALSE) %>%
        magrittr::inset(2, 2:5, c(k, NA_real_)) %>%
        dplyr::mutate_if(is.numeric, round, digits = digits) %>%
        dplyr::transmute(
          Metric = term,
          ` ` = paste0(estimate, " (", conf.low, " - ", conf.high, ")"),
          `P value` = dplyr::case_when(
            p.value < 0.001 ~ "< 0.001",
            is.na(p.value) ~ "",
            TRUE ~ as.character(p.value)
          )
        )
    },
    byclass = {
      cm %>%
        broom::tidy(by_class = TRUE) %>%
        dplyr::filter(!is.na(class)) %>%
        tidyr::spread(key = term, value = estimate) %>%
        dplyr::select(
          Class = class,
          Sensitivity = sensitivity,
          Specificity = specificity,
          PPV = pos_pred_value,
          NPV = neg_pred_value,
          F1 = f1,
          `Detection Prevalence` = detection_prevalence,
          `Balanced Accuracy` = balanced_accuracy
        ) %>%
        dplyr::mutate_if(is.numeric, round, digits = digits)
    }
  )
}

#' Extract gene expression for CS3 samples for given sample type and site
#' @param gx gene expression data normalized to housekeeping genes
#' @param annot annotation data
#' @param site samples to filter for in `nanostring.site`
#' @param pool is `sample.type` "POOL" or not?
extract_samples <- function(gx, annot, site, pool = FALSE) {
  pool_fun <- ifelse(pool, `==`, `!=`)
  samples <- subset(
    x = annot,
    subset = pool_fun(sample.type, "POOL") & nanostring.site == site,
    select = "File.Name",
    drop = TRUE
  )
  gx[samples]
}

load_nanostring <- function(cut = "all") {
  clinical_vars <- 3:37
  # import cut 1 nanostring
  c1 <- readr::read_csv(
    file = here::here("data/nstring", "nanostring_classifier_data_batch1_20170217_updated.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut1")

  # import batch 2 nanostring
  c2 <- readr::read_csv(
    file = here::here("data/nstring", "nanostring_classifier_data_batch2_20170221.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut2")

  # import batch 3 nanostring
  c3 <- readr::read_csv(
    file = here::here("data/nstring", "nanostring_classifier_data_batch3_20170307_updated_NCO.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut3")

  # import batch 4 nanostring
  c4 <- readr::read_csv(
    file = here::here("data/nstring", "nanostring_classifier_data_batch4_20170512.csv"),
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

define_batch <- function(preds_new, nsdat, batch = "b1") {
  cli::cat_line("Selecting batch ", batch, " data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(is.na(published), agree == 1, Batch == batch) %>%
    dplyr::select(ottaID, Adaboost.xpn, published) %>%
    dplyr::mutate_at("Adaboost.xpn", dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

define_overlap <- function(preds_new, nsdat) {
  cli::cat_line("Selecting overlap data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(!is.na(published), agree == 1) %>%
    dplyr::mutate_at(c("Adaboost.xpn", "published"),
                     dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

filter_nsdat <- function(nsdat, lab) {
  nsdat %>%
    dplyr::filter(`OTTA.ID` %in% lab$ottaID) %>%
    `colnames<-`(make.names(colnames(.)))
}

check_data_order <- function(lab, dat) {
  if (!all(lab$ottaID == dat$OTTA.ID)) {
    stop("Data is in the wrong order.")
  } else {
    tibble::lst(lab, dat)
  }
}

sl_data <- function(train_dat, study = NULL, type = c("training", "test")) {
  type <- match.arg(type)
  filter_fun <- switch(type, training = `!=`, test = `==`)
  if (is.null(study)) filter_fun <- function(x, y) TRUE

  train_dat %>%
    dplyr::filter(filter_fun(site, study)) %>%
    dplyr::select(-c(site, cut)) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("OTTA.ID")
}

sl_class <- function(train_lab, data) {
  train_lab %>%
    dplyr::filter(ottaID %in% rownames(data)) %>%
    dplyr::pull(Adaboost.xpn)
}
