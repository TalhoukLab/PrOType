define_batch <- function(preds_new, nsdat, var = "Adaboost.xpn", batch = "b1") {
  cli::cat_line("Selecting batch ", batch, " data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(.data[[var]] == .data[["TCGA.Predicted.Subtype"]], 1, 0)) %>%
    # dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(is.na(published), agree == 1, Batch == batch) %>%
    dplyr::select(ottaID, var, published) %>%
    dplyr::mutate_at(var, dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

define_overlap <- function(preds_new, nsdat, var = "Adaboost.xpn") {
  cli::cat_line("Selecting overlap data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(.data[[var]] == .data[["TCGA.Predicted.Subtype"]], 1, 0)) %>%
    # dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(!is.na(published), agree == 1) %>%
    dplyr::mutate_at(c(var, "published"),
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
