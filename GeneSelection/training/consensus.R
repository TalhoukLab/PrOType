compute_consensus <- function(preds_new, nsdat, batch = "b1") {
  cli::cat_rule("Computing Consensus")
  train <- define_batch(preds_new, nsdat, batch)
  overlap <- define_overlap(preds_new, nsdat)
  tibble::lst(train, overlap)
}

define_batch <- function(preds_new, nsdat, batch) {
  cli::cat_line("Selecting batch ", batch, " data and labels")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(is.na(published), agree == 1, Batch == batch) %>%
    dplyr::select(ottaID, Adaboost.xpn, published) %>%
    dplyr::mutate_at("Adaboost.xpn", dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

define_overlap <- function(preds_new, nsdat) {
  cli::cat_line("Selecting overlap data and labels")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(!is.na(published), agree == 1) %>%
    dplyr::mutate_at(c("Adaboost.xpn", "TCGA.Predicted.Subtype","published"),
                     dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

filter_nsdat <- function(nsdat, lab) {
  nsdat %>%
    dplyr::filter(`OTTA ID` %in% lab$ottaID) %>%
    `colnames<-`(make.names(colnames(.)))
}

check_data_order <- function(lab, dat) {
  if (!all(lab$ottaID == dat$OTTA.ID)) {
    stop("Data is in the wrong order.")
  } else {
    tibble::lst(lab, dat)
  }
}
