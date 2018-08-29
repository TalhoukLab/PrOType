#' Prepare NanoString samples for prediction: make the column names
#' syntactically valid, coerce to data frame, move OTTA id to row names, and
#' keep only numeric columns (expression data)
prepare_samples <- function(data) {
  data %>%
    `colnames<-`(make.names(colnames(.))) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    dplyr::select_if(is.numeric)
}

#' Predict NanoString samples from final model. Combine the predicted
#' assignments with predicted probability matrix.
predict_samples <- function(mod, data) {
  predict(mod, data) %>%
    tibble::enframe(name = "ottaID", value = "predicted") %>%
    cbind(predict(mod, data, type = "prob")) %>%
    tibble::as_tibble()
}
