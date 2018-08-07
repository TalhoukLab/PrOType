prepare_samples <- function(data) {
  data %>%
    `colnames<-`(make.names(colnames(.))) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    dplyr::select_if(is.numeric)
}

predict_samples <- function(mod, data) {
  predict(mod, data) %>%
    tibble::enframe(name = "ottaID", value = "predicted") %>%
    cbind(predict(mod, data, type = "prob")) %>%
    tibble::as_tibble()
}
