# Supplementary utility functions -----------------------------------------

# Load gene selection utility functions
source(here::here("pipeline/5-gene_selection/utils.R"))

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
