# Current post-processing utility functions -------------------------------

# Load packages and project wide utility functions
library(purrr)
source(here::here("assets/utils.R"))


# 1 - Evaluate Batch Effects ----------------------------------------------

#' drop suffix from OTTA ids (last 7 chars)
#' @param x atomic vector coerced to character
drop_char <- function(x) {
  x <- as.character(x)
  substr(x, 0, nchar(x) - 7)
}

#' Compute PVCA source of variation
#' @param ann_mat annotation matrix: samples are rows
#' @param dat_mat data matrix: samples are columns, features are rows
#' @param batch_factors vector of factors for mixed linear model
#' @param pct_threshold percentile of minimum variability of PC to explain
source_of_var <- function(ann_mat, dat_mat, batch_factors,
                          pct_threshold = 0.6) {
  # rows of ann_mat should be the same as column of dat_mat and in same order
  if (!all(rownames(ann_mat) == colnames(dat_mat))) {
    stop("All rownames of annotation matrix do not correspond to the column names of the data matrix")
  }
  phenoData <- new("AnnotatedDataFrame", data = ann_mat)
  MASet <- Biobase::ExpressionSet(assayData = data.matrix(dat_mat),
                                  phenoData = phenoData)
  pvca::pvcaBatchAssess(MASet, batch_factors, pct_threshold)
}

#' Plot PVCA Object
#' @param pvcaObj PVCA object from `source_of_var()`
#' @param color barplot color
#' @param title plot title
pvca_plot <- function(pvcaObj, color = "blue", title = "") {
  par(oma = c(1, 0.5, 1, 1), mar = c(5.1, 7.6, 4.1, 0.1))
  # "Weighted average proportion variance"
  bp <- barplot(sort(pvcaObj$dat), horiz = TRUE, col = color, main = title,
                xlab = "", ylab = "", xlim = c(0, 1.1), border = "white",
                space = 1, las = 1)
  axis(2, at = bp, labels = pvcaObj$label[order(pvcaObj$dat)], ylab = "",
       cex.axis = 0.8, las = 2)
  values <- paste0(round(sort(pvcaObj$dat) * 100, 1), "%")
  text(sort(pvcaObj$dat), bp, labels = values, pos = 4, cex = 0.8)
}


# 2 - Internal Validity Plots ---------------------------------------------

#' Import (threshold) iv summary data
#' @param dir data directory holding iv summaries
#' @param datasets vector of datasets to extract data for
#' @param thresholds logical; if `TRUE`, will import thresholded iv summaries
import_ivs <- function(dir = "data", datasets = c("ov.afc1_xpn", "ov.afc1_cbt"),
                       threshold = TRUE) {
  # IV summary filenames (threshold)
  filenames <- datasets %>%
    purrr::map_chr(~ file.path(dir, .,
                               paste0("data_pr_", .),
                               paste0("iv_summary_", ., "_threshold.rds")))
  # IV summary filenames (no threshold)
  if (!threshold) {
    filenames <- gsub("_threshold", "", filenames)
  }
  do.call(rbind, purrr::map(filenames, readRDS))
}

#' Match combinations of batch effect and algorithm to colour palette
#' @param x character in form of "batch_effect.algorithm"
match_colour <- function(x) {
  switch(
    as.character(x),
    xpn.mlr_ridge = "#878787",
    cbt.mlr_ridge = "#c6c6c6",
    xpn.mlr_lasso = "magenta",
    cbt.mlr_lasso = "#f49ae3",
    xpn.svm = "blue",
    cbt.svm = "#6f95e8",
    xpn.pam = "purple",
    cbt.pam = "#b296d3",
    xpn.rf = "#05660e",
    cbt.rf = "#7acc81",
    xpn.adaboost = "#e66b00",
    cbt.adaboost = "#efa667",
    xpn.nbayes = "#ffbe00",
    cbt.nbayes = "#ffd866",
    xpn.knn = "brown",
    cbt.knn = "#ce9565",
    xpn.lda = "red",
    cbt.lda = "#dd9d9d"
  )
}

#' Plot evaluation measures by class and overall for all algorithms
#' across bootstrap samples (output retrieved from supervised pipeline)
#' @param dir input directory with iv summaries
#' @param datasets vector of datasets to extract iv summaries for
#' @param threshold logical; with or without threshold?
#' @param print logical; prints plots to screen
#' @param save logical; saves plots to pdf devices
#' @param col.cust vector of custom colours to overwrite defaults
#' @param width pdf figure width
#' @param height pdf figure height
all_algo_plot <- function(dir, datasets, threshold = FALSE,
                          print = TRUE, save = TRUE, col.cust = NULL,
                          width = 16, height = 9) {
  # import iv data, process for general metrics, prepare for ggplot
  df <- import_ivs(dir = dir, datasets = datasets, threshold = threshold) %>%
    dplyr::filter(normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(batch_correction = as.factor(batch_correction),
                  mod = reorder(mod, -percentile_50),
                  bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # create colour palette and plot title
  col <- col.cust %||% purrr::map_chr(levels(droplevels(df$bcm)), match_colour)
  plot.title <- ifelse(threshold,
                       "Algorithm Performance Ranking with Threshold",
                       "Algorithm Performance Ranking")

  # plot evaluation measures
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = mod,
      y = percentile_50,
      colour = bcm,
      group = bcm
    )) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::facet_wrap(~measure) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
      width = 0.4,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0.6, 1) +
    ggplot2::scale_colour_manual(values = col, name = "Batch and Model") +
    ggplot2::labs(x = "Algorithm",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = FALSE))

  # save plot
  if (save) {
    th <- ifelse(threshold, "thresh", "nothresh")
    ggplot2::ggsave(
      plot = p,
      filename = file.path(outputDir, "plots",
                           paste0("all_algos_ranked_", th, ".png")),
      width = width,
      height = height
    )
  }
  if (print) print(p)
  p
}

#' Plot evaluation measures by class and overall for top 2 algorithms
#' across bootstrap samples (output retrieved from supervised pipeline)
#' @inheritParams all_algo_plot
#' @param algs top 2 algorithms from non-thresholded iv summary
#' @param algs_t top 2 algorithms from thresholded iv summary
top2_algo_plot <- function(dir, datasets,
                           algs = c("mlr_ridge", "mlr_lasso"),
                           algs_t = c("adaboost", "rf"),
                           print = TRUE, save = TRUE, col.cust = NULL,
                           width = 16, height = 9) {
  # create mapping tables for xpn & cbt
  maps <- datasets %>%
    purrr::set_names(c("xpn", "cbt")) %>%
    purrr::map(build_mapping) %>%
    purrr::map(dplyr::transmute, labels, class = as.factor(labs))

  # import ivs data for with/without threshold
  ivs_data <- purrr::map(c(FALSE, TRUE), import_ivs,
                         dir = dir, datasets = datasets)
  all_algs <- purrr::splice(algs, algs_t)

  # process and combine xpn and cbt data for class specific measures
  iv.combine.class <- purrr::map2(
    ivs_data,
    all_algs,
    ~ dplyr::filter(.x,
                    mod %in% .y,
                    normalization == "hc",
                    stringr::str_detect(measure, "\\.")) %>%
      tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
      dplyr::mutate(class = gsub("X", "", class)) %>%
      dplyr::mutate_at(c("measure", "class"), as.factor) %>%
      dplyr::select(-class, class) %>%
      dplyr::mutate(
        labels = unlist(purrr::map2(
          class, batch_correction,
          ~ maps[[.y]]$labels[match(.x, maps[[.y]]$class)])),
        bcm = interaction(batch_correction, mod)
      )
  ) %>%
    do.call(rbind, .) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # process and combine xpn and cbt for general metrics
  iv.combine.general <- purrr::map2(
    ivs_data,
    all_algs,
    ~  dplyr::filter(.x,
                     mod %in% .y,
                     normalization == "hc",
                     measure %in% c("auc", "accuracy", "macro_f1")) %>%
      dplyr::mutate(bcm = interaction(batch_correction, mod))
  ) %>%
    do.call(rbind, .) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # create colour palette
  col <- col.cust %||% purrr::map_chr(levels(droplevels(iv.combine$bcm)),
                                      match_colour)

  # store common ggplot layers
  gglayers <- list(
    ggplot2::aes(y = percentile_50, colour = bcm, group = bcm),
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.7)),
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
      width = 0.4,
      position = ggplot2::position_dodge(width = 0.7)
    ),
    ggplot2::theme_bw(),
    ggplot2::facet_wrap(~ measure, scales = "free"),
    ggplot2::scale_colour_manual(values = col, name = "Batch and Model"),
    ggplot2::labs(
      y = "Evaluation Measure Value",
      title = "Top 2 Supervised Algorithm Evaluation (with/without threshold)"
    )
  )

  # create iv plot
  p1 <- iv.combine.class %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = labels)) +
    ggplot2::xlab("Subtype") +
    gglayers

  # plot general metrics
  p2 <- iv.combine.general %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = mod)) +
    ggplot2::xlab("Algorithm") +
    gglayers +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  # save plots
  if (save) {
    fn <- "top2_algos_eval"
    ggplot2::ggsave(
      plot = p1,
      filename = file.path(outputDir, "plots", paste0(fn, "_byclass.png")),
      width = width,
      height = height
    )
    ggplot2::ggsave(
      plot = p2,
      filename = file.path(outputDir, "plots", paste0(fn, "_overall.png")),
      width = width,
      height = height
    )
  }

  # print plots
  if (print) {
    print(p1)
    print(p2)
  }
  list(p1, p2)
}

#' Heatmap on internal validitiy indices by algorithm
#' @param dir input directory where internal validity index matrix is
#' @param datset dataset to compute heatmap for
algii_heatmap <- function(dir, dataset) {
  # Read in ii
  ii <- readRDS(file.path(dir, dataset,
                          paste0("data_pr_", dataset),
                          paste0("ii_", dataset, ".rds")))

  # Heatmap: order algorithms by ranked ii, remove indices with NaN
  hm <- ii %>%
    magrittr::set_rownames(NULL) %>%
    tibble::column_to_rownames("Algorithms") %>%
    magrittr::extract(match(diceR:::consensus_rank(ii, 5)$top.list, rownames(.)),
                      purrr::map_lgl(., ~ all(!is.nan(.x))))

  # Plot heatmap with annotated colours, column scaling, no further reordering
  png(file.path(dir, "plots", paste0(dataset, "_algii_heatmap.png")))
  NMF::aheatmap(
    hm,
    annCol = data.frame(Criteria = c(rep("Maximized", 5),
                                     rep("Minimized", ncol(hm) - 5))),
    annColors = list(Criteria = stats::setNames(c("darkgreen", "deeppink4"),
                                                c("Maximized", "Minimized"))),
    Colv = NA, Rowv = NA, scale = "column", col = "PiYG",
    main = "Ranked Algorithms on Internal Validity Indices"
  )
  dev.off()
}


# 3 - Predict C2 ----------------------------------------------------------

#' Builds mapping matrix from integer class to correct labels names for xpn
#' @param train.set can only be "ov.afc1_xpn"
build_mapping_xpn <- function(train.set) {
  if (train.set == "ov.afc1_xpn") {
    data.frame(labs = seq_len(4),
               labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("Can only relabel C1 XPN")
  }
}


# 5 - Mapping Signatures C2 -----------------------------------------------

#' Calculate entropy loss and quadratic loss of a matrix
#' @param Rh matrix to calculate entropy/quadratic loss for
#' @param R correlation matrix
matrix_loss <- function(Rh, R = NULL) {
  # Add check that it's a correlation matrix
  if (is.null(R)) {
    R <- diag(nrow(Rh))
  }
  entropy <- psych::tr(solve(R) %*% Rh) - log(det(solve(R) %*% Rh)) - nrow(Rh)
  quadratic <- psych::tr((solve(R) %*% Rh - diag(nrow(Rh)))^2)
  tibble::lst(entropy, quadratic)
}
