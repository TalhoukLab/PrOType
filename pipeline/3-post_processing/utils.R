# Post-processing utility functions -------------------------------

# Load packages and project wide utility functions
`%||%` <- purrr::`%||%`
source(here::here("assets/utils.R"))


# 0 - Compare Reference Outputs -------------------------------------------

#' Compare two objects and show differences if different
#'
#' Objects are compared using `all.equal`
#'
#' @param path1 path to object 1
#' @param path2 path to object 2
#' @param df_set name of objects to compare
#' @param dataset name of dataset
check_dataframes <- function(path1, path2, df_set, dataset) {
  if (file.exists(path1) && file.exists(path2)) {
    a1 <- readRDS(path1)
    a2 <- readRDS(path2)

    validation <- all.equal(a1, a2)
    if (isTRUE(validation)) {
      cli::cat_line(df_set, " for dataset: ", dataset, " identical")
    } else {
      cli::cat_line(df_set, " for dataset: ", dataset, " DIFFERENT")
      cli::cat_line(validation)
    }
  } else if (!file.exists(path1)) {
    cli::cat_line("Can't check dataset: ", dataset, " missing reference ", df_set)
  } else if (!file.exists(path2)) {
    cli::cat_line("Can't check dataset: ", dataset, " missing computed ii ", df_set)
  }
}


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
  suppressWarnings(pvca::pvcaBatchAssess(MASet, batch_factors, pct_threshold))
}

#' Plot PVCA Object
#' @param pvcaObj PVCA object from `source_of_var()`
#' @param color barplot color
#' @param title plot title
pvca_plot <- function(pvcaObj, color = "blue", title = "") {
  pdat <- pvcaObj %>%
    rbind.data.frame() %>%
    dplyr::arrange(dat) %>%
    dplyr::mutate(
      label = forcats::fct_inorder(stringr::str_replace_all(
        string = label,
        pattern = c("clust:CohortLabel" = "Cluster/Cohort Interaction",
                    "CohortLabel" = "Cohort",
                    "clust" = "Cluster",
                    "resid" = "Residual")
      )),
      values = paste0(round(dat * 100, 1), "%")
    )
  ggplot(pdat, aes(x = label, y = dat)) +
    geom_col(fill = color) +
    geom_text(aes(label = values), hjust = -0.5) +
    labs(x = "Source of Batch Effect",
         y = "Weighted Average Proportion Variance",
         title = title) +
    lims(y = c(0, 1.1)) +
    coord_flip() +
    theme_bw()
}

#' Fast PCA for only the first n PC's
#'
#' More efficient PCA if only the first few PC's are needed (e.g. for plotting)
#' and there are many columns in `x`. Faster implementation uses `La.svd()`.
#'
#' @param x data matrix
#' @param n the first `n` principal components are returned
#' @param center logical; should `data` be centered
#' @param scale logical; should `data` be scaled
prcomp_n <- function(x, n = 3, center = TRUE, scale = FALSE) {
  x %>%
    scale(center = center, scale = scale) %>%
    magrittr::multiply_by_matrix(t(La.svd(., nu = 0, nv = n)$vt)) %>%
    magrittr::set_colnames(paste0("PC", seq_len(n)))
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
    purrr::map_chr(~ file.path(dir, "supervised", "summary", .,
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
      filename = file.path(outputDir, "post_processing", "plots",
                           paste0("all_algos_ranked_", th, ".pdf")),
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
  col <- col.cust %||% purrr::map_chr(levels(droplevels(iv.combine.general$bcm)),
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
      filename = file.path(outputDir, "post_processing", "plots",
                           paste0(fn, "_byclass.pdf")),
      width = width,
      height = height
    )
    ggplot2::ggsave(
      plot = p2,
      filename = file.path(outputDir, "post_processing", "plots",
                           paste0(fn, "_overall.pdf")),
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
  ii <- readRDS(file.path(dir, "unsupervised", "final", dataset,
                          paste0("ii_", dataset, ".rds")))

  # Heatmap: order algorithms by ranked ii, remove indices with NaN
  hm <- ii %>%
    magrittr::set_rownames(NULL) %>%
    tibble::column_to_rownames("Algorithms") %>%
    magrittr::extract(match(diceR:::consensus_rank(ii, 5)$top.list, rownames(.)),
                      purrr::map_lgl(., ~ all(!is.nan(.x))))

  # Plot heatmap with annotated colours, column scaling, no further reordering
  pdf(file.path(dir, "post_processing", "plots",
                paste0(dataset, "_algii_heatmap.pdf")))
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


# 4 - Validate Array ------------------------------------------------------

#' Import array validation data and filter for overlapping samples
#'
#' Combine TCGA and GSE validation data and keep overlapping samples
#'
#' @param dir directory for TCGA and GSE validation data
#' @param osamples character vector of overlapping samples. Given as "sampleID"
#'   column from `load_overlap()`
import_array_overlap <- function(dir = "data", osamples) {
  overlap_array <- c("tcga", "gse") %>%
    purrr::map_df(
      ~ file.path(dir, "OverlapSet", paste0("validation_", ., ".rds")) %>%
        readRDS() %>%
        tibble::rownames_to_column("sampleID")
    ) %>%
    dplyr::filter(sampleID %in% osamples) %>%
    tibble::column_to_rownames("sampleID")
  overlap_array
}

#' Join published labels with overlap array predictions
#'
#' @param pred predicted overlap array labels
#' @param overlap tibble of overlap sampleID, ottaID, and published labels
join_published_array <- function(pred, overlap) {
  pred %>%
    tibble::tibble(sampleID = rownames(attr(., "prob")), array = .) %>%
    dplyr::inner_join(overlap, ., by = "sampleID")
}

#' Evaluate overlap array predictions using splendid and caret
#'
#' @param data tibble of published labels and predicted overlap array labels
evaluate_array <- function(data) {
  p <- data[["published"]]
  a <- data[["array"]]
  is_pub <- !is.na(p)

  pp <- p[is_pub]
  ap <- a[is_pub]
  attributes(ap) <- attributes(a)
  attr(ap, "prob") <- attr(ap, "prob")[is_pub, ]
  attr(ap, "class.thres") <- attr(ap, "class.thres")[is_pub]
  list(
    published_vs_array = list(
      metrics = splendid::evaluation(pp, ap),
      confmat = caret::confusionMatrix(ap, pp)
    )
  )
}


# 7 - Mapping Signatures C2 -----------------------------------------------

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
