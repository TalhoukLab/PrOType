# Dependencies ----

# load dependencies
suppressPackageStartupMessages({
  library(purrr)
})

source(here::here("assets/utils.R"))

# Functions ----

#********************************************************************
# Import specified study with housekeeping normalization or not.
#     is.test.set: required to specify format if test set should be
#                  returned.
#********************************************************************
import_study <- function(dir = "data", study = "ov.afc1_cbt") {
  subdir <- paste0("data_pr_", study) # subdirectory

  # import the npcp and diceR labels
  dat <- file.path(dir, subdir, paste0("npcp-hcNorm_", study, ".rds")) %>%
    readr::read_rds() %>%
    `rownames<-`(stringr::str_sub(rownames(.), end = -8))

  # select k-modes as best enemble algorithm and map labels to npcp
  y <- file.path(dir, subdir, paste0("all_clusts_", study, ".rds")) %>%
    readr::read_rds() %>%
    dplyr::select(labs = kmodes) %>%
    dplyr::inner_join(build_mapping(study), by = "labs") %>%
    dplyr::select(y = labels)

  data.frame(y, dat)
}

#********************************************************************
# Import array data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
import_array <- function(dir = "data", map) {
  # combine GSE and TCGA validation data and match with mapping table
  validation.set <- c("gse", "tcga") %>%
    purrr::map_df(
      ~ file.path(dir, "ValidationSet", paste0("validation_", ., ".rds")) %>%
        readr::read_rds() %>%
        tibble::rownames_to_column("sampleID")
    ) %>%
    dplyr::inner_join(map["sampleID"], ., by = "sampleID") %>%
    as.data.frame() %>%
    tibble::column_to_rownames("sampleID")
  validation.set
}

#********************************************************************
# Combine array with predictions and and join with mapping table
#********************************************************************
get_overlap <- function(array, pred, map) {
  # join the validation set
  array %>%
    tibble::rownames_to_column("sampleID") %>%
    data.frame(array = pred) %>%
    dplyr::inner_join(map, by = "sampleID") %>%
    dplyr::select(sampleID, ottaID, published, array) %>%
    dplyr::filter(published != "n/a") %>%
    dplyr::mutate(published = as.factor(published))
}

#********************************************************************
# Return list of evaluation measures. Output is required for ploting.
#********************************************************************
evaluate_array <- function(data) {
  y <- data$array
  x <- factor(make.names(data$published))

  cat("Calling splendid\n")
  published_vs_array <- list(
    splendid::evaluation(x, y),
    caret::confusionMatrix(y, x)
  )
  tibble::lst(published_vs_array)
}

# #********************************************************************
# # Plot evaluation measures by class and overall for top algorithms
# # across bootstrap samples (output retrieved from supervised pipeline)
# #********************************************************************
# top_algo_plot <- function(plot.title, dir = "data", threshold = TRUE,
#                           print = TRUE, save = TRUE, col.cust = NULL) {
#   # import iv data, process for general metrics, prepare for ggplot
#   df <- import_iv(dir = dir, threshold = threshold) %>%
#     dplyr::filter(normalization == "hc",
#                   measure %in% c("auc", "accuracy", "macro_f1")) %>%
#     dplyr::mutate(batch_correction = as.factor(batch_correction),
#                   mod = reorder(mod, -percentile_50),
#                   bcm = interaction(batch_correction, mod)) %>%
#     dplyr::group_by(batch_correction, mod, measure)
#
#   # create colour palette
#   col <- col.cust %||% purrr::map_chr(levels(droplevels(df$bcm)), match_colour)
#
#   # plot evaluation measures
#   p <- df %>%
#     ggplot2::ggplot(ggplot2::aes(
#       x = mod,
#       y = percentile_50,
#       colour = bcm,
#       group = bcm
#     )) +
#     ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
#     ggplot2::facet_wrap(~measure) +
#     ggplot2::geom_errorbar(
#       ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
#       width = 0.4,
#       position = ggplot2::position_dodge(width = 0.5)
#     ) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(0.6, 1) +
#     ggplot2::scale_colour_manual(values = col, name = "Batch and Model") +
#     ggplot2::labs(x = "Evaluation Measure",
#                   y = "Evaluation Measure Value",
#                   title = plot.title) +
#     ggplot2::theme(legend.position = "bottom") +
#     ggplot2::guides(color = guide_legend(nrow = 2, byrow = FALSE))
#
#   # save plot
#   if (save) {
#     ggplot2::ggsave(plot = p, filename = "outputs/plots/all_algos_ranked.png")
#   }
#   if (print) print(p)
#   p
# }

# #********************************************************************
# # Plot evaluation measures by class and overall for top algorithms
# # across bootstrap samples (output retrieved from supervised pipeline)
# #********************************************************************
# sup_plots <- function(plot.title, dir = "data", threshold = TRUE,
#                       algs = c("mlr_ridge", "mlr_lasso"),
#                       print = TRUE, save = TRUE, col.cust = NULL) {
#   # create mapping tables for xpn & cbt
#   maps <- c("ov.afc1_xpn", "ov.afc1_cbt") %>%
#     purrr::set_names(c("xpn", "cbt")) %>%
#     purrr::map(build_mapping) %>%
#     purrr::map(dplyr::transmute, labels, class = as.factor(labs))
#
#   # import iv data
#   iv.data <- import_iv(dir = dir, threshold = threshold)
#
#   # process and combine xpn and cbt data
#   iv.combine.class <- iv.data %>%
#     dplyr::filter(mod %in% algs,
#                   normalization == "hc",
#                   stringr::str_detect(measure, "\\.")) %>%
#     tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
#     dplyr::mutate_at(c("measure", "class"), as.factor) %>%
#     dplyr::select(-class, class) %>%
#     dplyr::mutate(
#       labels = unlist(purrr::map2(
#         class, batch_correction,
#         ~ maps[[.y]]$labels[match(.x, maps[[.y]]$class)])),
#       bcm = interaction(batch_correction, mod)
#     ) %>%
#     dplyr::group_by(batch_correction, mod, measure)
#
#   # process and combine xpn and cbt for general metrics
#   iv.combine <- iv.data %>%
#     dplyr::filter(mod %in% algs,
#                   normalization == "hc",
#                   measure %in% c("auc", "accuracy", "macro_f1")) %>%
#     dplyr::mutate(bcm = interaction(batch_correction, mod)) %>%
#     dplyr::group_by(batch_correction, mod, measure)
#
#   # create colour palette
#   col <- col.cust %||% purrr::map_chr(levels(droplevels(iv.combine$bcm)),
#                                       match_colour)
#
#   # store common ggplot layers
#   gglayers <- list(
#     ggplot2::aes(y = percentile_50, colour = bcm, group = bcm),
#     ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)),
#     ggplot2::geom_errorbar(
#       ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
#       width = 0.4,
#       position = ggplot2::position_dodge(width = 0.5)
#     ),
#     ggplot2::theme_bw(),
#     ggplot2::facet_wrap(~ measure, scales = "free"),
#     ggplot2::scale_colour_manual(values = col, name = "Batch and Model"),
#     ggplot2::labs(y = "Evaluation Measure Value", title = plot.title)
#   )
#
#   # create iv plot
#   p1 <- iv.combine.class %>%
#     ggplot2::ggplot(ggplot2::aes(x = labels)) +
#     ggplot2::xlab("Subtype") +
#     gglayers
#
#   # plot general metrics
#   p2 <- iv.combine %>%
#     ggplot2::ggplot(ggplot2::aes(x = mod)) +
#     ggplot2::xlab("Evaluation Measure") +
#     gglayers
#
#   # save plots
#   if (save) {
#     fn <- file.path("outputs/plots", gsub(" ", "", plot.title))
#     ggplot2::ggsave(p1, filename = paste0(fn, "_byclass.png"))
#     ggplot2::ggsave(p2, filename = paste0(fn, "_overall.png"))
#   }
#
#   # print plots
#   if (print) {
#     print(p1)
#     print(p2)
#   }
#   list(p1, p2)
# }

#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.data_directory: path to evaluation list
#********************************************************************
plot_evals_noCBT <- function(plot.title, dir, output_dir,
                             algs = c("mlr_ridge", "mlr_lasso", "adaboost", "rf"),
                             print = TRUE, save = TRUE, col.cust = NULL,
                             y.lim.class = c(0.1, 1), y.lim.all = c(0.25, 1)) {
  # import eval list
  evals <- readr::read_rds(dir)
  str(evals)

  # do some preprocessing on the eval list
  cat("Processing eval list\n")
  evals.extract <- evals %>%
    purrr::map(purrr::flatten) %>%
    purrr::map(~ data.frame(cs = .$cs) %>%
                 tibble::rownames_to_column("measure"))

  # grab the names of the list
  cat("Getting names\n")
  names.list <- names(evals.extract)

  # prepare eval list for plotting
  cat("Preparing for plotting\n")
  evals.prep <- purrr::map2_df(names.list, evals.extract,
                               ~ .y %>% data.frame(alg = .x, .)) #%>%
    # dplyr::mutate_at(c("alg", "measure"), as.factor) %>%
    # tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
    # tidyr::separate(alg, c("batch_correction", "mod"), sep = "\\.") %>%
    # dplyr::mutate_at(c("measure", "class", "batch_correction", "mod"),
    #                  as.factor) %>%
    # dplyr::mutate(mod = factor(mod, levels = rev(levels(mod)))) %>% # reverse
    # dplyr::select(-cs, value = cs) %>% # move to end and rename
    # dplyr::filter(batch_correction == "xpn") %>%
    # dplyr::mutate(bcm = interaction(batch_correction, mod))
  str(evals.prep)

  # prepare eval list for plotting overall evaluation measures
  cat("Preparing for plotting overall evaluation measures")
  # str(evals)
  overall.prep <- evals %>%
    purrr::map(1) %>%
    purrr::map(`[`, c("accuracy", "auc", "macro_f1")) %>%
    purrr::map(as.data.frame) %>%
    purrr::map2_df(., names.list, ~ data.frame(mod = .y, .x)) %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    tidyr::separate(mod, c("batch_correction", "alg"), sep = "\\.") %>%
    dplyr::mutate_at(c("batch_correction", "alg"), as.factor) %>%
    dplyr::mutate(alg = factor(alg, levels = rev(levels(alg))), # reverse
                  bcm = interaction(batch_correction, alg)) %>%
    dplyr::filter(batch_correction == "xpn")

  # store common ggplot layers
  cat("Storing ggplot layers\n")
  gglayers <- list(
    ggplot2::aes(y = value, colour = bcm, group = bcm),
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5),
                        size = 3),
    ggplot2::facet_wrap(~ measure, scales = "free") ,
    ggplot2::theme_bw(),
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title)
  )
  # specify breaks
  brks <- paste("xpn", algs, sep = ".")

  # create class-wise plots for class eval measures
  p1 <- evals.prep %>%
    ggplot2::ggplot(ggplot2::aes(x = class)) +
    ggplot2::scale_colour_manual(
      values = c("#878787", "magenta", "#05660e", "#e66b00"),
      name = "Batch and Model",
      breaks = brks
    ) +
    ggplot2::ylim(y.lim.class) +
    gglayers

  # plot overall evaluation measures
  p2 <- overall.prep %>%
    ggplot2::ggplot(ggplot2::aes(x = alg)) +
    ggplot2::scale_colour_manual(
      values = c("#05660e", "magenta", "#878787", "#e66b00"),
      name = "Batch and Model",
      breaks = brks
    ) +
    ggplot2::ylim(y.lim.all) +
    gglayers

  cat("before save\n")
  # save plots
  if (save) {
    fn <- file.path(output_dir, "/plots", gsub(" ", "", plot.title))
    ggplot2::ggsave(p1, filename = paste0(fn, "_byclass.png"))
    ggplot2::ggsave(p2, filename = paste0(fn, "_overall.png"))
  }

  # print plots
  if (print) {
    print(p1)
    print(p2)
  }
  list(p1, p2)
}

# # import (threshold) iv summary data
# import_iv <- function(dir = "data", threshold = TRUE) {
#   # IV threshold filenames
#   fn.iv.xpn <- file.path(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
#   fn.iv.cbt <- file.path(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
#   if (!threshold) {
#     # IV filenames
#     fn.iv.xpn <- gsub("_threshold", "", fn.iv.xpn)
#     fn.iv.cbt <- gsub("_threshold", "", fn.iv.cbt)
#   }
#   sup.iv.xpn <- readr::read_rds(fn.iv.xpn)
#   sup.iv.cbt <- readr::read_rds(fn.iv.cbt)
#   rbind(sup.iv.xpn, sup.iv.cbt)
# }

# match batch and algorithm combinations to colour palette
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

### 1 - EVALUATE BATCH EFFECT FUNCTIONS
# Functions----
dropChar <- function(x) {
  x <- as.character(x)
  substr(x, 0, nchar(x) - 7)
}

#********************************************************************
# Generate substrain mapping table for specified study
#********************************************************************
build_mapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_cbt") {
    map <- data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    map <- data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    print("No valid training set specified")
  }
  return(map)
}

##################################################
# Function to compute PVCA Object
##################################################
CompSrcOfVar <- function(annMat, dat, factorsOfInterest, cols, ttl = "",
                         pct_threshold = 0.6) {
  # rows of exp should be the same as column of dat and in the same order
  if (!all(rownames(annMat) == colnames(dat))) {
    stop("All rownames of annotation matrix do not correspond to the column names of the data matrix")
  }
  phenoData <- new("AnnotatedDataFrame", data = annMat)
  MASet <- Biobase::ExpressionSet(assayData = data.matrix(dat),
                                  phenoData = phenoData)
  pvca::pvcaBatchAssess(MASet, factorsOfInterest, pct_threshold)
}

##################################################
# Function to Plot PVCA Object
##################################################
pvca.plot <- function(pvcaObj, cols = "blue", ttl = "") {
  par(oma = c(1, 0.5, 1, 1), mar = c(5.1, 7.6, 4.1, 0.1))
  # "Weighted average proportion variance"
  bp <- barplot(sort(pvcaObj$dat), horiz = TRUE, ylab = "", xlab = "",
                xlim = c(0,1.1), border = "white", main = ttl,
                space = 1, las = 1, col = cols)
  axis(2, at = bp, labels = pvcaObj$label[order(pvcaObj$dat)], ylab = "", cex.axis = 0.8, las = 2)
  values <- sort(pvcaObj$dat)
  new_values <- paste(round(values * 100, 1), "%", sep = "")
  text(sort(pvcaObj$dat), bp, labels = new_values, pos = 4, cex = 0.8)
}

### 2 - INTERNAL VALIDITY PLOTS

#********************************************************************
# Generate substrain mapping table for specified study
#********************************************************************
build_mapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set %in% c("ov.afc1_cbt", "ov.afc2_cbt")) {
    map <- data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if (train.set %in% c("ov.afc1_xpn", "ov.afc2_xpn")) {
    map <- data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    print("No valid training set specified")
  }
  return(map)
}

# import (threshold) iv summary data
import_iv <- function(dir = "data", datasets = c("ov.afc1_xpn", "ov.afc1_cbt"),
                      threshold = TRUE) {

  # IV threshold filenames
  fn.iv.xpn <- file.path(dir, datasets[1],
                         paste0("data_pr_", datasets[1]),
                         paste0("iv_summary_", datasets[1], "_threshold.rds"))
  fn.iv.cbt <- file.path(dir, datasets[2],
                         paste0("data_pr_", datasets[2]),
                         paste0("iv_summary_", datasets[2], "_threshold.rds"))

  if (!threshold) {
    # IV filenames
    fn.iv.xpn <- gsub("_threshold", "", fn.iv.xpn)
    fn.iv.cbt <- gsub("_threshold", "", fn.iv.cbt)
  }
  sup.iv.xpn <- readr::read_rds(fn.iv.xpn)
  sup.iv.cbt <- readr::read_rds(fn.iv.cbt)
  rbind(sup.iv.xpn, sup.iv.cbt)
}

#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
top_algo_plot <- function(plot.title, dir, datasets, threshold = FALSE,
                          print = TRUE, save = TRUE, col.cust = NULL) {
  # import iv data, process for general metrics, prepare for ggplot
  df <- import_iv(dir = dir, datasets = datasets, threshold = threshold) %>%
    dplyr::filter(normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(batch_correction = as.factor(batch_correction),
                  mod = reorder(mod, -percentile_50),
                  bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # create colour palette
  col <- col.cust %||% purrr::map_chr(levels(droplevels(df$bcm)), match_colour)

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
  th <- ifelse(threshold, "thresh", "nothresh")
  if (save) {
    ggplot2::ggsave(
      plot = p,
      filename = file.path(outputDir, "plots", paste0("all_algos_ranked", th, ".png"))
    )
  }
  if (print) print(p)
  p
}

#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
sup_plots <- function(plot.title, dir, datasets,
                      algs = c("mlr_ridge", "mlr_lasso"),
                      algs_t = c("adaboost", "rf"),
                      print = TRUE, save = TRUE, col.cust = NULL) {
  # create mapping tables for xpn & cbt
  maps <- datasets %>%
    purrr::set_names(c("xpn", "cbt")) %>%
    purrr::map(build_mapping) %>%
    purrr::map(dplyr::transmute, labels, class = as.factor(labs))

  # import iv data
  iv.data <- import_iv(dir = dir, datasets = datasets, threshold = FALSE)
  iv.data_t <- import_iv(dir = dir, datasets = datasets, threshold = TRUE)

  # process and combine xpn and cbt data for threshold/no threshold
  iv.class <- iv.data %>%
    dplyr::filter(mod %in% algs,
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
    ) %>%
    dplyr::group_by(batch_correction, mod, measure)

  iv.class_t <- iv.data_t %>%
    dplyr::filter(mod %in% algs_t,
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
    ) %>%
    dplyr::group_by(batch_correction, mod, measure)
  iv.combine.class <- rbind.data.frame(iv.class, iv.class_t)

  # process and combine xpn and cbt for general metrics
  iv.general <- iv.data %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)
  iv.general_t <- iv.data_t %>%
    dplyr::filter(mod %in% algs_t,
                  normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)
  iv.combine <- rbind.data.frame(iv.general, iv.general_t)

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
    ggplot2::labs(y = "Evaluation Measure Value", title = plot.title)
  )

  # create iv plot
  p1 <- iv.combine.class %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = labels)) +
    ggplot2::xlab("Subtype") +
    gglayers

  # plot general metrics
  p2 <- iv.combine %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = mod)) +
    ggplot2::xlab("Evaluation Measure") +
    gglayers

  # save plots
  if (save) {
    fn <- gsub(" ", "", plot.title)
    ggplot2::ggsave(p1, filename = file.path(outputDir, "plots", paste0(fn, "_byclass.png")))
    ggplot2::ggsave(p2, filename = file.path(outputDir, "plots", paste0(fn, "_overall.png")))
  }

  # print plots
  if (print) {
    print(p1)
    print(p2)
  }
  list(p1, p2)
}

# Heatmap on internal validitiy indices by algorithm
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


### 3 - PREDICT C2

#********************************************************************
# Builds mapping matrix from integer class to correct labels names
buildMapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM", "C4-DIF", "C5-PRO", "C1-MES"))
  } else {
    stop("Can only relabel C1 XPN")
  }
}


### 5 - MAPPING SIGNATURES C2

# Calculate entropy loss and quadratic loss of a matrix
matrix_loss <- function(Rh, R = NULL) {
  # Add check that it's a correlation matrix
  if (is.null(R)) {
    R <- diag(nrow(Rh))
  }
  entropy <- psych::tr(solve(R) %*% Rh) - log(det(solve(R) %*% Rh)) - nrow(Rh)
  quadratic <- psych::tr((solve(R) %*% Rh - diag(nrow(Rh)))^2)
  tibble::lst(entropy, quadratic)
}
