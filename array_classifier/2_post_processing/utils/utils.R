# Dependencies ----

# load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
})

# Functions ----

# build mapping
build_mapping <- function(train.set) {
  # label mapping
  labs <- seq_len(4)
  if (train.set == "ov.afc1_cbt") {
    data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    stop("No valid training set specified")
  }
}

#********************************************************************
# Import specified study with housekeeping normalization or not.
#     is.test.set: required to specify format if test set should be
#                  returned.
#********************************************************************
import_study <- function(dir, study = "ov.afc1_cbt", hc.normalize = TRUE) {
  # specify hc normalized npcp or not
  hc <- ifelse(hc.normalize, "-hcNorm", "")

  # import the npcp and diceR labels
  dat <- paste0(dir, "data_pr_", study, "/npcp", hc, "_", study, ".rds") %>%
    readr::read_rds() %>%
    `rownames<-`(stringr::str_sub(rownames(.), end = -8))

  # select k-modes as best enemble algorithm and map labels to npcp
  y <- paste0(dir, "data_pr_", study, "/all_clusts_", study, ".rds") %>%
    readr::read_rds() %>%
    dplyr::select(labs = kmodes) %>%
    dplyr::inner_join(build_mapping(study), by = "labs") %>%
    dplyr::select(y = labels)

  data.frame(y, dat)
}

#********************************************************************
# train a given algorithm on a given training set
#   x.processed: training set
#   alg: algorithm of interest (see splendid docs for further details)
#********************************************************************
train_final <- function(x.processed, alg) {
  splendid::classification(
    data = x.processed[, -1],
    class = x.processed[, 1],
    algorithms = alg,
    standardize = FALSE
  )
}

#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
get_mapping <- function(dir = "data/") {
  # TCGA overlap
  tcga.mapped <- paste0(dir, "TCGA_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv() %>%
    dplyr::select(
      sampleID = TCGA,
      ottaID = `OTTA-ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # GSE overlap
  gse.mapped <- paste0(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv") %>%
    readr::read_csv() %>%
    dplyr::select(
      sampleID = GSE9891,
      ottaID = `OTTA ID`,
      published = `MOL-SUBTYPE-NAME (published)`
    )

  # combine & drop NAs
  dplyr::bind_rows(tcga.mapped, gse.mapped) %>%
    dplyr::filter(published != "n/a")
}

#********************************************************************
# Import array data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
import_array <- function(dir = "data/", map) {
  # GSE
  validation.gse <- paste0(dir, "ValidationSet/validation_gse.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("sampleID")

  # TCGA
  validation.tcga <- paste0(dir, "ValidationSet/validation_tcga.rds") %>%
    readr::read_rds() %>%
    tibble::rownames_to_column("sampleID")

  # combine and match with mapping table
  validation.set <- dplyr::bind_rows(validation.gse, validation.tcga) %>%
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
# Simple predict function to take it a fit and predict on new.data
#********************************************************************
predict_overlap <- function(fit, new.data) {
  splendid::prediction(
    mod = fit,
    data = new.data,
    class = seq_len(nrow(new.data))
  ) %>%
    data.table::setattr("sampleID", rownames(new.data))
}

#********************************************************************
# Return list of evaluation measures. Output is required for ploting.
#********************************************************************
evaluate_array <- function(x) {
  published_vs_array <- list(
    splendid::evaluation(x$published, x$array),
    caret::confusionMatrix(x$published, x$array)
  )
  tibble::lst(published_vs_array)
}

#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
top_algo_plot <- function(dir = "./", threshold = TRUE, plot.title,
                          print = TRUE, save = TRUE, col.cust = NULL) {
  if (!threshold) {
    # import IV results
    sup.iv.xpn <- paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds") %>%
      readr::read_rds()
    sup.iv.cbt <- paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds") %>%
      readr::read_rds()
  } else {
    # import IV results
    sup.iv.xpn <- paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds") %>%
      readr::read_rds()
    sup.iv.cbt <- paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds") %>%
      readr::read_rds()
  }

  # process data for general metrics
  iv.combine <- rbind(sup.iv.xpn, sup.iv.cbt) %>%
    dplyr::filter(normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(batch_correction = as.factor(batch_correction))

  # plot general metrics
  pd <- ggplot2::position_dodge(width = 0.5)
  if (is.null(col.cust)) {
    if (threshold) {
      col <- c("#e66b00", "#efa667",
               "#05660e", "#7acc81",
               "#878787", "#c6c6c6",
               "blue", "#6f95e8",
               "magenta", "#f49ae3",
               "purple", "#b296d3",
               "#ffbe00", "#ffd866",
               "brown", "#ce9565",
               "red", "#dd9d9d")
    } else {
      col <- c("#878787", "#c6c6c6",
               "magenta", "#f49ae3",
               "blue", "#6f95e8",
               "purple", "#b296d3",
               "#05660e", "#7acc81",
               "#e66b00", "#efa667",
               "#ffbe00", "#ffd866",
               "brown", "#ce9565",
               "red", "#dd9d9d")
    }
  } else {
    col <- col.cust
  }

  # prepare df for ggplot2
  df <- iv.combine %>%
    dplyr::mutate(mod = reorder(mod, -percentile_50),
                  bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # plot evaluation measures
  p1 <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = mod,
      y = percentile_50,
      colour = bcm,
      group = bcm
    )) +
    ggplot2::geom_point(position = pd) +
    ggplot2::facet_wrap(~measure) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
      width = 0.4,
      position = pd
    ) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0.6, 1) +
    ggplot2::scale_colour_manual(values = col, name = "Batch and Model") +
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::guides(color = guide_legend(nrow = 2, byrow = FALSE))

  # save plot
  if (save) {
    ggplot2::ggsave(plot = p1,
                    filename = paste0("outputs/plots/all_algos_ranked.png"))
  }
  if (print) print(p1)
  p1
}

#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
sup_plots <- function(dir = "./", threshold = TRUE, plot.title,
                      algs = c("mlr_ridge", "mlr_lasso"),
                      print = TRUE, save = TRUE, col.cust = NULL) {
  if (!threshold) {
    # import IV results
    sup.iv.xpn <- paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds") %>%
      readr::read_rds()
    sup.iv.cbt <- paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds") %>%
      readr::read_rds()
  } else {
    # import IV results
    sup.iv.xpn <- paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds") %>%
      readr::read_rds()
    sup.iv.cbt <- paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds") %>%
      readr::read_rds()
  }

  # create mapping tables for xpn & cbt
  xpn.map <- build_mapping("ov.afc1_xpn") %>%
    dplyr::transmute(labels, class = as.factor(labs))
  cbt.map <- build_mapping("ov.afc1_cbt") %>%
    dplyr::transmute(labels, class = as.factor(labs))

  # process the xpn data
  iv.xpn.class <- sup.iv.xpn %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  stringr::str_detect(measure, "\\.")) %>%
    tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
    dplyr::mutate_at(c("measure", "class"), as.factor) %>%
    dplyr::select(-class, class) %>%
    dplyr::inner_join(xpn.map, by = "class")

  # process the cbt data
  iv.cbt.class <- sup.iv.cbt %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  stringr::str_detect(measure, "\\.")) %>%
    tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
    dplyr::mutate_at(c("measure", "class"), as.factor) %>%
    dplyr::select(-class, class) %>%
    dplyr::inner_join(cbt.map, by = "class")

  # combine xpn and cbt
  iv.combine.class <- rbind(iv.xpn.class, iv.cbt.class) %>%
    dplyr::mutate(bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # plot class-wise metrics
  pd <- ggplot2::position_dodge(width = 0.5)
  if (is.null(col.cust)) {
    if (threshold) {
      col <- c("#05660e", "#7acc81", "#e66b00", "#efa667")
    } else {
      col <- c("#878787", "#c6c6c6","magenta", "#f49ae3")
    }
  } else {
    col <- col.cust
  }

  # store common ggplot layers
  gglayers <- list(
    ggplot2::aes(y = percentile_50, colour = bcm, group = bcm),
    ggplot2::geom_point(position = pd),
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
      width = 0.4,
      position = pd
    ),
    ggplot2::theme_bw(),
    ggplot2::facet_wrap(~ measure, scales = "free"),
    ggplot2::scale_colour_manual(values = col, name = "Batch and Model"),
    ggplot2::labs(y = "Evaluation Measure Value", title = plot.title)
  )

  # create iv plot
  p1 <- iv.combine.class %>%
    ggplot2::ggplot(ggplot2::aes(x = labels)) +
    ggplot2::xlab("Subtype") +
    gglayers

  # process and combine xpn and cbt for general metrics
  iv.combine <- rbind(sup.iv.xpn, sup.iv.cbt) %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(batch_correction = as.factor(batch_correction),
                  bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # plot general metrics
  p2 <- iv.combine %>%
    ggplot2::ggplot(ggplot2::aes(x = mod)) +
    ggplot2::xlab("Evaluation Measure") +
    gglayers

  # save plots
  if (save) {
    fn <- paste0("outputs/plots/", stringr::str_remove_all(plot.title, " "))
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

#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.dir: path to evaluation list
#********************************************************************
plot_evals <- function(eval.dir, plot.title, algs = c("mlr_ridge", "mlr_lasso"),
                       threshold = TRUE, print = TRUE, save = TRUE,
                       col.cust = NULL) {
  # import eval list
  evals <- eval.dir %>%
    readr::read_rds() %>%
    purrr::imap(~ {
      if (grepl(paste(algs, collapse = "|"), .y)) {
        purrr::map(.x, ~ .)
      }
    }) %>%
    purrr::compact()

  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(purrr::flatten) %>%
    purrr::map(~ data.frame(cs = .$cs) %>%
                 tibble::rownames_to_column("measure"))

  # grab the names of the list
  names.list <- names(evals.extract)

  # prepare the eval list for plotting
  evals.prep <- purrr::map2(names.list, evals.extract,
                            ~ .y %>% data.frame(alg = .x, .)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      alg = as.factor(alg),
      measure = as.factor(measure),
      class = stringr::str_split(measure, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor(),
      measure = stringr::str_split(measure, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor(),
      mod = stringr::str_split(alg, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor(),
      batch_correction = stringr::str_split(alg, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor(),
      value = cs) %>%
    dplyr::select(-c(cs, alg))

  # reorganize factor levels
  evals.prep$mod <- factor(evals.prep$mod, levels = rev(levels(evals.prep$mod)))

  # specify positioning and colour
  pd <- ggplot2::position_dodge(width = 0.5)
  if (is.null(col.cust)) {
    if (threshold) {
      col <- c("#05660e", "#7acc81", "#e66b00", "#efa667")
    } else {
      col <- c("#c6c6c6", "#878787", "#f49ae3", "magenta")
    }
  } else {
    col <- col.cust
  }

  brks <- paste(rep(c("xpn", "cbt"), 2), rep(algs, each = 2), sep = ".")

  # create class-wise plots for class eval measures
  p1 <- evals.prep %>%
    ggplot2::ggplot(ggplot2::aes(
      x = class,
      y = value,
      colour = interaction(batch_correction, mod),
      group = interaction(batch_correction, mod)
    )) +
    ggplot2::geom_point(position = pd, size = 3) +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::scale_colour_manual(values = col,
                                 name = "Batch and Model",
                                 breaks = brks) +
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::ylim(c(0, 1))

  # prepare eval list for plottin overall evaluation measures
  overall <- evals %>%
    purrr::map(purrr::flatten) %>%
    purrr::map(~ data.frame(
      accuracy = .$accuracy,
      auc = .$auc,
      macro_f1 = .$macro_f1
    )) %>%
    purrr::map2(., names.list, ~ data.frame(mod = .y, .x)) %>%
    dplyr::bind_rows() %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    dplyr::mutate(
      alg = stringr::str_split(mod, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor() %>%
        factor(levels = rev(levels(.))), # reorganize factor levels
      batch_correction = stringr::str_split(mod, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor()
    ) %>%
    dplyr::select(-mod)

  # specify colouring
  if (is.null(col.cust)) {
    if (threshold) {
      col <- c("#7acc81", "#05660e", "#efa667", "#e66b00")
    } else {
      col <- c("#f49ae3", "magenta", "#c6c6c6", "#878787")
    }
  } else {
    col <- col.cust
  }

  # plot overall evaluation measures
  p2 <- overall %>%
    ggplot2::ggplot(ggplot2::aes(
      x = alg,
      y = value,
      colour = interaction(batch_correction, alg),
      group = interaction(batch_correction, alg)
    )) +
    ggplot2::geom_point(position = pd, size = 3) +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(values = col,
                                 name = "Batch and Model",
                                 breaks = brks) +
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::ylim(c(0.3, 0.9))

  # save plots
  if (save) {
    fn <- paste0("outputs/plots/", stringr::str_remove_all(plot.title, " "))
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

#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.dir: path to evaluation list
#********************************************************************
plot_evals_noCBT <- function(dir, plot.title,
                             algs = c("mlr_ridge", "mlr_lasso", "adaboost", "rf"),
                             print = TRUE, save = TRUE, col.cust = NULL,
                             y.lim.class = c(0.1, 1), y.lim.all = c(0.25, 1)) {
  # import eval list
  evals <- readr::read_rds(dir)

  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(purrr::flatten) %>%
    purrr::map(~ data.frame(cs = .$cs) %>%
                 tibble::rownames_to_column("measure"))

  # grab the names of the list
  names.list <- names(evals.extract)

  # prepare the eval list for plotting
  evals.prep <- purrr::map2(names.list, evals.extract,
                            ~ .y %>% data.frame(alg = .x, .)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      alg = as.factor(alg),
      measure = as.factor(measure),
      class = stringr::str_split(measure, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor(),
      measure = stringr::str_split(measure, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor(),
      mod = stringr::str_split(alg, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor() %>%
        factor(levels = rev(levels(.))), # reorganize factor levels
      batch_correction = stringr::str_split(alg, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor(),
      value = cs) %>%
    dplyr::select(-c(cs, alg)) %>%
    dplyr::filter(batch_correction == "xpn")

  # specify positioning and colour
  pd <- ggplot2::position_dodge(width = 0.5)
  col <- c("#878787", "magenta", "#05660e", "#e66b00")
  brks <- paste(rep(c("xpn"), 4), algs, sep = ".")

  # create class-wise plots for class eval measures
  p1 <- evals.prep %>%
    ggplot2::ggplot(ggplot2::aes(
      x = class,
      y = value,
      colour = interaction(batch_correction, mod),
      group = interaction(batch_correction, mod)
    )) +
    ggplot2::geom_point(position = pd, size = 3) +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::scale_colour_manual(values = col,
                                 name = "Batch and Model",
                                 breaks = brks) +
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::ylim(y.lim.class)

  # prepare eval list for plottin overall evaluation measures
  overall.prep <- evals %>%
    purrr::map(purrr::flatten) %>%
    purrr::map(~ data.frame(
      accuracy = .$accuracy,
      auc = .$auc,
      macro_f1 = .$macro_f1
    )) %>%
    purrr::map2(., names.list, ~ data.frame(mod = .y, .x)) %>%
    dplyr::bind_rows() %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    dplyr::mutate(
      alg = stringr::str_split(mod, "\\.") %>%
        purrr::map(~ .[2]) %>%
        unlist() %>%
        as.factor(),
      batch_correction = stringr::str_split(mod, "\\.") %>%
        purrr::map(~ .[1]) %>%
        unlist() %>%
        as.factor()
    ) %>%
    dplyr::select(-mod) %>%
    dplyr::filter(batch_correction == "xpn")

  # reorganize factor levels
  overall.prep$alg <- factor(overall.prep$alg,
                             levels = rev(levels(overall.prep$alg)))

  # specify colouring
  col <- c("#05660e", "magenta", "#878787", "#e66b00")

  # plot overall evaluation measures
  p2 <- overall.prep %>%
    ggplot2::ggplot(ggplot2::aes(
      x = alg,
      y = value,
      colour = interaction(batch_correction, alg),
      group = interaction(batch_correction, alg)
    )) +
    ggplot2::geom_point(position = pd, size = 3) +
    ggplot2::facet_wrap(~measure, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(values = col,
                                 name = "Batch and Model",
                                 breaks = brks) +
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::ylim(y.lim.all)

  # save plots
  if (save) {
    fn <- paste0("outputs/plots/", stringr::str_remove_all(plot.title, " "))
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
