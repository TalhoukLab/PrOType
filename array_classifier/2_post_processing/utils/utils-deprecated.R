# Deprecated functions from initial post-processing code ------------------

# Use current utility functions
source(here::here("array_classifier/2_post_processing/utils/utils.R"))

# Import specified study with housekeeping normalization or not.
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

# import (threshold) iv summary data
import_iv <- function(dir = "data", threshold = TRUE) {
  # IV threshold filenames
  fn.iv.xpn <- file.path(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
  fn.iv.cbt <- file.path(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
  if (!threshold) {
    # IV filenames
    fn.iv.xpn <- gsub("_threshold", "", fn.iv.xpn)
    fn.iv.cbt <- gsub("_threshold", "", fn.iv.cbt)
  }
  sup.iv.xpn <- readr::read_rds(fn.iv.xpn)
  sup.iv.cbt <- readr::read_rds(fn.iv.cbt)
  rbind(sup.iv.xpn, sup.iv.cbt)
}

# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
top_algo_plot <- function(plot.title, dir = "data", threshold = TRUE,
                          print = TRUE, save = TRUE, col.cust = NULL) {
  # import iv data, process for general metrics, prepare for ggplot
  df <- import_iv(dir = dir, threshold = threshold) %>%
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
    ggplot2::labs(x = "Evaluation Measure",
                  y = "Evaluation Measure Value",
                  title = plot.title) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = guide_legend(nrow = 2, byrow = FALSE))

  # save plot
  if (save) {
    ggplot2::ggsave(plot = p, filename = "outputs/plots/all_algos_ranked.png")
  }
  if (print) print(p)
  p
}

# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
sup_plots <- function(plot.title, dir = "data", threshold = TRUE,
                      algs = c("mlr_ridge", "mlr_lasso"),
                      print = TRUE, save = TRUE, col.cust = NULL) {
  # create mapping tables for xpn & cbt
  maps <- c("ov.afc1_xpn", "ov.afc1_cbt") %>%
    purrr::set_names(c("xpn", "cbt")) %>%
    purrr::map(build_mapping) %>%
    purrr::map(dplyr::transmute, labels, class = as.factor(labs))

  # import iv data
  iv.data <- import_iv(dir = dir, threshold = threshold)

  # process and combine xpn and cbt data
  iv.combine.class <- iv.data %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  stringr::str_detect(measure, "\\.")) %>%
    tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
    dplyr::mutate_at(c("measure", "class"), as.factor) %>%
    dplyr::select(-class, class) %>%
    dplyr::mutate(
      labels = unlist(purrr::map2(
        class, batch_correction,
        ~ maps[[.y]]$labels[match(.x, maps[[.y]]$class)])),
      bcm = interaction(batch_correction, mod)
    ) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # process and combine xpn and cbt for general metrics
  iv.combine <- iv.data %>%
    dplyr::filter(mod %in% algs,
                  normalization == "hc",
                  measure %in% c("auc", "accuracy", "macro_f1")) %>%
    dplyr::mutate(bcm = interaction(batch_correction, mod)) %>%
    dplyr::group_by(batch_correction, mod, measure)

  # create colour palette
  col <- col.cust %||% purrr::map_chr(levels(droplevels(iv.combine$bcm)),
                                      match_colour)

  # store common ggplot layers
  gglayers <- list(
    ggplot2::aes(y = percentile_50, colour = bcm, group = bcm),
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)),
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = percentile_5, ymax = percentile_95),
      width = 0.4,
      position = ggplot2::position_dodge(width = 0.5)
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

  # plot general metrics
  p2 <- iv.combine %>%
    ggplot2::ggplot(ggplot2::aes(x = mod)) +
    ggplot2::xlab("Evaluation Measure") +
    gglayers

  # save plots
  if (save) {
    fn <- file.path("outputs/plots", gsub(" ", "", plot.title))
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

# Plot evaluation measures by class and overall without CBT
plot_evals_noCBT <- function(plot.title, dir, output_dir,
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

  # prepare eval list for plotting
  evals.prep <- purrr::map2_df(names.list, evals.extract,
                               ~ .y %>% data.frame(alg = .x, .)) %>%
    dplyr::mutate_at(c("alg", "measure"), as.factor) %>%
    tidyr::separate(measure, c("measure", "class"), sep = "\\.") %>%
    tidyr::separate(alg, c("batch_correction", "mod"), sep = "\\.") %>%
    dplyr::mutate_at(c("measure", "class", "batch_correction", "mod"),
                     as.factor) %>%
    dplyr::mutate(mod = factor(mod, levels = rev(levels(mod)))) %>% # reverse
    dplyr::select(-cs, value = cs) %>% # move to end and rename
    dplyr::filter(batch_correction == "xpn") %>%
    dplyr::mutate(bcm = interaction(batch_correction, mod))

  # prepare eval list for plotting overall evaluation measures
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
