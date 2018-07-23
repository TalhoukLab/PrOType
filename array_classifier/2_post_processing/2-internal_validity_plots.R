
# IV Plot Definitions -----------------------------------------------------

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
                   axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggplot2::guides(color = guide_legend(nrow = 2, byrow = FALSE))

  # save plot
  th <- ifelse(threshold, "thresh", "nothresh")
  if (save) {
    ggplot2::ggsave(
      plot = p,
      filename = file.path(outputDir, "plots", "all_algos_ranked", th, ".png")
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
algii_heatmap <- function(ii) {
  # Heatmap: order algorithms by ranked ii, remove indices with NaN
  hm <- ii %>%
    tibble::column_to_rownames("Algorithms") %>%
    magrittr::extract(match(diceR:::consensus_rank(ii, 5)$top.list, rownames(.)),
                      purrr::map_lgl(., ~ all(!is.nan(.x))))

  # Plot heatmap with annotated colours, column scaling, no further reordering
  NMF::aheatmap(
    hm,
    annCol = data.frame(Criteria = c(rep("Maximized", 5),
                                     rep("Minimized", ncol(hm) - 5))),
    annColors = list(Criteria = stats::setNames(c("darkgreen", "deeppink4"),
                                                c("Maximized", "Minimized"))),
    Colv = NA, Rowv = NA, scale = "column", col = "PiYG",
    main = "Ranked Algorithms on Internal Validity Indices"
  )
}

# Produce internal validity plots -----------------------------------------

data_dir <- file.path(dir, datasets, paste0("data_pr_", datasets))
plot_args <- list(dir = data_dir, print = FALSE, save = FALSE)
save_args <- list(width = 16, height = 9)
ranked_fn <- file.path(outputDir, "plots",
                       c("ranked_algorithms_noThreshold.png",
                         "ranked_algorithms_threshold.png"))
top2_fn <- file.path(outputDir, "plots",
                     c("top2_algorithms_byclass.png",
                       "top2_algorithms_overall.png"))

cli::cat_line("Plotting ranked algorithms")
ranked.algo <- purrr::map2(
  c(FALSE, TRUE),
  c("Algorithm Performance Ranking",
    "Algorithm Performance Ranking with Threshold"),
  ~ purrr::invoke(top_algo_plot, plot_args, threshold = .x, plot.title = .y)
) %>% purrr::set_names(ranked_fn)

purrr::iwalk(
  ranked.algo,
  ~ purrr::invoke(ggplot2::ggsave, save_args, filename = .y, plot = .x)
)

cli::cat_line("Plotting top 2 algorithms with and without threshold")
top.algs.no.threshold <- purrr::invoke(
  sup_plots,
  plot_args,
  plot.title = "Top Supervised Algorithm Evaluation",
  algs = c("mlr_ridge", "mlr_lasso"),
  algs_t = c("adaboost", "rf")
) %>% purrr::set_names(top2_fn)

purrr::iwalk(
  top.algs.no.threshold,
  ~ purrr::invoke(ggplot2::ggsave, save_args, filename = .y, plot = .x)
)

hm <- readRDS(file.path(data_dir, paste0("ii_", dataset, ".rds")))
algii_heatmap(hm)
