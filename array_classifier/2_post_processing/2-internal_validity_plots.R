library(magrittr)
source(here::here("array_classifier/2_post_processing/utils/utils.R"))

# IV Plot Definitions -----------------------------------------------------

# Produce internal validity plots -----------------------------------------

plot_args <- list(dir = outputDir, datasets = datasets, print = FALSE, save = TRUE)
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

cli::cat_line("Plotting algorithm by internal validity index heatmap")
purrr::walk(datasets, algii_heatmap, dir = outputDir)
