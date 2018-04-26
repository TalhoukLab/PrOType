###############################################################
###### STEP 0: Plot Overall Algorithm Performance - Cut 1 #####
###############################################################

library(here)
source(here("array_classifier/2_post_processing/utils/utils.R"))

# Common arguments
plot_args <- list(dir = "data", print = FALSE, save = FALSE)
save_args <- list(width = 16, height = 9)
save_dir <- "outputs/plots"

# top algos overall NO threshold
ranked.algo <- purrr::invoke(top_algo_plot,
                             plot_args,
                             threshold = FALSE,
                             plot.title = "Algorithm Performance Ranking")

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "ranked_algorithms_noThreshold.png"),
  plot = ranked.algo
)

# top algos overall threshold
ranked.algo.threshold <- purrr::invoke(top_algo_plot,
                                       plot_args,
                                       threshold = TRUE,
                                       plot.title = "Algorithm Performance Ranking with Threshold")

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "ranked_algorithms_threshold.png"),
  plot = ranked.algo.threshold
)

# top two NO threshold
top.algs <- purrr::invoke(
  sup_plots,
  plot_args,
  threshold = FALSE,
  plot.title = "Top Supervised Alg Evaluation",
  algs = c("mlr_ridge", "mlr_lasso")
)

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "top2_algorithms_byClass_noThreshold.png"),
  plot = top.algs[[1]]
)

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "top2_algorithms_noThreshold.png"),
  plot = top.algs[[2]]
)

# top two threshold
top.algs.threshold <- purrr::invoke(
  sup_plots,
  plot_args,
  threshold = TRUE,
  plot.title = "Top Supervised Alg Evaluation with Threshold",
  algs = c("adaboost", "rf")
)

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "top2_algorithms_byClass_threshold.png"),
  plot = top.algs.threshold[[1]]
)

purrr::invoke(
  ggplot2::ggsave,
  save_args,
  filename = file.path(save_dir, "top2_algorithms_threshold.png"),
  plot = top.algs.threshold[[2]]
)
