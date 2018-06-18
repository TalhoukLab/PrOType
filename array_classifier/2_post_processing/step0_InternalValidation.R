###############################################################
###### STEP 0: Plot Overall Algorithm Performance - Cut 1 #####
###############################################################

library(here)
library(magrittr)
source(here("array_classifier/2_post_processing/utils/utils.R"))

# Common arguments
cat(output_dir, "\n")

plot_args <- list(dir = output_dir, print = FALSE, save = FALSE)
save_args <- list(width = 16, height = 9)
save_dir <- file.path(paste0(output_dir, "plots"))
ranked_fn <- file.path(save_dir, c("ranked_algorithms_noThreshold.png",
                                   "ranked_algorithms_threshold.png"))
top2_fn <- file.path(save_dir, c("top2_algorithms_byClass_noThreshold.png",
                                 "top2_algorithms_noThreshold.png"))
top2_th_fn <- file.path(save_dir, c("top2_algorithms_byClass_threshold.png",
                                    "top2_algorithms_threshold.png"))

# top algos overall (without/with) threshold
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

# top two NO threshold
top.algs.no.threshold <- purrr::invoke(
  sup_plots,
  plot_args,
  threshold = FALSE,
  plot.title = "Top Supervised Alg Evaluation",
  algs = c("mlr_ridge", "mlr_lasso")
) %>% purrr::set_names(top2_fn)

purrr::iwalk(
  top.algs.no.threshold,
  ~ purrr::invoke(ggplot2::ggsave, save_args, filename = .y, plot = .x)
)

# top two threshold
top.algs.threshold <- purrr::invoke(
  sup_plots,
  plot_args,
  threshold = TRUE,
  plot.title = "Top Supervised Alg Evaluation with Threshold",
  algs = c("adaboost", "rf")
) %>% purrr::set_names(top2_th_fn)

purrr::iwalk(
  top.algs.threshold,
  ~ purrr::invoke(ggplot2::ggsave, save_args, filename = .y, plot = .x)
)
