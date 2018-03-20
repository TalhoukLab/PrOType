###############################################################
###### STEP 0: Plot Overall Algorithm Performance - Cut 1 #####
###############################################################

suppressPackageStartupMessages({
  source("utils/validation_plots.R")
  require(ggplot2)
  require(tidyverse)
  require(splendid) # This is a github Package
  require(caret)
})


# top algos overall NO threshold
ranked.algo <- top_algo_plot(
  dir = "array_classifier/2_post_processing/data/", 
  threshold = FALSE, 
  plot.title = "Algorithm Performance Ranking", 
  print = FALSE, save = FALSE
  )

ggsave(
  ranked.algo, 
  filename = "array_classifier/2_post_processing/outputs/plots/ranked_algorithms_noThreshold.png", 
  width = 16, height = 9
  )

# top algos overall threshold
ranked.algo.threshold <- top_algo_plot(
  dir = "array_classifier/2_post_processing/data/", 
  threshold = TRUE, 
  plot.title = "Algorithm Performance Ranking with Threshold", 
  print = FALSE, save = FALSE
  )

ggsave(
  ranked.algo.threshold, 
  filename = "array_classifier/2_post_processing/outputs/plots/ranked_algorithms_threshold.png", 
  width = 16, height = 9
  )

# top two NO threshold
top.algs <- sup_plots(
  dir = "array_classifier/2_post_processing/data/", 
  plot.title = "Top Supervised Alg Evaluation", 
  threshold = FALSE, print = FALSE, save = FALSE, 
  algs = c("mlr_ridge", "mlr_lasso")
  )

ggsave(
  top.algs[[1]], 
  filename = "array_classifier/2_post_processing/outputs/plots/top2_algorithms_byClass_noThreshold.png", 
  width = 16, height = 9
  )

ggsave(
  top.algs[[2]], 
  filename = "array_classifier/2_post_processing/outputs/plots/top2_algorithms_noThreshold.png", 
  width = 16, height = 9
  )

# top two threshold
top.algs.threshold <- sup_plots(
  dir = "array_classifier/2_post_processing/data/", 
  plot.title = "Top Supervised Alg Evaluation with Threshold", 
  threshold = TRUE, print = FALSE, save = FALSE, 
  algs = c("adaboost", "rf")
  )

ggsave(
  top.algs.threshold[[1]], 
  filename = "array_classifier/2_post_processing/outputs/plots/top2_algorithms_byClass_threshold.png", 
  width = 16, height = 9
  )

ggsave(
  top.algs.threshold[[2]], 
  filename = "array_classifier/2_post_processing/outputs/plots/top2_algorithms_threshold.png", 
  width = 16, height = 9
  )

