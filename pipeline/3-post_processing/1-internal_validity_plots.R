# Internal Validity Plots -------------------------------------------------

# Load utility functions
source(here::here("pipeline/3-post_processing/utils.R"))

# Store common plotting arguments
plot_args <- list(dir = outputDir, datasets = datasets, print = FALSE)

cli::cat_line("Plotting all ranked algorithms")
p1 <- purrr::invoke(all_algo_plot, plot_args, threshold = FALSE)
p2 <- purrr::invoke(all_algo_plot, plot_args, threshold = TRUE)

cli::cat_line("Plotting top 2 algorithms with and without threshold")
p3 <- purrr::invoke(top2_algo_plot, plot_args)

cli::cat_line("Plotting algorithm by internal validity index heatmap")
p4 <- purrr::walk(datasets, algii_heatmap, dir = outputDir)
