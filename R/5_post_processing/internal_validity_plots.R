# Internal Validity Plots -------------------------------------------------

# Load utility functions
source(here::here("R/5_post_processing/utils/utils.R"))

# Store common plotting arguments
plot_args <- list(dir = outputDir, datasets = datasets, print = FALSE)

cli::cat_line("Plotting all ranked algorithms")
purrr::invoke(all_algo_plot, plot_args, threshold = FALSE)
purrr::invoke(all_algo_plot, plot_args, threshold = TRUE)

cli::cat_line("Plotting top 2 algorithms with and without threshold")
purrr::invoke(top2_algo_plot, plot_args)

cli::cat_line("Plotting algorithm by internal validity index heatmap")
purrr::walk(datasets, algii_heatmap, dir = outputDir)
