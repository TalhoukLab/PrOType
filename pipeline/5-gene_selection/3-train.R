source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))

cli::cat_line("Getting Data")
x <- sl_data(train_dat, study, "training")
y <- sl_class(train_lab, x)
seed_alg <- 2018

sum_freq <- readr::read_csv(
  file = file.path(outputDir, "gene_selection", "sum_freq",
                   paste0(study, "_sum_freq.csv")),
  col_types = readr::cols()
)

cli::cat_line("Running Final Training")
classify_top_genes(x,
                   y,
                   sum_freq,
                   outputDir,
                   study,
                   seed_alg,
                   alg,
                   shouldCompute = shouldCompute)
cli::cat_line("Finished processing bootstrap")
