source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))

cli::cat_line("Summarizing ", study, " bootstrap frequencies")
fnames <- list.files(
  path = file.path(outputDir, "gene_selection", "boot_freq"),
  pattern = paste0(study, "_freq_(", paste(algs, collapse = "|"), ")"),
  full.names = TRUE
)
sum_freq <- fnames %>%
  purrr::map(readr::read_csv, col_types = readr::cols()) %>%
  purrr::reduce(dplyr::inner_join, by = "genes") %>%
  dplyr::arrange(dplyr::desc(rfFreq))
readr::write_csv(sum_freq, file.path(outputDir, "gene_selection", "sum_freq",
                                     paste0(study, "_sum_freq.csv")))
cli::cat_line("Completed ", study, " summary frequencies")
