
check_dataframes <- function(path1, path2, df_set, dataset) {
  if (file.exists(path1) && file.exists(path2)) {
    a1 <- readRDS(path1)
    a2 <- readRDS(path2)

    if (nrow(dplyr::anti_join(a1, a2)) == 0) {
      cli::cat_line(df_set, " for dataset: ", dataset, " identical")
    } else {
      cli::cat_line(df_set, " for dataset: ", dataset, " DIFFERENT")
    }
  } else if (!file.exists(path1)) {
    cli::cat_line("Can't check dataset: ", dataset, " missing reference ", df_set)
  } else if (!file.exists(path2)) {
    cli::cat_line("Can't check dataset: ", dataset, " missing computed ii ", df_set)
  }
}

# Check for reference object
for (dataset in datasets) {
  reference_ii <- file.path(data_dir, "references", dataset, "ii.rds")
  computed_ii <- file.path(output_dir, "iv_summary", paste0("data_pr_", dataset), paste0("ii_", dataset, ".rds"))

  reference_iv_summary <- file.path(data_dir, "references", dataset, "iv_summary.rds")
  computed_iv_summary <- file.path(output_dir, "iv_summary", paste0("data_pr_", dataset), paste0("iv_summary_", dataset, ".rds"))

  check_dataframes(reference_ii, computed_ii, "ii", dataset)
  check_dataframes(reference_iv_summary, computed_iv_summary, "iv_summary", dataset)
}
