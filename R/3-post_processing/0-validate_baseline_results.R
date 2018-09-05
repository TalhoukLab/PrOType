# Compare reference outputs -----------------------------------------------

# Compare computed outputs against reference outputs from lockdown run
for (dataset in datasets) {
  reference_ii <- file.path(dataDir, "references", dataset, "ii.rds")
  computed_ii <- file.path(outputDir, "unsupervised", "final", dataset, paste0("ii_", dataset, ".rds"))

  reference_iv_summary <- file.path(dataDir, "references", dataset, "iv_summary.rds")
  computed_iv_summary <- file.path(outputDir, "supervised", "summary", dataset, paste0("iv_summary_", dataset, ".rds"))

  check_dataframes(reference_ii, computed_ii, "ii", dataset)
  check_dataframes(reference_iv_summary, computed_iv_summary, "iv_summary", dataset)
}
