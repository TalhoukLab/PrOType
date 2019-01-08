rmarkdown::render(
  input = "pipeline/7-supplementary/Supp_A.Rmd",
  output_dir = file.path(outputDir, "supplementary/reports"),
  params = list(outputDir = outputDir)
)
