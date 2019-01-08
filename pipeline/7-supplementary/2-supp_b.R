rmarkdown::render(
  input = "pipeline/7-supplementary/Supp_B.Rmd",
  output_dir = file.path(outputDir, "supplementary/reports"),
  params = list(outputDir = outputDir)
)
