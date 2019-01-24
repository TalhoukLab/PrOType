# rmarkdown::render(
#   input = "pipeline/7-supplementary/Supp_B.Rmd",
#   output_dir = file.path(outputDir, "supplementary/reports"),
#   params = list(outputDir = outputDir)
# )
file_name <- "pipeline/7-supplementary/Supp_B"
knitr::purl(input = paste0(file_name, ".Rmd"),
            output = paste0(file_name, ".R"))
source(paste0(file_name, ".R"))
