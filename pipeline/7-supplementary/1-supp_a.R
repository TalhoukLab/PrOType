# rmarkdown::render(
#   input = "pipeline/7-supplementary/Supp_A.Rmd",
#   output_dir = file.path(outputDir, "supplementary/reports"),
#   params = list(outputDir = outputDir)
# )
file_name <- "pipeline/7-supplementary/Supp_A"
knitr::purl(input = paste0(file_name, ".Rmd"),
            output = paste0(file_name, ".R"))
options(warn = -1)
source(paste0(file_name, ".R"))
options(warn = 0)
