# CRAN packages
pkg_list <- c("tidyverse", "devtools", "cli", "here", "diceR", "packrat",
              "magrittr", "mclust", "glmnet", "caret", "pheatmap", "epiR",
              "ggforce", "RColorBrewer", "gplots", "plotly")

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr"), warning = function(w) stop(w))
}

for (package in pkg_list) {
  if (!package %in% installed.packages()) {
    withCallingHandlers(install.packages(package), warning = function(w) stop(w))
  }
}

# GitHub devel packages
devtools::install_git("git://github.com/AlineTalhouk/splendid.git")
devtools::install_git("git://github.com/r-lib/processx.git")

# Bioconductor packages
source("https://bioconductor.org/biocLite.R")
biocLite("pvca")
biocLite("AnnotationDbi")
bio