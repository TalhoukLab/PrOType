pkg_list <- c("tidyverse", "crayon", "here", "diceR", "packrat", "cli", "magrittr",
              "devtools", "glmnet", "caret", "pheatmap", "epiR", "ggforce",
              "RColorBrewer", "gplots", "plotly")

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr"), warning = function(w) stop(w))
}

for (package in pkg_list) {
  withCallingHandlers(install.packages(package), warning = function(w) stop(w))
}

devtools::install_github("AlineTalhouk/splendid")

source("https://bioconductor.org/biocLite.R")
biocLite("pvca")
biocLite("AnnotationDbi")
biocLite("hgu133plus2.db")
biocLite("hgug4112a.db")
