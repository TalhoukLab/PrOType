pkg_list <- c("tidyverse", "cli", "here", "diceR", "packrat", "magrittr", "mclust",
              "glmnet", "caret", "pheatmap", "epiR", "ggforce",
              "RColorBrewer", "gplots", "plotly")

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr"), warning = function(w) stop(w))
}

install.packages("devtools")
devtools::install_git("git://github.com/r-lib/processx.git")

for (package in pkg_list) {
  withCallingHandlers(install.packages(package), warning = function(w) stop(w))
}

devtools::install_github("AlineTalhouk/splendid")

source("https://bioconductor.org/biocLite.R")
biocLite("pvca")
biocLite("AnnotationDbi")
biocLite("hgu133plus2.db")
biocLite("hgug4112a.db")
