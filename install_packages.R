pkg_list <- c("tidyverse", "crayon", "here", "diceR", "packrat", "cli", "magrittr",
              "devtools", "glmnet", "caret", "pheatmap", "epiR", "ggforce",
              "pvca", "RColorBrewer", "AnnotationDbi", "gplots")

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr"), warning = function(w) stop(w))
}

for (package in pkg_list) {
  withCallingHandlers(install.packages(package), warning = function(w) stop(w))
}

devtools::install_github("AlineTalhouk/splendid")
