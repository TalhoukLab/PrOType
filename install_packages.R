pkg_list <- c("tidyverse", "crayon", "here", "diceR") # however you want to get this

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr"), warning = function(w) stop(w))
}

for (package in pkg_list) {
  withCallingHandlers(install.packages(package), warning = function(w) stop(w))
}

devtools::install_github("AlineTalhouk/splendid")
