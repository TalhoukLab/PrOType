pkg_list <- c("crayon", "here", "diceR") # however you want to get this

compareVersion("3.3.1", "3.5.0")
compareVersion("3.5.0", "3.5.0")
compareVersion("3.5.0", "3.3.1")

if (compareVersion("3.5.0", as.character(getRversion())) == 1) {
  withCallingHandlers(install.packages("dplyr", repos="https://mirror.its.sfu.ca/mirror/CRAN/"), warning = function(w) stop(w))
}

withCallingHandlers(install.packages("crayon", repos="https://mirror.its.sfu.ca/mirror/CRAN/"), warning = function(w) stop(w))
withCallingHandlers(install.packages("here", repos="https://mirror.its.sfu.ca/mirror/CRAN/"), warning = function(w) stop(w))
withCallingHandlers(install.packages("diceR", repos="http://cran.stat.sfu.ca"), warning = function(w) stop(w))



# for (pkg in pkg_list) {
#     withCallingHandlers(install.packages(pkg), #, repos="https://mirror.its.sfu.ca/mirror/CRAN/"),
#                     warning = function(w) stop(w))
# }

#devtools::install_github("AlineTalhouk/diceR")
