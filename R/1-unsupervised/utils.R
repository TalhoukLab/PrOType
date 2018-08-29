source(here::here("assets/utils.R"))

# Default hc from diceR but with data matrix as input
hc <- function(x, k, method = "average") {
  diceR:::hc(stats::dist(x), k = k, method = method)
}
