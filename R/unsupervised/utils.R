source(here::here("assets/utils.R"))

hc <- function(x, k, method = "average") {
  as.integer(stats::cutree(stats::hclust(stats::dist(x), method = method), k))
}
