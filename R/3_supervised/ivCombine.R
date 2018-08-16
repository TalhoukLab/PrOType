# command line arguments
library(magrittr)

x <- list.files(file.path(outputDir, "supervised", "summary"),
                recursive = TRUE,
                full.names = TRUE,
                pattern = "supervised_ov.*") %>%
  purrr::map(~ readRDS(.)) %>%
  data.table::rbindlist()

readr::write_rds(x, file.path(outputDir, "supervised", "summary", "supervised_COMBINED.rds"))
x1 <- subset(x, x$measure == "accuracy")
x2 <- x1[order(x1$percentile_50, decreasing = TRUE), ]
print(x2[!duplicated(x2[,c(`percentile_50`)]),])
