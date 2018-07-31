suppressPackageStartupMessages(library(tidyverse))

# input path from command line
args <- commandArgs(TRUE)

# extract file run names
files <- list.files(path = args[1]) %>%
  strsplit("_", fixed = TRUE) %>%
  map_chr(1) %>%
  tibble(
    algoRuns = .,
    runID = str_extract(algoRuns, "[0-9]+"),
    algo = str_extract(algoRuns, "[aA-zZ]+")
  )

# summarise number of reps per algo
(files.summarise <- count(files, algo))
