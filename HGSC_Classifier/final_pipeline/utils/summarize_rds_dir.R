suppressPackageStartupMessages({
  require(tidyverse)
  require(stringr)
})

# input path from command line
args <- commandArgs(TRUE)

# extract file run names
files <- list.files(path=args[1]) %>%
  strsplit("_", fixed = TRUE) %>%
  lapply('[', 1) %>%
  unlist() %>%
  data.frame(algoRuns = .) %>%
  mutate(runID = stringr::str_extract(algoRuns, "[0-9]+"),
         algo = stringr::str_extract(algoRuns, "[aA-zZ]+"))

# summarise number of reps per algo
files.summarise <- files %>%
  group_by(algo) %>%
  summarise(count = length(algo))

print(files.summarise)
