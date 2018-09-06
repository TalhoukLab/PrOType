# Extract studies from batch 1
b1 <- readr::read_csv(
  file = here::here(
    "assets/data/nstring",
    "nanostring_classifier_data_batch1_20170217_updated.csv"
  ),
  col_types = readr::cols()
)
cat(unique(b1[["site"]]), "\n")
