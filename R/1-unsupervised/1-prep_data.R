# This file is called in the beginning and not processed as part of the queue
# Inputs: pr, dataset, datadir, dpath
# Outputs: tdat, cdat

# Data file names
fname <- file.path(dpath, paste0(dataset, ".RData"))
tdat_name <- file.path(datadir, paste0("tdat_", dataset, ".rds"))
cdat_name <- file.path(datadir, paste0("cdat_", dataset, ".rds"))

# Check if datadir and fname exist
if (!dir.exists(datadir)) {
  stop("Directory ", datadir, " does not exist")
  q("no", 1, FALSE)
}
if (!file.exists(fname)) {
  stop("File ", fname, "does not exist")
  q("no", 1, FALSE)
}

# Process if not already exists and save as rds objects
cli::cat_line("Processing transposed data")
if (file.exists(tdat_name)) {
  cli::cat_line("Transposed data already exists, skipping...")
} else {
  tdat <- t(get(load(fname)))
  saveRDS(tdat, tdat_name)
}

cli::cat_line("Processing scaled data")
if (file.exists(cdat_name)) {
  cli::cat_line("Scaled data already exists, skipping...")
} else {
  cdat <- switch(
    pr,
    cs = diceR::prepare_data(tdat, scale = TRUE, type = "conventional"),
    rs = diceR::prepare_data(tdat, scale = TRUE, type = "robust"),
    ns = diceR::prepare_data(tdat, scale = FALSE)
  )
  saveRDS(cdat, cdat_name)
}
