# This file is called in the beginning and not processed as part of the Q
# Inputs: dpath, ndat, pr, and datadir
# Outputs: saved tdat, cdat

# Load data
fname <- file.path(dpath, paste0(dataset, ".RData"))
tdat_name <- file.path(datadir, paste0("tdat_", dataset, ".rds"))
cdat_name <- file.path(datadir, paste0("cdat_", dataset, ".rds"))

# Save as RDS objects
if (!dir.exists(datadir)) {
  cli::cat_line("ERROR: directory does not exist:", datadir)
  quit(status=1)
}

cli::cat_line("Processing tdat")
if (!file.exists(fname)) {
  cli::cat_line("ERROR: file not found:", fname)
  stop(0)
}
if (file.exists(tdat_name)) {
  cli::cat_line("tdat already exists.  Skipping.")
} else {
  tdat <- t(get(load(fname)))
  saveRDS(tdat, tdat_name)
}

cli::cat_line("Processing cdat")
if (file.exists(cdat_name)) {
  cli::cat_line("cdat already exists. Skipping.")
} else {
  # Center and scale
  cdat <- switch(
    pr,
    cs = diceR::prepare_data(tdat, scale = TRUE, type = "conventional"),
    rs = diceR::prepare_data(tdat, scale = TRUE, type = "robust"),
    ns = diceR::prepare_data(tdat, scale = FALSE)
  )

  saveRDS(cdat, cdat_name)
}
