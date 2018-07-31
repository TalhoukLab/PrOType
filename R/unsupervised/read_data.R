# This file is called in the beginning and not processed as part of the Q
# Inputs: dpath, ndat, pr, and datadir
# Outputs: saved tdat, cdat

# Load data
fname <- paste0(dpath, ndat, ".RData")
tdat_name <- file.path(datadir, paste0("tdat_", ndat, ".rds"))
cdat_name <- file.path(datadir, paste0("cdat_", ndat, ".rds"))

# Save as RDS objects
if (!dir.exists(datadir)) {
  cat("ERROR: directory does not exist:", datadir)
  quit(status=1)
}

cli::cat_line("Processing tdat")
if (!file.exists(fname)) {
  cat("ERROR: file not found:", fname)
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
