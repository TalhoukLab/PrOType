# This file is called in the beginning and not processed as part of the Q
# Inputs: dpath, ndat, pr, and datadir
# Outputs: saved tdat, cdat

library(diceR)
library(tidyverse)

# Load data
fname <- paste0(dpath, ndat, ".RData")
tdat <- t(get(load(fname)))

# Center and scale
cdat <- switch(
  pr,
  cs = prepare_data(tdat, scale = TRUE, type = "conventional"),
  rs = prepare_data(tdat, scale = TRUE, type = "robust"),
  ns = prepare_data(tdat, scale = FALSE)
)

# Save as RDS objects
saveRDS(tdat, paste0(datadir, "/tdat_", ndat, ".rds"))
saveRDS(cdat, paste0(datadir, "/cdat_", ndat, ".rds"))
