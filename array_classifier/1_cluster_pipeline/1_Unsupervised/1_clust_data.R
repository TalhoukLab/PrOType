# input fixed cdat,sfdir,r,k
# input variable algs, seed (s)

outputFile <- paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat)

cli::cat_line("Checking previous input")
if (file.exists(paste0(outputFile, ".rds")) && !shouldCompute) {
  cli::cat_line("File already exists, skipping")
  quit(status = 0)
}

cc_args <- tibble::lst(
  data = cdat,
  nk = k,
  reps = 1,
  prep.data = "none",
  seed.data = s,
  file.name = outputFile
)

ssclust <- switch(
  algs,
  nmfbrunet = purrr::invoke(diceR::consensus_cluster, cc_args,
                            algorithms = "nmf", nmf.method = "brunet"),
  nmflee = purrr::invoke(diceR::consensus_cluster, cc_args,
                         algorithms = "nmf", nmf.method = "lee"),
  distalgs = purrr::invoke(diceR::consensus_cluster, cc_args,
                           algorithms = c("km", "pam"),
                           distance = c("eucl", "spear", "manh")),
  rest = purrr::invoke(diceR::consensus_cluster, cc_args,
                       algorithms = "block")
)
