# input fixed cdat,sfdir,r,k
# input variable algs, seed (s)

outputFile <- paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat)

cli::cat_line("Checking previous Input\n")
if (file.exists(paste0(outputFile, ".rds")) && !shouldCompute) {
      cli::cat_line("File already exists, skipping.\n")
      quit(status = 0)
}

library(diceR)
r<-1

ssclust <- switch(algs,
nmfbrunet = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "brunet",
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    nmflee = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "lee",
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    distalgs = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("km", "pam"),
        distance = c("eucl", "spear", "manh"),
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    },
    rest = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("block"), #,"ap"), #,"gmm"),
        prep.data = "none", seed.data = s,
        file.name = outputFile)
    }
)
