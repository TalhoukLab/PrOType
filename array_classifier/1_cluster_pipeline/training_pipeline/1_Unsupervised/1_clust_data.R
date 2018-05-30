# input fixed cdat,sfdir,r,k
# input variable algs, seed (s)

library(diceR)

k <- 4
r <- 1

ssclust <- switch(algs,
nmfbrunet = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "brunet",
        prep.data = "none", seed.data = s,
        file.name = paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat))
    },
    nmflee = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = "nmf", nmf.method = "lee",
        prep.data = "none", seed.data = s,
        file.name = paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat))
    },
    distalgs = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("km", "pam"),
        distance = c("eucl", "spear", "manh"),
        prep.data = "none", seed.data = s,
        file.name = paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat))
    },
    rest = {
        consensus_cluster(data = cdat, nk = k, reps = r,
        algorithms = c("block"), #,"ap"), #,"gmm"),
        prep.data = "none", seed.data = s,
        file.name = paste0(sfdir, "/rds_out_", ndat, "/", algs, s, "_", ndat))
    }
)
