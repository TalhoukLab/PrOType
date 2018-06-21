for (dataset in unlist(strsplit("ov.afc1_xpn ov.afc1_cbt", " "))) {
cat("Starting Part 0\n")
cat(dataset, "\n")
output_dir <- "/outputs//iv_summary"
#source("array_classifier/2_post_processing/step0_InternalValidation.R")
}
datasets <- unlist(strsplit("ov.afc1_xpn ov.afc1_cbt", " "))
algs <- purrr::set_names(unlist(strsplit("adaboost rf mlr_ridge mlr_lasso", " ")))
output_dir <- "/outputs/"
cat("Starting Part 1\n")
#source("array_classifier/2_post_processing/step1_BuildFinalModel.R")
cat("Starting Part 2\n")
source("array_classifier/2_post_processing/step2_ArrayValidation.R")
