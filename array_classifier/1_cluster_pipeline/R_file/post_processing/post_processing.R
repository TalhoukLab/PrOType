trainSet <- "ov.afc1_xpn"
testSet <- "ov.afc2_xpn"
datasets <- c("ov.afc1_xpn", "ov.afc2_xpn")
outputDir <- "/outputs/"
data_dir <- "assets/data"
cli::cat_line("Starting step 1")
source("array_classifier/2_post_processing/1-EvaluateBatchEffects.R")
cli::cat_line("Starting step 2")
source("array_classifier/2_post_processing/2-internal_validity_plots.R")
cli::cat_line("Starting step 3")
source("array_classifier/2_post_processing/3-predict_C2.R")
cli::cat_line("Starting step 4")
source("array_classifier/2_post_processing/4-ProbeMapping_C2v2.R")
cli::cat_line("Starting step 5")
source("array_classifier/2_post_processing/5-MappingSignatures_c2.R")
cli::cat_line("Validating Results")
source("array_classifier/2_post_processing/validate_baseline_results.R)
