trainSet <- "ov.afc1_xpn"
trainSet2 <- "ov.afc1_cbt"
testSet <- "ov.afc2_xpn"
datasets <- c("ov.afc1_xpn", "ov.afc1_cbt")
outputDir <- "/outputs/"
dataDir <- "assets/data"
cli::cat_line("Validating Results")
source("array_classifier/2_post_processing/0-validate_baseline_results.R")
cli::cat_line("Post-processing step 1")
source("array_classifier/2_post_processing/1-evaluate_batch_effects.R")
cli::cat_line("Post-processing step 2")
source("array_classifier/2_post_processing/2-internal_validity_plots.R")
cli::cat_line("Post-processing step 3")
source("array_classifier/2_post_processing/3-predict_C2.R")
cli::cat_line("Post-processing step 4")
source("array_classifier/2_post_processing/4-probe_mapping_C2v2.R")
cli::cat_line("Post-processing step 5")
source("array_classifier/2_post_processing/5-mapping_signatures_C2.R")
