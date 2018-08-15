args <- commandArgs(trailingOnly = TRUE)

output_file <- args[1]
source("R/6_gene_selection/scripts/define.R")
source("R/6_gene_selection/scripts/utils.R")

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat)
train_dat <- train$dat
train_lab <- train$lab

# Get studies
studies <- unique(train_dat$site)
print(studies)

write(studies, sep = "\n", file = output_file)
#readr::read_csv(paste0(studies, "\n")) %>% readr::write_csv(output_file)
