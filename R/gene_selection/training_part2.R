# Load Parameters----
trainModel <- 1
predOverlap <- 1
predCut2 <- 1
refineModel <- 1
top_overall <- 60
n_min <- 40
alg <- "rf"

# Load packages----
library(here)
library(tidyverse)
suppressPackageStartupMessages({
  library(magrittr)
  library(splendid)
  library(caret)
  library(cli)
})

GS_training_dir <- "R/gene_selection/scripts"
GS_training_files <- c(
  "utils.R",
  "define.R",
  "train.R",
  "bootstrap.R",
  "evaluate.R",
  "summary.R",
  "analysis.R"
)
walk(here(GS_training_dir, GS_training_files), source)

c1_path <- file.path(output_dir, "output/TrainingC1")
plot_dir <- file.path(output_dir, "plots")

# Load data----
# Load the NanoString data and select cut
nsdat <- load_nanostring()

# Load prediction labels
pred_labs <- load_prediction_labels(nsdat)
preds_new <- pred_labs$preds_new

# Compute consensus
train <- define_batch(preds_new, nsdat, batch = "b1")
train_dat <- train$dat
train_lab <- train$lab
overlap <- define_overlap(preds_new, nsdat)
overlap_dat <- overlap$dat
overlap_lab <- overlap$lab

# Get studies
studies <- unique(train_dat$site)

# Define first test set (cut2 excluding overlap)
test1 <- define_batch(preds_new, nsdat, batch = "b2")
test1_lab <- test1$lab
test1_dat <- test1$dat

# Get studies from test set
study_test1 <- unique(test1_dat$site)


# Run the analysis----

# Determine Top Genes ----
cli::cat_line("Determine Top Genes")
fnames <- list.files(
  path = file.path(output_dir, "output/sumFreq"),
  pattern = "sumFreq",
  full.names = TRUE
)

# For each study, we arrange by the most included rf gene proportion and we
# return the top 70 genes
site_names <- table(train_dat$site) %>% paste0(names(.), .)
rf70 <- fnames %>%
  set_names(site_names) %>%
  map(~ read_csv(., col_types = cols()) %>%
        arrange(desc(`rfFreq`)) %>%
        pull(genes) %>%
        head(70)
  )

# Load the average frequency across all studies
sumFreq <- read.csv(file.path(output_dir, "output/sumFreq/overallFreqs.csv"),
                    stringsAsFactors = FALSE) %>%
  arrange(desc(`rfFreq`))

# We consider the union of the genes across studies
genes70 <- Reduce(union, rf70)

geneRanks_rf <- rf70 %>%
  map(match, x = genes70, nomatch = 80) %>%
  as.data.frame() %>%
  set_rownames(genes70)

# Generate annotations for rows
legend_title <- paste0("InTop", top_overall)
annotation_row <-
  tibble(!!legend_title := ifelse(genes70 %in% head(sumFreq$genes, top_overall),
                                  "yes", "no")) %>%
  as.data.frame() %>%
  set_rownames(genes70)

cli::cat_line("Plotting heatmap of top 70 rf genes")

pdf(file.path(plot_dir, "Heatmap_rf_top70.pdf"))
pheatmap::pheatmap(
  mat = geneRanks_rf,
  annotation_row = annotation_row,
  fontsize_row = 5,
  main = "Random Forest"
)
dev.off()

# Train models with different number of genes----
if (trainModel) {
  cli::cat_line("Train Model with top ", n_min, "-70 rf genes")
  x <- sl_data(train_dat)
  y <- sl_class(train_lab, x)
  genes <- get_genes(train_dat)

  # Sequence from n_min to length(genes70) adding one gene at a time
  ng <- seq(n_min, length(genes70), 1) %>% set_names(paste0("ng_", .))
  rf_genes <- make.names(sumFreq$genes)
  l <- ng %>%
    map(head, x = rf_genes) %>%
    map(~ splendid::classification(x[, .], y, algorithms = alg, seed_alg = 2018))
  write_rds(l, file.path(c1_path, "rf_allc1.rds"))
}

# Predict overlapping samples----
if (predOverlap) {
  cli::cat_line("Predict overlapping samples")
  fnames <- list.files(c1_path, pattern = "all", full.names = TRUE)
  x.new <- sl_data(overlap_dat)
  y.new <- sl_class(overlap_lab, x.new)

  mod.pred <- fnames %>%
    readRDS() %>%
    purrr::map(~ splendid::prediction(., x.new[, which_genes(., alg)], y.new) %>%
                 set_names(rownames(x.new))) %>%
    set_names(paste(alg, names(.), sep = "_"))

  write_rds(mod.pred, file.path(c1_path, "overlap_pred.rds"))
}

# Evaluate the predictions on cut2 samples----
if (predCut2) {
  cli::cat_line("Predict cut2 samples")
  fnames <- list.files(c1_path, pattern = "all", full.names = TRUE)
  x.new <- sl_data(test1_dat)
  y.new <- sl_class(test1_lab, x.new)

  mod.pred <- fnames %>%
    readRDS() %>%
    purrr::map(~ splendid::prediction(., x.new[, which_genes(., alg)], y.new) %>%
                 set_names(rownames(x.new))) %>%
    set_names(paste(alg, names(.), sep = "_"))

  write_rds(mod.pred, file.path(c1_path, "cut2_pred.rds"))

  rf <- mod.pred %>%
    as.data.frame(., col.names = names(.)) %>%
    tibble::rownames_to_column(var = "ottaID") %>%
    inner_join(test1_lab, ., by = "ottaID")

  res_pred <- rf %>%
    select(matches("ng")) %>%
    map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    map_dbl(pluck, "overall", "Accuracy")

  pdf(file.path(plot_dir, "Accuracy_cut2.pdf"))
  plot(
    seq(n_min, length(genes70), 1),
    res_pred,
    ylim = c(0.9, 1),
    xlab = "Number of Genes",
    ylab = "Prediction Accuracy",
    main = "Prediction Accuracy on Cut 2\nas a Function of Number of Genes",
    pch = 19
  )
  dev.off()

  byclass.res <- rf %>%
    select(matches("ng")) %>%
    map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    map(~ .[["byClass"]][, "F1"]) %>%
    transpose() %>%
    map(~ list(F1_score = unlist(.))) %>%
    set_names(gsub("\\.", "-", map_chr(strsplit(names(.), split = " "), 2)))

  loso_plot(
    file_name = file.path(plot_dir, "F1score_cut2_byclass.pdf"),
    data = byclass.res,
    group = "F1_score",
    main = "F1 Score by class\nby # of genes",
    xbreaks = seq(n_min, length(genes70), 1),
    ylim = c(0.8, 1),
    col_alg = c("red", "forestgreen", "darkslategray3", "darkviolet"),
    show_max = FALSE,
    legend_border = FALSE
  )
}

# Refine model by removing problematic genes ----
if (refineModel) {
  cli::cat_line("Refine model")
  # select genes that do not perform well on array
  x <- sl_data(train_dat)
  y <- sl_class(train_lab, x)

  grm1 <- "CTHRC1"
  grm2 <- c("CTHRC1", "CD68", "CTLA4", "ABCC3", "TLR4", "GFPT2")

  # Remove genes
  topGenes_1 <- make.names(head(sumFreq$genes, top_overall))
  topGenes_2 <- setdiff(topGenes_1, grm1)
  topGenes_3 <- setdiff(topGenes_1, grm2)
  topGenes <- list(topGenes_1, topGenes_2, topGenes_3)

  fits <- topGenes %>%
    map(~ splendid::classification(x[, .], y, algorithms = alg, seed_alg = 2018))
  write_rds(fits, file.path(c1_path, "rf_alternate_c1.rds"))

  # test alternate models on cut 2
  x.new <- sl_data(test1_dat)
  y.new <- sl_class(test1_lab, x.new)

  mod.pred <- fits %>%
    map(~ splendid::prediction(., x.new[, which_genes(., alg)], y.new) %>%
          set_names(rownames(x.new))) %>%
    set_names(c("fit_0", "fit_1", "fit_2"))
  write_rds(mod.pred, file.path(c1_path, "cut2_pred_alternates_list.rds"))

  rf <- mod.pred %>%
    as.data.frame(., col.names = names(.)) %>%
    tibble::rownames_to_column("ottaID") %>%
    inner_join(test1_lab, ., by = "ottaID")

  res_pred <- rf %>%
    select(matches("fit")) %>%
    map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    map_dbl(pluck, "overall", "Accuracy")

  pdf(file.path(output_dir, "plots/Accuracy_alternates.pdf"))
  plot(
    1:3,
    res_pred,
    ylim = c(0.9, 1),
    xlab = "Number of Genes",
    ylab = "Prediction Accuracy",
    main = "Prediction Accuracy on Cut 2 \n as a Function of Number of Genes",
    pch = 19
  )
  dev.off()

  byclass.res <- rf %>%
    select(matches("fit")) %>%
    map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    map(~ .[["byClass"]][, "F1"]) %>%
    transpose() %>%
    map(~ list(F1_score = unlist(.))) %>%
    set_names(gsub("\\.", "-", map_chr(strsplit(names(.), split = " "), 2)))

  loso_plot(
    file_name = file.path(plot_dir, "F1score_alternates.pdf"),
    data = byclass.res,
    group = "F1_score",
    main = "F1 Score by class\nby # of genes",
    xbreaks = seq_len(3),
    ylim = c(0.8, 1),
    col_alg = c("red", "forestgreen", "darkslategray3", "darkviolet"),
    show_max = FALSE,
    legend_border = FALSE
  )

  colnames(rf)[2] <- "Label"
  write_csv(rf, file.path(c1_path, "cut2_pred_alternates_table.csv"))
}
