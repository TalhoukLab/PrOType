source(here::here("pipeline/5-gene_selection/0-gs_setup.R"))

# Load Parameters
trainModel <- 1
predOverlap <- 1
predCut2 <- 1
refineModel <- 1
top_study <- 70
top_overall <- 60
n_min <- 40
alg <- "rf"
seed <- 2018
c1_path <- file.path(outputDir, "gene_selection", "retrain")
plot_dir <- file.path(outputDir, "gene_selection", "plots")

# Compute overlap
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

# Load the average frequency across all studies
sumFreq <-
  file.path(outputDir, "gene_selection", "sum_freq", "overall_freq.csv") %>%
  readr::read_csv(col_types = readr::cols()) %>%
  dplyr::arrange(dplyr::desc(rfFreq), dplyr::desc(lassoFreq))


# Run the analysis----

# Determine Top Genes ----
cli::cat_line("Determine Top Genes")
fnames <- list.files(
  path = file.path(outputDir, "gene_selection", "sum_freq"),
  pattern = "sum_freq",
  full.names = TRUE
)
site_names <- table(train_dat$site) %>% paste0(names(.), .)

# For each study, we arrange by the most included rf gene proportion and we
# return the top `top_study` genes
rf_top <- fnames %>%
  purrr::set_names(site_names) %>%
  purrr::map(~ {
    readr::read_csv(file = ., col_types = readr::cols()) %>%
      dplyr::arrange(dplyr::desc(rfFreq)) %>%
      dplyr::pull(genes) %>%
      head(top_study)
  })

# We consider the union of the genes across studies
rf_union <- Reduce(union, rf_top)

# Train models with different number of genes----
if (trainModel) {
  cli::cat_line("Train Model with top ", n_min, "-", length(rf_union), " rf genes")
  x <- sl_data(train_dat)
  y <- sl_class(train_lab, x)
  genes <- get_genes(train_dat)

  # Sequence from n_min to length(rf_union) adding one gene at a time
  ng <- seq(n_min, length(rf_union), 1) %>% magrittr::set_names(paste0("ng_", .))
  rf_genes <- make.names(sumFreq$genes)
  l <- ng %>%
    purrr::map(head, x = rf_genes) %>%
    purrr::map(~ splendid::classification(x[, .], y, algorithms = alg, seed_alg = seed))
  saveRDS(l, file.path(c1_path, "rf_allc1.rds"))
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
                 magrittr::set_names(rownames(x.new))) %>%
    purrr::set_names(paste(alg, names(.), sep = "_"))

  saveRDS(mod.pred, file.path(c1_path, "overlap_pred.rds"))
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
                 magrittr::set_names(rownames(x.new))) %>%
    purrr::set_names(paste(alg, names(.), sep = "_"))

  saveRDS(mod.pred, file.path(c1_path, "cut2_pred.rds"))

  rf <- mod.pred %>%
    as.data.frame(., col.names = names(.)) %>%
    tibble::rownames_to_column(var = "ottaID") %>%
    dplyr::inner_join(test1_lab, ., by = "ottaID")

  res_pred <- rf %>%
    dplyr::select(dplyr::matches("ng")) %>%
    purrr::map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    purrr::map_dbl(purrr::pluck, "overall", "Accuracy")

  pdf(file.path(plot_dir, "Accuracy_cut2.pdf"))
  plot(
    seq(n_min, length(rf_union), 1),
    res_pred,
    ylim = c(0.9, 1),
    xlab = "Number of Genes",
    ylab = "Prediction Accuracy",
    main = "Prediction Accuracy on Cut 2\nas a Function of Number of Genes",
    pch = 19
  )
  legend("bottomright",
         pch = 19,
         c(paste0("Min = ", min(res_pred), " #", which.min(res_pred)),
           paste0("Max = ", max(res_pred), " #", which.max(res_pred))),
         bty = "n")
  dev.off()

  byclass.res <- rf %>%
    dplyr::select(dplyr::matches("ng")) %>%
    purrr::map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    purrr::map(~ .[["byClass"]][, "F1"]) %>%
    purrr::transpose() %>%
    purrr::map(~ list(F1_score = unlist(.))) %>%
    purrr::set_names(purrr::map_chr(strsplit(names(.), split = " "), 2))

  pdf(file.path(plot_dir, "F1_cut2_byclass.pdf"))
  loso_plot(
    data = byclass.res,
    group = "F1_score",
    main = "F1 Score by class\nby # of genes",
    xbreaks = seq(n_min, length(rf_union), 1),
    ylim = c(0.8, 1),
    col_alg = c("red", "forestgreen", "darkslategray3", "darkviolet"),
    show_max = FALSE,
    legend_border = FALSE
  )
  dev.off()
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
    purrr::map(~ splendid::classification(x[, .], y, algorithms = alg, seed_alg = seed))
  saveRDS(fits, file.path(c1_path, "rf_alternate_c1.rds"))

  # test alternate models on cut 2
  x.new <- sl_data(test1_dat)
  y.new <- sl_class(test1_lab, x.new)

  mod.pred <- fits %>%
    purrr::map(~ splendid::prediction(., x.new[, which_genes(., alg)], y.new) %>%
                 magrittr::set_names(rownames(x.new))) %>%
    purrr::set_names(c("fit_0", "fit_1", "fit_2"))
  saveRDS(mod.pred, file.path(c1_path, "cut2_pred_alternates_list.rds"))

  rf <- mod.pred %>%
    as.data.frame(., col.names = names(.)) %>%
    tibble::rownames_to_column("ottaID") %>%
    dplyr::inner_join(test1_lab, ., by = "ottaID")

  res_pred <- rf %>%
    dplyr::select(dplyr::matches("fit")) %>%
    purrr::map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    purrr::map_dbl(purrr::pluck, "overall", "Accuracy")

  pdf(file.path(outputDir, "gene_selection", "plots", "Accuracy_alternates.pdf"))
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
    dplyr::select(dplyr::matches("fit")) %>%
    purrr::map(caret::confusionMatrix, rf[["Adaboost.xpn"]]) %>%
    purrr::map(~ .[["byClass"]][, "F1"]) %>%
    purrr::transpose() %>%
    purrr::map(~ list(F1_score = unlist(.))) %>%
    purrr::set_names(gsub("\\.", "-", purrr::map_chr(strsplit(names(.), split = " "), 2)))

  pdf(file.path(plot_dir, "F1_alternates.pdf"))
  loso_plot(
    data = byclass.res,
    group = "F1_score",
    main = "F1 Score by class\nby # of genes",
    xbreaks = seq_len(3),
    ylim = c(0.8, 1),
    col_alg = c("red", "forestgreen", "darkslategray3", "darkviolet"),
    show_max = FALSE,
    legend_border = FALSE
  )
  dev.off()

  colnames(rf)[2] <- "Label"
  readr::write_csv(rf, file.path(c1_path, "cut2_pred_alternates_table.csv"))
}
