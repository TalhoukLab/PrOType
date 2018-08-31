# Evaluate Batch Effects --------------------------------------------------

# Load packages and utility functions
suppressPackageStartupMessages(library(Biobase))
source(here::here("R/3-post_processing/utils/utils.R"))

# Create mapping, read in data and clusters for trainSet and trainSet2
for (dataSet in datasets) {

  cli::cat_line("Creating plot")

  # Load the cases and label the cuts
  cohorts.tmp <- readr::read_csv(file.path(dataDir, "nstring", "inclusion.csv"),
                                 col_types = readr::cols())

  # Select cases included in post and pull out only useful info
  cohorts <- cohorts.tmp %>%
    dplyr::filter(post == 1) %>%
    dplyr::select(c(Label, StudyID, CohortLabel)) %>%
    dplyr::mutate(Label = gsub("\\.|\\+", "_", Label))

  mapping <- build_mapping(dataSet)
  tdat <- readRDS(file.path(outputDir, "unsupervised", "prep_data",
                            dataSet,
                            paste0("tdat_", dataSet, ".rds")))
  final_clust_file <- readRDS(file.path(outputDir, "unsupervised", "final",
                                        dataSet,
                                        paste0("all_clusts_", dataSet, ".rds")))

  rownames(tdat) <- drop_char(rownames(tdat))
  FinalClust <- data.frame(clust = mapping$labels[final_clust_file[, "kmodes"]],
                           Label = rownames(tdat),
                           stringsAsFactors = FALSE)

  if (nrow(FinalClust) != nrow(tdat)) {
    stop("Number of rows do not match in one of the datasets")
  }

  # Join with cluster labels
  cohorts <- dplyr::inner_join(FinalClust, cohorts, "Label") %>%
    tibble::column_to_rownames("Label")

  # Compute PVCA
  # Assess the batch sources by fitting all "sources" as random effects including
  # two-way interaction terms in the Mixed Model(depends on lme4 package) to
  # selected principal components, which were obtained from the original data
  # correlation matrix.
  # xpn "#7FA197"
  # cbt "#91B0A8"

  # Compute PVCA Object
  pvcaObj <- source_of_var(cohorts, t(tdat), c("clust", "CohortLabel"))
  saveRDS(pvcaObj, file.path(outputDir, "evals",
                             paste0(dataSet, "_pvcaObj.rds")))

  # PVCA Plot
  pdf(file.path(outputDir, "plots", paste0(dataSet, "_pvca.pdf")))
  pvca_plot(pvcaObj, "#91B0A8", dataSet)
  dev.off()

  # Compute PCA Object for first 3 PC's
  pca <- prcomp_n(tdat, n = 3)
  saveRDS(pca, file.path(outputDir, "evals", paste0(dataSet, "_pca.rds")),
          compress = "xz")
  pca_df <- tibble::tibble(batch1 = as.character(cohorts$clust),
                           batch2 = cohorts$CohortLabel) %>%
    cbind(pca)
  saveRDS(pca_df, file.path(outputDir, "evals", paste0(dataSet, "_pca_df.rds")))

  # PCA plots
  p1 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, type = "scatter3d",
                        mode = "markers", color = ~batch1)
  htmlwidgets::saveWidget(p1, file.path(outputDir, "plots",
                                        paste0(dataSet, "_pcaClust.html")),
                          selfcontained = FALSE)

  p2 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, type = "scatter3d",
                        mode = "markers", color = ~batch2)
  htmlwidgets::saveWidget(p2, file.path(outputDir, "plots",
                                        paste0(dataSet, "_pcastudy.html")),
                          selfcontained = FALSE)
}
