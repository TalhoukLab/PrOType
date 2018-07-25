# Compute PVCA Plots

# Load Packages----
suppressPackageStartupMessages({
  library(tidyverse)
  library(pvca)
})
source(here::here("array_classifier/2_post_processing/utils/utils.R"))

mapping <- build_mapping(trainSet)

tdat <- readRDS(file.path(outputDir, trainSet, paste0("data_pr_", trainSet), paste0("tdat_", trainSet, ".rds")))
final_clust_file <- readRDS(file.path(outputDir, trainSet, paste0("data_pr_", trainSet), paste0("all_clusts_", trainSet, ".rds")))

rownames(tdat) <- dropChar(rownames(tdat))
FinalClust <- data.frame(clust = mapping$labels[final_clust_file[, "kmodes"]],
                         Label = rownames(tdat),
                         stringsAsFactors = FALSE)

# Load the cases and label the cuts
cohorts.tmp <- read.csv(file.path(data_dir, "nstring", "inclusion.csv"), stringsAsFactors = FALSE)

# Select cases included in post and pull out only useful info
cohorts <- cohorts.tmp %>%
  filter(post == 1) %>%
  select(c(Label, StudyID, CohortLabel)) %>%
  mutate(Label = gsub("\\.|\\+", "_", Label))

if (dim(FinalClust)[1] != nrow(tdat)) {
  stop("number of rows do not match in one of the datasets")
}

cohorts <- dplyr::inner_join(FinalClust, cohorts, "Label")
rownames(cohorts) <- cohorts$Label

# Compute PVCA---
# Assess the batch sources by fitting all "sources" as random effects including two-way interaction terms in the Mixed Model(depends on lme4 package) to selected principal components, which were obtained from the original data correlation matrix.
#xpn "#7FA197"
#cbt "#91B0A8"

#Compute pvca Object
pvcaObj <- CompSrcOfVar(cohorts, t(tdat), c("clust", "CohortLabel"), "#91B0A8", trainSet)
saveRDS(pvcaObj, file.path(outputDir, "evals", "pvcaObj.rds"))

pdf(file.path(outputDir, "plots", paste0(trainSet, "_pvca.pdf")))
pvca.plot(pvcaObj, "#91B0A8", trainSet)

# Compute PCA---
pcaa <- prcomp(tdat)
saveRDS(pcaa, file.path(outputDir, "evals", "pcaa.rds"), compress = "xz")
df <- data.frame(
  batch1 = as.character(cohorts$clust),
  batch2 = as.character(cohorts$CohortLabel),
  pcaa$x[, 1:3],
  stringsAsFactors = FALSE
)

p1 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, color = ~batch1)
htmlwidgets::saveWidget(p1, file.path(outputDir, "plots", paste0(trainSet, "_pcaClust.html")))

p2 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, color = ~batch2)
htmlwidgets::saveWidget(p2, file.path(outputDir, "plots", paste0(trainSet, "_pcastudy.html")))
