# Compute PVCA Plots

# Load Packages----
library(tidyverse)
library(pvca)

# Functions----
dropChar <- function(x) {
  x <- as.character(x)
  substr(x, 0, nchar(x) - 7)
}

#********************************************************************
# Generate substrain mapping table for specified study
#********************************************************************
build_mapping <- function(train.set) {
  labs <- c(1, 2, 3, 4)
  if (train.set == "ov.afc1_cbt") {
    map <- data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if (train.set == "ov.afc1_xpn") {
    map <- data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    print("No valid training set specified")
  }
  return(map)
}

##################################################
# Function to compute PVCA Object
##################################################
CompSrcOfVar <- function(annMat, dat, factorsOfInterest, cols, ttl = "",
                         pct_threshold = 0.6) {
  # rows of exp should be the same as column of dat and in the same order
  if (!all(rownames(annMat) == colnames(dat))) {
    stop("All rownames of annotation matrix do not correspond to the column names of the data matrix")
  }
  phenoData <- new("AnnotatedDataFrame", data = annMat)
  MASet <- Biobase::ExpressionSet(assayData = data.matrix(dat),
                                  phenoData = phenoData)
  pvca::pvcaBatchAssess(MASet, factorsOfInterest, pct_threshold)
}

##################################################
# Function to Plot PVCA Object
##################################################
pvca.plot <- function(pvcaObj, cols = "blue", ttl = "") {
  par(oma = c(1, 0.5, 1, 1), mar = c(5.1, 7.6, 4.1, 0.1))
  # "Weighted average proportion variance"
  bp <- barplot(sort(pvcaObj$dat), horiz = TRUE, ylab = "", xlab = "",
                xlim = c(0,1.1), border = "white", main = ttl,
                space = 1, las = 1, col = cols)
  axis(2, at = bp, labels = pvcaObj$label[order(pvcaObj$dat)], ylab = "", cex.axis = 0.8, las = 2)
  values <- sort(pvcaObj$dat)
  new_values <- paste(round(values * 100, 1), "%", sep = "")
  text(sort(pvcaObj$dat), bp, labels = new_values, pos = 4, cex = 0.8)
}

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
