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

# Inputs----
ndatc1 <- "ov.afc1_xpn"

# outputDir <- "/Users/atalhouk/Repositories/NanoString/HGSCS/Results/"
# InputDir <- "/Users/atalhouk/Repositories/NanoString/HGSCS/data/intermediate/outputs/"

outputDir <- "/Users/dchiu/Documents/Misc/Dustin/Results/"
InputDir <- "/Users/dchiu/Documents/Misc/Dustin/data_complete/anonymized_data/data/"

# Load Data----
tdatc1 <- readRDS(paste0(InputDir, "data_pr_", ndatc1, "/tdat_", ndatc1, ".rds"))
rownames(tdatc1) <- dropChar(rownames(tdatc1))

FinalClustc1 <- data.frame(clust = readRDS(paste0(InputDir, "data_pr_", ndatc1, "/all_clusts_", ndatc1, ".rds"))[,"kmodes"],
                           Label = rownames(tdatc1),
                           stringsAsFactors = FALSE)

mapping <- build_mapping(ndatc1)
FinalClustc1$clust <- mapping$labels[FinalClustc1$clust]

# tdatc2 <- readRDS(paste0(InputDir,ndatc2,"/cdat_",ndatc2,".rds"))
# rownames(tdatc2) <- rownames(tdatc2) %>% dropChar(.)

# FinalClustc2 <- data.frame(clust=readRDS(paste0(InputDir,ndatc2,"/all_clusts_",ndatc2,".rds"))[,"kmodes"], Label=rownames(tdatc2), stringsAsFactors = FALSE)

# mapping <- build_mapping(ndatc2)
# FinalClustc2$clust <- mapping$labels[FinalClustc2$clust]

# Load the cases and label the cuts
cohorts.tmp <- read.csv(file.path(InputDir, "inclusion.csv"), stringsAsFactors = FALSE)
# Select cases included in post and pull out only useful info
cohorts <- cohorts.tmp %>%
  filter(post == 1) %>%
  # select(c(Label, StudyID, CohortLabel, Cut))
  select(c(Label, StudyID, CohortLabel)) %>%
  mutate(Label = gsub("\\.|\\+", "_", Label))


if (dim(FinalClustc1)[1] != nrow(tdatc1)) {
  stop("number of rows do not match in one of the datasets")
}
# if(dim(FinalClustc2)[1]!=nrow(tdatc2)){stop("number of rows do not match in one of the datasets")}

# FinalClust <- rbind(FinalClustc1,FinalClustc2)
FinalClust <- FinalClustc1
cohorts <- dplyr::inner_join(FinalClust, cohorts, "Label")
rownames(cohorts) <- cohorts$Label

# Compute PVCA---
# Assess the batch sources by fitting all "sources" as random effects including two-way interaction terms in the Mixed Model(depends on lme4 package) to selected principal components, which were obtained from the original data correlation matrix.
#xpn "#7FA197"
#cbt "#91B0A8"
#Compute pvca Object
pvcaObj <- CompSrcOfVar(cohorts, t(tdatc1), c("clust", "CohortLabel"), "#91B0A8", ndatc1)
saveRDS(pvcaObj, file.path(outputDir, "pvcaObj.rds"))

pdf(paste0(outputDir, "figures/", ndatc1, "_pvca.pdf"))
pvca.plot(pvcaObj, "#91B0A8", ndatc1)
dev.off()

# pvcaObj <- CompSrcOfVar(subset(cohorts, Cut==1), t(tdatc1),c("clust","CohortLabel"), ndatc1,"#91B0A8")
#
# pdf(paste0(outputDir,"figures/",ndatc1,"_pvca.pdf"))
# pvca.plot(pvcaObj,ndatc1,"#91B0A8")
# dev.off()
#
# pvcaObj <- CompSrcOfVar(subset(cohorts, Cut==2), t(tdatc2),c("clust","CohortLabel"), ndatc1,"#91B0A8")
#
# pdf(paste0(outputDir,"figures/",ndatc2,"_pvca.pdf"))
# pvca.plot(pvcaObj,ndatc2,"#91B0A8")
# dev.off()

# Compute PCA---
pcaa <- prcomp(tdatc1)
saveRDS(pcaa, file.path(outputDir, "pcaa.rds"), compress = "xz")
# cohorts1 <- cohorts %>% filter(Cut==1)
cohorts1 <- cohorts
# dim(pcaa$x)
df <- data.frame(
  batch1 = as.character(cohorts1$clust),
  batch2 = as.character(cohorts1$CohortLabel),
  pcaa$x[, 1:3],
  stringsAsFactors = FALSE
)

p1 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, color = ~batch1)
#htmlwidgets::saveWidget(p1, paste0(outputDir, "figures/", ndatc1, "_pcaClust.html"))

p2 <- plotly::plot_ly(df, x = ~PC2, y = ~PC1, z = ~PC3, color = ~batch2)
#htmlwidgets::saveWidget(p2, paste0(outputDir, "figures/", ndatc1, "_pcastudy.html"))
