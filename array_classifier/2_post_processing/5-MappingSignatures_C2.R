options(stringsAsFactors = FALSE)
library(gplots)
library(magrittr)

# Functions---
tr <- function(X) {
  sum(diag(X))
}

matrix_loss <- function(Rh, R = NULL) {
  # Add check that it's a correlation matrix
  if (is.null(R)) {
    R <- diag(nrow(Rh))
  }
  entropy_loss <- tr(solve(R) %*% Rh) - log(det(solve(R) %*% Rh)) - nrow(Rh)
  quadratic_loss <- tr((solve(R) %*% Rh - diag(nrow(Rh)))^2)
  return(list(entropy = entropy_loss, quadratic = quadratic_loss))
}


# Load data----

rfive.type <- read.csv(file.path(data_dir, "external", "lasso_PAM100.csv"))
rfour.type <- read.csv(file.path(data_dir, "external", "verhaakGS.csv")) %>%
  set_colnames(c("Gene.Name", "C4.DIF", "C2.IMM", "C1.MES", "C5.PRO"))

# TODO:// Where does the 'preds' come from?
input.file.vec <- dir(file.path(outputDir, "evals"),
  full.names = TRUE,
  pattern = "centroid_data*.rds") %>%
    grep("preds", ., value = TRUE)

input.file.name <- dir(paste0(outputDir, "evals"),
    full.names = FALSE,
    pattern = "centroid_data*.rds"
  ) %>%
  grep("preds", ., value = TRUE) %>%
  strsplit(., split = "[.]") %>%
  lapply(., "[", 1) %>%
  unlist(.)
Rh <- NULL
R <- NULL
Loss <- NULL
diagLoss <- NULL

# Make and Save Plots----
pdf(file.path(outputDir, "plots", "Cor_w_Sigs_c2.pdf")

for (input_file in input.file.vec) {
  rAff.eb <- read.csv(input_file)

  common.gene5 <- intersect(rfive.type$X, rAff.eb$Gene)
  common.gene4 <- intersect(rfour.type$Gene.Name, rAff.eb$Gene)

  # Find the id of the common genes within each data set
  mch.Affy.idx4 <- match(common.gene4, rAff.eb$Gene)
  mch.Affy.idx5 <- match(common.gene5, rAff.eb$Gene)
  mch.fourT.idx <- match(common.gene4, rfour.type$Gene.Name)
  mch.fiveT.idx <- match(common.gene5, rfive.type$X)

  # Remove the gene names and re-order them to match each other
  four.mtx <- as.matrix(rfour.type[mch.fourT.idx, 2:5])
  five.mtx <- as.matrix(rfive.type[mch.fiveT.idx, 2:6])

  affy.mtx4 <- as.matrix(rAff.eb[
    mch.Affy.idx4,
    c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")
  ])
  affy.mtx5 <- as.matrix(rAff.eb[
    mch.Affy.idx5,
    c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")
  ])

  type.cor.mtx4 <- cor(four.mtx, affy.mtx4, method = "pearson")
  heatmap.2(type.cor.mtx4,
    trace = "none", col = bluered(25),#Rowv = FALSE, Colv = FALSE,
    key = FALSE, dendrogram = "none",
    cellnote = format(round(type.cor.mtx4, 2)), notecol = "black",
    main = paste(
      input.file.name[k], "vs Verhaak \n", "N. of common genes:",
      length(common.gene4)
    ), margins = c(8, 8)
  )
  Rh[[k]] <- type.cor.mtx4[, order(rownames(type.cor.mtx4))]
  Loss[[k]] <- matrix_loss(Rh[[k]])
  diagLoss[[k]] <- sum((diag(Rh[[k]]) - rep(1,ncol(Rh[[k]])))^2)
  type.cor.mtx5 <- cor(five.mtx, affy.mtx5, method = "pearson")
  heatmap.2(type.cor.mtx5,
    trace = "none", col = bluered(25),
    key = FALSE, dendrogram = "none",
    cellnote = format(round(type.cor.mtx5, 2)), notecol = "black",
    main = paste0(
      input.file.name[k], "vs Chen \n", "N. of common genes:",
      length(common.gene5)
    ),
    margins = c(8, 8)
  )
}

matrix(unlist(Loss), ncol = 2, byrow = TRUE) %>% set_rownames(input.file.name)
# TODO:// Should this be written anywhere?
diagLoss
