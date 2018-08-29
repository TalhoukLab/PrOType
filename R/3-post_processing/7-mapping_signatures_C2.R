# Mapping Signatures C2 ---------------------------------------------------

# Load utility functions
`%>%` <- magrittr::`%>%`
source(here::here("R/3-post_processing/utils/utils.R"))

# Load data
rfive.type <- readr::read_csv(
  file = file.path(dataDir, "external", "Lasso_PAM100.csv"),
  col_types = readr::cols()
) %>%
  magrittr::set_colnames(c("Gene.Name", colnames(.[-1])))
rfour.type <- readr::read_csv(
  file = file.path(dataDir, "external", "verhaakGS.csv"),
  col_types = readr::cols()
) %>%
  magrittr::set_colnames(c("Gene.Name", "C4.DIF", "C2.IMM", "C1.MES", "C5.PRO"))

# Input file names
input.file.vec <- list.files(
  path = file.path(outputDir, "evals"),
  pattern = "centroid_data_preds.*\\.csv",
  full.names = TRUE
)
input.file.name <- tools::file_path_sans_ext(basename(input.file.vec))

# Initiate Loss list and specify constant for affy labels
Loss <- purrr::list_along(input.file.vec)
affy.labs <- c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")

# Make and Save Plots----
pdf(file.path(outputDir, "plots", "Cor_w_Sigs_c2.pdf"))

for (k in seq_along(input.file.vec)) {
  # Read input file
  rAff.eb <- readr::read_csv(input.file.vec[k], col_types = readr::cols()) %>%
    magrittr::set_colnames(make.names(colnames(.)))

  # Common genes
  common.gene4 <- intersect(rfour.type$Gene.Name, rAff.eb$Gene)
  common.gene5 <- intersect(rfive.type$Gene.Name, rAff.eb$Gene)

  # Find the id of the common genes within each data set
  mch.Affy.idx4 <- match(common.gene4, rAff.eb$Gene)
  mch.Affy.idx5 <- match(common.gene5, rAff.eb$Gene)
  mch.fourT.idx <- match(common.gene4, rfour.type$Gene.Name)
  mch.fiveT.idx <- match(common.gene5, rfive.type$Gene.Name)

  # Remove the gene names and re-order them to match each other
  four.mtx <- as.matrix(rfour.type[mch.fourT.idx, -1]) %>%
    magrittr::extract(, order(colnames(.)))
  five.mtx <- as.matrix(rfive.type[mch.fiveT.idx, -1])
  affy.mtx4 <- as.matrix(rAff.eb[mch.Affy.idx4, affy.labs])
  affy.mtx5 <- as.matrix(rAff.eb[mch.Affy.idx5, affy.labs])

  # Correlation matrices and entropy/quadratic loss for mtx4
  type.cor.mtx4 <- cor(four.mtx, affy.mtx4, method = "pearson")
  type.cor.mtx5 <- cor(five.mtx, affy.mtx5, method = "pearson")
  Loss[[k]] <- type.cor.mtx4 %>%
    magrittr::extract(, order(rownames(.))) %>%
    matrix_loss()

  # Heatmaps
  par(cex.main = 0.9)
  # Common heatmap arguments
  heatmap_args <- list(trace = "none", col = gplots::bluered(25), key = FALSE,
                       dendrogram = "none", notecol = "black",
                       margins = c(5, 5), cexRow = 0.9, cexCol = 0.9,
                       Rowv = FALSE, Colv = FALSE)
  # Invoke heatmap on common and different arguments
  purrr::invoke(
    .f = gplots::heatmap.2, .x = heatmap_args,
    x = type.cor.mtx4, cellnote = format(round(type.cor.mtx4, 2)),
    main = paste(input.file.name[k], "vs Verhaak \nN. of common genes:",
                 length(common.gene4))
  )
  purrr::invoke(
    .f = gplots::heatmap.2, .x = heatmap_args,
    x = type.cor.mtx5, cellnote = format(round(type.cor.mtx5, 2)),
    main = paste(input.file.name[k], "vs Chen \nN. of common genes:",
                 length(common.gene5))
  )
  par(cex.main = 1.2)
}
dev.off()

# Save loss information
Loss_df <- Loss %>%
  do.call(rbind, .) %>%
  magrittr::set_rownames(input.file.name)
saveRDS(Loss_df, file.path(outputDir, "evals", "Loss_df.rds"))
