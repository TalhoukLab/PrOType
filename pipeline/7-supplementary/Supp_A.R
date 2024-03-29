## ----setup_A, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
params <- list(outputDir = outputDir)

## ----child="Supp_A05.Rmd"------------------------------------------------

## ----setup_A05, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_A05------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(here)  
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", FALSE)
panderOptions("table.split.table", Inf)

aa_raw <- readRDS(file.path(
  params$outputDir,
  "unsupervised/map_genes/ov.afc1_xpn/npcp-hcNorm_ov.afc1_xpn.rds"
))
aa_clusts <- readRDS(file.path(
  params$outputDir,
  "unsupervised/final/ov.afc1_xpn/all_clusts_ov.afc1_xpn.rds"
))
sl_ci <- readr::read_csv(file.path(
  params$outputDir,
  "supervised/top_ci/sup_lrn_ov.afc1_xpn.csv"
), col_types = readr::cols())
tcga_dat <- readr::read_csv(
  here("data/nstring/OTTA_Cohorts_20171103_AFFY_MA.csv"),
  col_types = readr::cols()
)
load(here("data/nstring/tcga_data_with_molsubtypes.rda")) # eset

## ----aa_internal_validation----------------------------------------------
# All-array interval validation
aa_iv <- sl_ci %>% 
  dplyr::filter(grepl("accuracy|^f1", X1)) %>% 
  tidyr::gather(alg, ci, -1, factor_key = TRUE) %>% 
  tidyr::spread(X1, ci) %>% 
  dplyr::select(
    Algorithm = alg,
    `Overall Accuracy` = accuracy,
    `F1-score C1.MES` = f1.X4,
    `F1-score C2.IMM` = f1.X1,
    `F1-score C4.DIF` = f1.X2,
    `F1-score C5.PRO` = f1.X3
  )
pandoc.table(aa_iv)

## ----aocs_table----------------------------------------------------------
# AOCS samples in OTTA cohort
aocs_tab <- matrix(
  c(76, 3, 0, 1, 4, 42, 0, 1, 0, 5, 46, 1, 3, 0, 0, 33),
  nrow = 4, 
  byrow = TRUE,
  dimnames = list(
    `AOCS Predicted Labels` = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO"),
    `AOCS Published Labels` = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")
  )
) %>% 
  as.table()

## ----aocs_confmat--------------------------------------------------------
aocs_confmat <- caret::confusionMatrix(aocs_tab)
pandoc.table(ftable(aocs_confmat[["table"]]))

## ----aocs_ov_metrics-----------------------------------------------------
aocs_confmat_ov_metrics <- confmat_metrics(aocs_confmat, metrics = "overall")
pandoc.table(aocs_confmat_ov_metrics)

## ----aocs_bc_metrics-----------------------------------------------------
aocs_confmat_bc_metrics <- confmat_metrics(aocs_confmat, metrics = "byclass")
pandoc.table(aocs_confmat_bc_metrics, keep.trailing.zeros = TRUE)

## ----aa_kmodes-----------------------------------------------------------
# All-array samples
aa_samples <- gsub("_", "-", stringr::str_sub(rownames(aa_raw), 15, 26))

# All-array kmodes clustering labels
aa_kmodes <- aa_clusts %>% 
  dplyr::transmute(
    sample = aa_samples,
    kmodes = factor(kmodes,
                    labels = c("C2.IMM", "C4.DIF", "C5.PRO", "C1.MES")) %>% 
      relevel("C1.MES")
  ) %>%
  dplyr::filter(grepl("TCGA", sample)) %>% 
  tibble::as_tibble()

## ----tcga_training-------------------------------------------------------
# TCGA samples
tcga_samples <- tcga_dat %>%
  dplyr::filter(StudyID == "TCGA", `Included Post 20171103 = 1` == 1) %>%
  dplyr::mutate(SAMPLEID = gsub("_", "-", substr(Label, 15, 26))) %>%
  dplyr::pull()

# TCGA training set labels from expression set for included samples
tcga_training <- eset %>%
  `[`(, intersect(tcga_samples, Biobase::sampleNames(.))) %>% {
    tibble::tibble(
      sample = Biobase::sampleNames(.),
      tcga_labels = factor(.[["MolSubtype"]],
                           labels = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO"))
    )
  }

## ----aa_vs_tcga_confmat--------------------------------------------------
# Compare all-array kmodes clustering with TCGA training set labels
aa_vs_tcga <- dplyr::inner_join(aa_kmodes, tcga_training, by = "sample") %>% 
  with(., table(
    `All-Array K-modes Clustering Labels` = kmodes,
    `TCGA Training Labels` = tcga_labels
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(aa_vs_tcga[["table"]]))

## ----aa_vs_tcga_ov_metrics-----------------------------------------------
aa_vs_tcga_ov_metrics <- confmat_metrics(aa_vs_tcga, metrics = "overall")
pandoc.table(aa_vs_tcga_ov_metrics)

## ----aa_vs_tcga_bc_metrics-----------------------------------------------
aa_vs_tcga_bc_metrics <- confmat_metrics(aa_vs_tcga, metrics = "byclass")
pandoc.table(aa_vs_tcga_bc_metrics, keep.trailing.zeros = TRUE)


