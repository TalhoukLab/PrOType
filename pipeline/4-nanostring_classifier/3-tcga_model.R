# Predict NanoString data using TCGA model --------------------------------

# Load packages and utility functions
suppressPackageStartupMessages(library(Biobase))
library(randomForest)
source(here::here("pipeline/4-nanostring_classifier/utils.R"))

# Load raw data
load(here::here("data/nstring/tcga_data_with_molsubtypes.rda")) # eset
load(here::here("data/nstring/otta_exp_data.rdata")) # otta.eset
dat <- readr::read_csv(here::here("data/nstring/OTTA_Cohorts_20171103_AFFY_MA.csv"), col_types = readr::cols())

# Random forest parameters
trees <- 1000
m <- floor(sqrt(trees))

# TCGA samples
tcga_samples <- dat %>%
  dplyr::filter(StudyID == "TCGA", `Included Post 20171103 = 1` == 1) %>%
  dplyr::mutate(SAMPLEID = gsub("_", "-", substr(Label, 15, 26))) %>%
  dplyr::pull(SAMPLEID)

# Keeping common features from both expression set and scaling
eset <- eset %>% `[`(, intersect(tcga_samples, sampleNames(.)))
otta_exp <- exprs(otta.eset) %>% `class<-`("numeric")
tcga_exp <- exprs(eset)
features <- intersect(rownames(otta_exp), rownames(tcga_exp))
tcga_dat <- data.frame(scale(t(tcga_exp[features, ])),
                       subtype = eset[["MolSubtype"]])
otta_dat <- scale(t(otta_exp[features, ]))

# TCGA "dummy" CV training to set random state for TCGA model
set.seed(1024)
flds <- caret::createFolds(seq_len(nrow(tcga_dat)))
purrr::walk(seq_along(flds), ~ {
  train <- tcga_dat[-flds[[.]], ]
  randomForest(subtype ~ ., data = train, ntree = trees, mtry = m)
})

# TCGA model
tcga_model <-
  randomForest(subtype ~ ., data = tcga_dat, ntree = trees, mtry = m)

# Prediction on OTTA data using TCGA model (seed needed to break prob ties)
cli::cat_line("Predicting full nanostring data using TCGA model")
set.seed(21)
pred <- splendid::prediction(tcga_model, otta_dat)

# Predictions
tcga_pred <- tibble::tibble(ottaID = rownames(otta_dat)) %>%
  dplyr::mutate(preds = pred %>% {
    dplyr::case_when(. == "C1" ~ "C1.MES",
                     . == "C2" ~ "C2.IMM",
                     . == "C4" ~ "C4.DIF",
                     . == "C5" ~ "C5.PRO")
  })
readr::write_csv(tcga_pred, file.path(outputDir, "nanostring", "predictions",
                                      "tcga_pred.csv"))

# Probabilities
tcga_probs <- as.data.frame(attr(pred, "prob")) %>%
  `colnames<-`(c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")) %>%
  tibble::rownames_to_column("ottaID") %>%
  tibble::as_tibble()
readr::write_csv(tcga_probs, file.path(outputDir, "nanostring", "predictions",
                                       "tcga_probs.csv"))
