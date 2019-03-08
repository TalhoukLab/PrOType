## ----setup_D, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
params <- list(outputDir = outputDir)

## ----child="Supp_D01.Rmd"------------------------------------------------

## ----setup_C01, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_D01------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(ggplot2)
  library(pander)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", FALSE)
panderOptions("table.split.table", Inf)

dat_raw <- readxl::read_excel(here("data/nstring/nanostring data_replicates and Xsite_20160915.xlsx"))
dat <- readr::read_csv(file.path(params$outputDir, "gene_selection/final_model/replicates_and_Xsite_predictions.csv"),
                       col_types = readr::cols())
otta_2015_raw <- readr::read_csv(file.path(params$outputDir, "gene_selection/final_model/Final_Predictions.csv"),
                                 col_types = list("X1" = readr::col_skip()))
otta_2017_raw <- readr::read_csv(here("data/nstring/nano2_ns_predictions.csv"), col_types = readr::cols())
otta_2018_raw <- readr::read_csv(here("data/nstring/nano3_ns_predictions.csv"), col_types = readr::cols())

fig_path <- file.path(params$outputDir, "supplementary/figures/D01")

## ----clean_D01-----------------------------------------------------------
# Cleaned data: split into original, replicates, and Xsites
dat_clean <- dat %>%
  tidyr::separate(col = ottaID,
                  into = c("ottaID", "type"),
                  sep = "_(?=[^_|LT]+$)",
                  fill = "right") %>%
  tidyr::replace_na(list(type = "O"))

# Filter samples by type
original_samples <- dat_clean %>% dplyr::filter(type == "O")
replicate_samples <- dat_clean %>% dplyr::filter(type == "R")
Xsite_samples <- dat_clean %>% dplyr::filter(type == "X1")

## ----o_vs_r_combine------------------------------------------------------
# Compare original vs replicate predictions
compare_OR <- dplyr::inner_join(original_samples,
                                replicate_samples,
                                by = "ottaID",
                                suffix = c("_O", "_R")) %>% 
  dplyr::filter(!grepl("LT|ARL", ottaID))

## ----o_vs_r_confmat------------------------------------------------------
confmat_OR <- compare_OR %>% 
  with(., table(
    `Predicted Replicates` = predicted_R,
    `Final Originals` = predicted_O
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(confmat_OR[["table"]]))

## ----o_vs_r_overall_metrics----------------------------------------------
ov_OR <- confmat_metrics(confmat_OR, metrics = "overall")
pandoc.table(ov_OR)

## ----o_vs_r_by-class_metrics---------------------------------------------
bc_OR <- confmat_metrics(confmat_OR, metrics = "byclass")
pandoc.table(bc_OR, keep.trailing.zeros = TRUE)

## ----o_vs_r_entropy, fig.width=7, fig.height=5---------------------------
# Compute original vs replicate entropy
entropy_OR <- compare_OR %>%
  dplyr::transmute(
    ottaID,
    entropy_O = apply(.[grep("(?<!type|predicted)_O", names(.), perl = TRUE)], 1,
                        entropy::entropy, unit = "log2"),
    entropy_R = apply(.[grep("(?<!type|predicted)_R", names(.), perl = TRUE)], 1,
                        entropy::entropy, unit = "log2"),
    match = factor(ifelse(predicted_O == predicted_R, "Agree", "Disagree"),
                   levels = c("Agree", "Disagree"))
  ) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("entropy")), round, digits = 3)
readr::write_csv(entropy_OR, file.path(params$outputDir, "supplementary/tables/entropy_O_vs_R.csv"))

# Plot original vs replicate concordance
p <- ggplot(entropy_OR, aes(entropy_O, entropy_R, color = match)) +
  geom_point(alpha = 0.5) +
  geom_abline(linetype = "dashed") +
  scale_color_discrete(drop = FALSE) +
  labs(x = "Entropy of Original Samples",
       y = "Entropy of Replicate Samples",
       title = "Entropy Comparison of Within CodeSet Replicates") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_O_vs_R.pdf"), p, width = 7, height = 5)

## ----o_vs_x_samples------------------------------------------------------
# Samples with original and Xsite
OX_samples <- dat_clean %>%
  dplyr::filter(grepl("O|X", type)) %>%
  dplyr::count(ottaID) %>%
  dplyr::filter(n == 3) %>% 
  dplyr::pull(ottaID) %>% 
  paste(collapse = "|")

## ----o_vs_x_pred---------------------------------------------------------
# OX samples predictions matched to site
OX_pred <- dat_raw %>%
  dplyr::select(ottaID = `OTTA ID`, File.Name) %>% 
  dplyr::filter(!grepl("_R", ottaID), grepl(OX_samples, ottaID)) %>%
  dplyr::mutate(
    site = File.Name %>% 
      substring(18, 20) %>% {
        dplyr::case_when(. == "n31" ~ "AOC",
                         . %in% c("ip3", "mus") ~ "USC",
                         TRUE ~ .)
      }
  ) %>% 
  dplyr::inner_join(dat, by = "ottaID") %>%
  dplyr::transmute(
    ottaID = gsub("_X.*", "", ottaID),
    site,
    predicted
  ) %>%
  tidyr::spread(site, predicted) %>% 
  as.data.frame() %>% 
  tibble::column_to_rownames("ottaID")

## ----o_vs_x_fk, results='markup'-----------------------------------------
# Fleiss' Kappa
OX_fk <- irr::kappam.fleiss(OX_pred)
irr::print.irrlist(OX_fk)

## ----cross_codeset-------------------------------------------------------
# Load OTTA predictions from different codesets, extract IDs and predictions
otta_2015 <- otta_2015_raw %>%
  dplyr::select(ottaID, pred_2015 = final)
otta_2017 <- otta_2017_raw %>%
  dplyr::transmute(
    ottaID = gsub("-N1", "", stringr::str_split_fixed(sample, pattern = "_", n = 4)[, 3]),
    pred_2017 = pred
  ) %>% 
  dplyr::filter(ottaID != "TUKO00487")
otta_2018 <- otta_2018_raw %>%
  dplyr::transmute(
    ottaID = stringr::str_split_fixed(sample, pattern = "_", n = 4)[, 3],
    pred_2018 = pred
  )

## ----15_v_18_confmat-----------------------------------------------------
confmat_1518 <-
  dplyr::inner_join(otta_2015, otta_2018, by = "ottaID") %>% 
  with(., table(
    `Predicted Classifier Only Labels` = pred_2018,
    `Predicted Original Labels` = pred_2015
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(confmat_1518[["table"]]))

## ----15_v_18_overall_metrics---------------------------------------------
ov_1518 <- confmat_metrics(confmat_1518, metrics = "overall")
pandoc.table(ov_1518)

## ----15_v_18_by-class_metrics--------------------------------------------
bc_1518 <- confmat_metrics(confmat_1518, metrics = "byclass")
pandoc.table(bc_1518, keep.trailing.zeros = TRUE)

## ----15_v_17_confmat-----------------------------------------------------
confmat_1517 <-
  dplyr::inner_join(otta_2015, otta_2017, by = "ottaID") %>% 
  with(., table(
    `Predicted Mixed CodeSet Replicate Labels` = pred_2017,
    `Predicted Original Labels` = pred_2015
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(confmat_1517[["table"]]))

## ----15_v_17_overall_metrics---------------------------------------------
ov_1517 <- confmat_metrics(confmat_1517, metrics = "overall")
pandoc.table(ov_1517)

## ----15_v_17_by-class_metrics--------------------------------------------
bc_1517 <- confmat_metrics(confmat_1517, metrics = "byclass")
pandoc.table(bc_1517, keep.trailing.zeros = TRUE)

## ----17_v_18_confmat-----------------------------------------------------
confmat_1718 <-
  dplyr::inner_join(otta_2017, otta_2018, by = "ottaID") %>% 
  with(., table(
    `Predicted Classifer Only CodeSet Labels` = pred_2018,
    `Predicted Mixed CodeSet Labels` = pred_2017
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(confmat_1718[["table"]]))

## ----17_v_18_overall_metrics---------------------------------------------
ov_1718 <- confmat_metrics(confmat_1718, metrics = "overall")
pandoc.table(ov_1718)

## ----17_v_18_by-class_metrics--------------------------------------------
bc_1718 <- confmat_metrics(confmat_1718, metrics = "byclass")
pandoc.table(bc_1718, keep.trailing.zeros = TRUE)

## ----cross_codeset_fk, results='markup'----------------------------------
# Fleiss' Kappa
cc_all <- otta_2015 %>% 
  dplyr::inner_join(otta_2017, by = "ottaID") %>% 
  dplyr::left_join(otta_2018, by = "ottaID") %>% 
  tidyr::replace_na(list(pred_2018 = "Missing")) %>%
  as.data.frame() %>% 
  dplyr::distinct() %>% 
  tibble::column_to_rownames("ottaID")
cc_fk <- irr::kappam.fleiss(cc_all)
irr::print.irrlist(cc_fk)


## ----child="Supp_D02.Rmd"------------------------------------------------

## ----setup_D02, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_D02------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", FALSE)
panderOptions("table.split.table", Inf)

as_dat <- readxl::read_excel(here("data/nstring/Predictions_Anatomical_10FEB2019.xlsx"))
lax_van_dat <- readr::read_csv(file.path(params$outputDir,
                                         "gene_selection/final_model/lax_van_om_test_predictions.csv"),
                               col_types = readr::cols())
bro_dat <- readr::read_csv(file.path(params$outputDir,
                                     "gene_selection/final_model/bro_het_test_predictions.csv"),
                           col_types = readr::cols())

## ----clean_D02-----------------------------------------------------------
# Cleaned data: split into ovary and omentum
dat_clean <- rbind(lax_van_dat, bro_dat) %>%
  tidyr::separate(ottaID, c("ottaID", "type"), sep = "_", extra = "merge", fill = "right") %>%
  tidyr::replace_na(list(type = "OV")) %>% 
  dplyr::filter(!ottaID %in% c("TBRO00024", "TBRO00027", "TBRO00066"))

# Filter samples by type
ovary_samples <- dat_clean %>% dplyr::filter(type == "OV")
omentum_samples <- dat_clean %>% dplyr::filter(type %in% c("OM", "OM1"))
levs <- c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")

## ----ov_vs_om_combine----------------------------------------------------
# Compare ovary and omentum samples
compare_OV_OM <- dplyr::inner_join(ovary_samples,
                                   omentum_samples,
                                   by = "ottaID",
                                   suffix = c("_OV", "_OM")) %>% 
  dplyr::mutate_at(c("predicted_OV", "predicted_OM"), factor, levels = levs)

## ----ov_vs_om_confmat----------------------------------------------------
confmat_OV_OM <- compare_OV_OM %>% 
  with(., table(
    `Predicted Omentum` = predicted_OM,
    `Final Adnexal` = predicted_OV
  )) %>% 
  caret::confusionMatrix()
pandoc.table(ftable(confmat_OV_OM[["table"]]))

## ----ov_vs_om_overall_metrics--------------------------------------------
ov_OV_OM <- confmat_metrics(confmat_OV_OM, metrics = "overall")
pandoc.table(ov_OV_OM)

## ----ov_vs_om_by-class_metrics-------------------------------------------
bc_OV_OM <- confmat_metrics(confmat_OV_OM, metrics = "byclass")
pandoc.table(bc_OV_OM, keep.trailing.zeros = TRUE)

## ----subtype_by_site-----------------------------------------------------
ss_tab <- as_dat %>% 
  dplyr::mutate(
    final = factor(final),
    site = factor(
      `Anatomical-Category`,
      levels = c("adnexal", "UNK", "omentum", "lower genital track", "upper genital track", "peritoneal"),
      labels = toupper(c("adnexal", "presumed adnexal", "omentum", "lower genital track", "upper genital track", "peritoneal")))
  ) %>% 
  Amisc::describeBy("final", by1 = "site", digits = 1) %>% 
  dplyr::select(-c(Variable, Total, PValue)) %>% 
  dplyr::slice(-1)
pandoc.table(ss_tab)

## ----adnexal_comparisons-------------------------------------------------
# Adnexal vs Omentum
aom_chisq <- as_dat %>% 
  dplyr::filter(`Anatomical-Category` %in% c("adnexal", "omentum")) %>% 
  dplyr::transmute(
    final = factor(final),
    site = factor(`Anatomical-Category`)
  ) %>% 
  table() %>% 
  chisq.test() %>% 
  broom::tidy()

# Adnexal vs Presumed Adnexal
apa_chisq <- as_dat %>% 
  dplyr::filter(`Anatomical-Category` %in% c("adnexal", "UNK")) %>% 
  dplyr::transmute(
    final = factor(final),
    site = factor(`Anatomical-Category`)
  ) %>% 
  table() %>% 
  chisq.test() %>% 
  broom::tidy()

# Adnexal vs Other
aot_chisq <- as_dat %>% 
  dplyr::filter(
    `Anatomical-Category` %in% c("adnexal", "lower genital track", "upper genital track", "peritoneal")
  ) %>% 
  dplyr::transmute(
    final = factor(final),
    site = dplyr::case_when(
      `Anatomical-Category` == "adnexal" ~ "adnexal",
      TRUE ~ "other"
    )
  ) %>% 
  table() %>% 
  chisq.test() %>% 
  broom::tidy()

# All comparisons
a_chisq <- dplyr::bind_rows(aom_chisq, apa_chisq, aot_chisq) %>% 
  magrittr::set_names(c("Statistic", "P Value", "df", "Method")) %>% 
  tibble::add_column(
    Comparison = c("Adnexal vs Omentum", "Adenxal vs Presumed Adnexal", "Adnexal vs Other"),
    .before = 1
  )
pandoc.table(a_chisq, justify = "left")


## ----child="Supp_D03.Rmd"------------------------------------------------

## ----setup_D03, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_D03------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(survival)
  library(survminer)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", TRUE)

all_pred <- readr::read_csv(here("data/nstring/predictions.csv"),
                            col_types = list("X1" = readr::col_skip()))
final_preds <- readr::read_csv(file.path(params$outputDir, "gene_selection/final_model/Final_Predictions.csv"),
                               col_types = list("X1" = readr::col_skip()))
b1 <- readr::read_csv(here("data/nstring/nanostring_classifier_data_batch1_20170217_updated.csv"),
                      col_types = readr::cols())
b2 <- readr::read_csv(here("data/nstring/nanostring_classifier_data_batch2_20170221.csv"),
                      col_types = readr::cols())
b3 <- readr::read_csv(here("data/nstring/nanostring_classifier_data_batch3_20170307_updated_NCO.csv"),
                      col_types = readr::cols())
b4 <- readr::read_csv(here("data/nstring/nanostring_classifier_data_batch4_20170512.csv"),
                      col_types = readr::cols())
as_dat <- readxl::read_excel(here("data/nstring/Predictions_Anatomical_10FEB2019.xlsx"))
nec_dat <- readxl::read_excel(here("data/nstring/FinalSubtype_AnalyticFile.xlsx"))
cd8_dat <- suppressWarnings(
  readxl::read_excel(here("data/nstring/OTTA DB_CD8_20180821 for Mike_HGS CCC END_edit_all.xlsx"))
)

fig_path <- file.path(params$outputDir, "supplementary/figures/D03")

## ----clin_combine--------------------------------------------------------
# Remove ARL samples
all_pred <- all_pred %>% 
  dplyr::filter(!grepl("ARL", ottaID))

final_preds <- final_preds %>% 
  dplyr::filter(!grepl("ARL", ottaID))

b4 <- b4 %>% 
  dplyr::filter(is.na(ARL_block)) %>% 
  dplyr::select(-ARL_block)

# Check column names
stopifnot(identical(names(b1), names(b2)))
stopifnot(identical(names(b2), names(b3)))
stopifnot(identical(names(b3), names(b4)))

# Anatomical sites
as_dat <- as_dat %>%
  dplyr::select(ottaID, `Anatomical-Category`)

# Necrosis
nec_dat <- nec_dat %>% 
  dplyr::select(ottaID = ottaid, SampleRegionNecrosis2)

# CD8
cd8_dat <- cd8_dat %>% 
  dplyr::select(ottaID = `OTTA ID`, CD8_MAX)

# Combine batches
clin_d <- rbind(b1, b2, b3, b4) %>% 
  dplyr::rename(ottaID = `OTTA ID`) %>% 
  dplyr::inner_join(as_dat, by = "ottaID") %>% 
  dplyr::inner_join(nec_dat, by = "ottaID") %>% 
  dplyr::left_join(cd8_dat, by = "ottaID")

# Check otta IDs
stopifnot(identical(all_pred[["ottaID"]], clin_d[["ottaID"]]))
stopifnot(identical(final_preds[["ottaID"]], clin_d[["ottaID"]]))

## ----calculate-survival--------------------------------------------------
# Format survival data
# per data dictionary: "OTTA data dictionary_20160914_nanostring.xlsx"
# finalstatus     1=alive; 2=dead; 8=did not follow
# progression_assumed     0=no no evidence or documentation of progression, and patient is still alive; 
#                         1=yes site coded as 'yes'; 
#                         2=assumed yes no evidence or documentation of progression, but patient died; progression assumed; 
#                         9 = unknown no information on progression or vital status available
# causedeath      refer to item 12 in OCAC Clinical Data Capture Guidelines, 
#                   1 = Progression of Disease - known or presumed, 
#                   2 = Treatment related, 
#                   3 = Other, 
#                   7 = Not applicable (i.e. still alive), 
#                   9 = Unknown 
# Covariates: BRCA 1/2 mut, residual disease, age, stage, final subtype (-mut, -res)
# Censor at 10 years
CUTOFF.YEAR <- 10
clin_d <- clin_d %>%
  dplyr::mutate(
    anatomical_site = dplyr::case_when(
      `Anatomical-Category` == "UNK" ~ "presumed adnexal",
      TRUE ~ `Anatomical-Category`
    ) %>% 
      factor(levels = c("adnexal", "omentum", "peritoneal", "upper genital track",
                        "lower genital track", "presumed adnexal")),
    cellularity = factor(
      `Sample Region Cellularity`,
      levels = c("0-20", "21-40", "41-60", "61-80", "81-100")
    ), 
    necrosis = dplyr::case_when(
      SampleRegionNecrosis2 == "1" ~ "none",
      SampleRegionNecrosis2 == "2" ~ "<=20%",
      SampleRegionNecrosis2 == "3" ~ ">20%",
      TRUE ~ NA_character_
    ) %>% 
      forcats::fct_relevel("none"),
    cd8 = dplyr::case_when(
      CD8_MAX == 0 ~ "none",
      CD8_MAX == 1 ~ "low",
      CD8_MAX == 2 ~ "med",
      CD8_MAX == 3 ~ "high",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("none", "low", "med", "high")),
    race_v1 = dplyr::case_when(
      race == "1" ~ "white",
      hispanic == 2 ~ "hispanic",
      race %in% c("2", "3", "4", "5") ~ "other",
      TRUE ~ NA_character_
    ) %>% 
      forcats::fct_relevel("white"),
    residual_disease = dplyr::case_when(
      resdx == "1" ~ "no residual",
      resdx %in% c("2", "3", "4", "5", "6", "8") ~ "any residual",
      TRUE ~ NA_character_
    ) %>% 
      forcats::fct_relevel("no residual"),
    brca_v1 = dplyr::case_when(
      brca1_2 %in% c("4", "6", "WT") ~ "all wildtypes",
      brca1_2 %in% c("1", "BRCA 1") ~ "pathogenic BRCA1 mutation",
      brca1_2 == "2" ~ "pathogenic BRCA2 mutation",
      TRUE ~ NA_character_
    ) %>% 
      factor(),
    brca_v2 = dplyr::case_when(
      brca1_2 %in% c("4", "6", "WT") ~ "all wildtypes",
      brca1_2 %in% c("1", "BRCA 1", "2", "3", "BRCA 1 and BRCA 2") ~ "pathogenic BRCA1/BRCA2/NOS mutation",
      TRUE ~ NA_character_
    ) %>% 
      factor(),
    age_dx = dplyr::case_when(
      refage_revised == "." ~ NA_real_,
      TRUE ~ as.numeric(refage_revised)
    ),
    stage = dplyr::case_when(
      stagenew == "1" ~ "low",
      stagenew == "2" ~ "high",
      TRUE ~ NA_character_
    ) %>% 
      forcats::fct_relevel("low"),
    os_yrs = dplyr::case_when(
      timelastfu %in% c("", ".") ~ NA_real_,
      TRUE ~ as.numeric(timelastfu) / 365.25
    ),
    os_sts = dplyr::case_when(
      finalstatus == 1 ~ "os.censored",
      finalstatus == 2 ~ "os.event",
      TRUE ~ NA_character_
    ),
    os_yrs10 = ifelse(os_yrs < CUTOFF.YEAR, os_yrs, CUTOFF.YEAR),
    os_sts10 = dplyr::case_when(
      finalstatus == 1 | os_yrs10 < os_yrs ~ "os.censored",
      TRUE ~ os_sts
    ),
    dss_yrs = os_yrs,
    dss_sts = dplyr::case_when(
      causedeath == 1 ~ "dss.event",
      causedeath == 9 ~ NA_character_,
      TRUE ~ "dss.censored"
    ),
    dss_yrs10 = os_yrs10,
    dss_sts10 = dplyr::case_when(
      causedeath != 9 & dss_yrs10 < dss_yrs ~ "dss.censored",
      TRUE ~ dss_sts
    ),
    pfs_yrs = dplyr::case_when(
      daystoprogression %in% c("", ".") ~ NA_real_,
      TRUE ~ as.numeric(daystoprogression) / 365.25
    ),
    pfs_sts = dplyr::case_when(
      progression_assumed_OCAC == 0 ~ "pfs.censored",
      progression_assumed_OCAC %in% c(1, 2) ~ "pfs.event",
      TRUE ~ NA_character_
    ),
    pfs_yrs10 = ifelse(pfs_yrs < CUTOFF.YEAR, pfs_yrs, CUTOFF.YEAR),
    pfs_sts10 = dplyr::case_when(
      progression_assumed_OCAC == 0 | pfs_yrs10 < pfs_yrs ~ "pfs.censored",
      TRUE ~ pfs_sts
    )
  ) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::matches("sts")), as.factor)

# Join predictions with clinical data and modify case TVAN20158
d <-
  list(all_pred, final_preds, clin_d) %>%
  purrr::reduce(dplyr::inner_join, by = "ottaID") %>% 
  dplyr::mutate(
    final = factor(final),
    os_yrs = magrittr::inset(
      os_yrs,
      ottaID == "TVAN20158",
      difftime(as.Date("2007-10-27"), as.Date("2003-04-24"), units = "days") /
        365.241
    ),
    os_yrs10 = magrittr::inset(
      os_yrs10,
      ottaID == "TVAN20158",
      difftime(as.Date("2007-10-27"), as.Date("2003-04-24"), units = "days") /
        365.241
    ),
    dss_yrs = os_yrs,
    dss_yrs10 = os_yrs10,
    pfs_yrs = magrittr::inset(
      pfs_yrs,
      ottaID == "TVAN20158",
      NA_real_
    ),
    pfs_yrs10 = magrittr::inset(
      pfs_yrs10,
      ottaID == "TVAN20158",
      NA_real_
    )
  ) %>% 
  dplyr::rename(array_preds = Adaboost.xpn,
                TCGA_preds = TCGA.Predicted.Subtype) %>% 
  dplyr::mutate_at(c("array_preds", "TCGA_preds"), make.names)

# Concordant and Discordant samples
conc <- d %>% dplyr::filter(array_preds == TCGA_preds)
disc <- d %>% dplyr::filter(array_preds != TCGA_preds)

# make sure there are no os_yrs > 100 years
stopifnot(sum(d[["os_yrs"]] > 100, na.rm = TRUE) == 0)

## ----subtype_by_brca-----------------------------------------------------
sb_tab <- dplyr::inner_join(
  Amisc::describeBy(d, "final", by1 = "brca_v1", digits = 1),
  Amisc::describeBy(d, "final", by1 = "brca_v2", digits = 1),
  by = c("Levels", "all wildtypes")
) %>% 
  dplyr::select(2:5, 9) %>% 
  dplyr::slice(-1)
pandoc.table(sb_tab, split.tables = Inf)

## ----year_dx_by_subtype, fig.width=7, fig.height=5-----------------------
bp_dat <- d %>%
  dplyr::filter(anatomical_site %in% c("adnexal", "presumed adnexal")) %>% 
  dplyr::transmute(
    dxyear = as.numeric(refage_revised) + as.numeric(dobyear),
    final
  )

num5 <- fivenum(bp_dat[["dxyear"]])
iqr <- paste(num5[c(2, 4)], collapse = " - ")
num5_text <- paste0(c("Min", "Median (IQR)", "Max"),
                    ": ",
                    c(num5[1], paste0(num5[3], " (", iqr, ")"), num5[5]),
                    collapse = ", ")

p <- ggplot(bp_dat, aes(final, dxyear)) +
  geom_boxplot(varwidth = TRUE, outlier.shape = NA, coef = 0,
               fill = "#D1D1D1", color = "white", na.rm = TRUE) +
  geom_dotplot(method = "histodot", binwidth = 1, binaxis = "y", dotsize = 0.05,
               color = "#3A6EE3", na.rm = TRUE) +
  labs(
    x = "Final Subtype",
    y = "Year of Diagnosis",
    title = "Year of Diagnosis by HGSC Final Subtype Classification",
    subtitle = paste0("All Adnexal and Presumed Adnexal n=", nrow(bp_dat), ", ", num5_text)
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10))

# print(p)
ggsave(file.path(fig_path, "year_dx_by_subtype.pdf"), p, width = 7, height = 5)

## ----cohort_characteristics_all------------------------------------------
var_names <- c("age_dx", "stage", "residual_disease", "cellularity",
               "necrosis", "brca_v1", "race_v1", "cd8", "anatomical_site")
var_desc <- c("Age at Diagnosis", "Stage", "Residual Disease", "Cellularity",
              "Necrosis", "BRCA1/BRCA2", "Race", "CD8", "Anatomical Site")
d_cohort_adnexal_all <- d %>%
  dplyr::transmute(final, !!!rlang::syms(var_names)) %>% 
  dplyr::filter(anatomical_site %in% c("adnexal", "presumed adnexal"))
uni_ass_adnexal_all <- d_cohort_adnexal_all %>% 
  Amisc::describeBy(var_names, var_desc, "final", dispersion = "sd", digits = 1, p.digits = 2) %>% 
  dplyr::mutate(PValue = ifelse(PValue %in% c("0", "0.00"), "< 0.001", PValue))
pandoc.table(uni_ass_adnexal_all, split.tables = Inf,
             caption = "Cohort characteristics for all cases by subtype")

## ----cohort_characteristics_adnexal--------------------------------------
d_cohort_adnexal_only <- d_cohort_adnexal_all %>% 
  dplyr::filter(anatomical_site == "adnexal")
var_names_adnexal_only <- var_names %>% purrr::discard(~ . == "anatomical_site")
var_desc_adnexal_only <- var_desc %>% purrr::discard(~ . == "Anatomical Site")
uni_ass_adnexal_only <- d_cohort_adnexal_only %>% 
  Amisc::describeBy(var_names_adnexal_only, var_desc_adnexal_only, "final", dispersion = "sd", digits = 1, p.digits = 2) %>% 
  dplyr::mutate(PValue = ifelse(PValue %in% c("0", "0.00"), "< 0.001", PValue))
pandoc.table(uni_ass_adnexal_only, split.tables = Inf,
             caption = "Cohort characteristics for all known ovarian site by subtype")

## ----follow_up-----------------------------------------------------------
# Calculate follow-up times summarized with the median
fu_times <- function(data, time, status, event, group = NULL, digits = 1) {
  if (!is.null(group)) {
    data <- dplyr::group_by(data, .data[[group]])
  }
  fu_dat <- data %>% 
    dplyr::summarize(
      `Observation Time` = median(.data[[time]], na.rm = TRUE),
      `Censoring Time` = median(.data[[time]][.data[[status]] == event], na.rm = TRUE),
      `Reverse KM` = survfit(Surv(.data[[time]], ifelse(.data[[status]] == event, 0, 1)) ~ 1) %>% 
        broom::glance() %>% 
        purrr::pluck("median"),
      Events = sum(.data[[status]] == event, na.rm = TRUE)
    ) %>% 
    dplyr::mutate_if(is.numeric, round, digits = digits)
  if (!is.null(group)) {
    dplyr::rename(fu_dat, Class = group)
  } else {
    tibble::add_column(fu_dat, Class = "Full Cohort", .before = 1)
  }
}

## ----times_total---------------------------------------------------------
os_args <- list(time = "os_yrs", status = "os_sts", event = "os.event")
pfs_args <- list(time = "pfs_yrs", status = "pfs_sts", event = "pfs.event")

os_full <- purrr::invoke(fu_times, os_args, data = d)
pfs_full <- purrr::invoke(fu_times, pfs_args, data = d)

os_conc <- purrr::invoke(fu_times, os_args, data = conc)
pfs_conc <- purrr::invoke(fu_times, pfs_args, data = conc)

## ----times_final_os------------------------------------------------------
times_final_os <- os_full %>% 
  rbind(purrr::invoke(fu_times, os_args, data = d, group = "final"))
pandoc.table(
  times_final_os,
  caption = "OS Median Follow-up Time in Years by Final Predictions"
)

## ----times_final_pfs-----------------------------------------------------
times_final_pfs <- pfs_full %>% 
  rbind(purrr::invoke(fu_times, pfs_args, data = d, group = "final"))
pandoc.table(
  times_final_pfs,
  caption = "PFS Median Follow-up Time in Years by Final Predictions"
)

## ----surv_params_C03-----------------------------------------------------
# Common km arguments (Display legend only for top (OS) survival plot)
km_args <- list(title = c("Overall Survival", "Progression-Free Survival"),
                legend = c(list(c(0.9, 0.9)), "none"))

# Common survival plot arguments
surv_args <- list(
  data = d,
  conf.int = TRUE,
  pval = TRUE,
  pval.method = TRUE,
  risk.table = TRUE,
  risk.table.height = 1/3,
  xlab = "Time in Years",
  ylab = "Survival Probability",
  legend.title = "Class",
  legend.labs = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")
)

# Common combine plot arguments
comb_args <- list(print = FALSE, ncol = 1, nrow = 2)

## ----km_final_aa, fig.width=7, fig.height=10-----------------------------
# Filter for all adnexal sites
d_aa <- dplyr::filter(d, anatomical_site %in% c("adnexal", "presumed adnexal"))
surv_args_aa <- purrr::map_at(surv_args, "data", ~ d_aa)

km_final_aa <- purrr::list_merge(
  km_args,
    fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ final, data = d_aa),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ final, data = d_aa)
  )
)
plots_km_final_aa <- km_final_aa %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args_aa, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "Adnexal and Presumed Adnexal Sites OTTA (Final Model)")
ggsave(file.path(fig_path, "km_final_adnexal_all.pdf"), plots_km_final_aa, width = 7, height = 10)

## ----km_final_ka, fig.width=7, fig.height=10-----------------------------
# Filter for known adnexal sites
d_ka <- dplyr::filter(d, anatomical_site == "adnexal")
surv_args_ka <- purrr::map_at(surv_args, "data", ~ d_ka)

km_final_ka <- purrr::list_merge(
  km_args,
    fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ final, data = d_ka),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ final, data = d_ka)
  )
)
plots_km_final_ka <- km_final_ka %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args_ka, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "Adnexal Sites OTTA (Final Model)")
ggsave(file.path(fig_path, "km_final_adnexal_known.pdf"), plots_km_final_ka, width = 7, height = 10)

## ----rm_rplots_D03-------------------------------------------------------
file.remove(here::here("Rplots.pdf"))

## ----mvs_dat-------------------------------------------------------------
# Multivariate survival data sources
mvs_dat <- d %>% 
  dplyr::transmute(
    final = forcats::fct_relevel(final, "C1.MES", after = Inf),
    residual_disease,
    brca_v1,
    age_dx,
    stage,
    anatomical_site,
    cd8,
    os_yrs,
    os_sts,
    pfs_yrs,
    pfs_sts
  ) %>% 
  as.data.frame()
mvs_dat_adnexal_all <- mvs_dat %>% 
  dplyr::filter(anatomical_site %in% c("adnexal", "presumed adnexal"))
mvs_dat_adnexal_only <- mvs_dat %>% 
  dplyr::filter(anatomical_site == "adnexal")

# Multivarite survival arguments
mvs_args <- list(
  stat.test = "logtest",
  var.names.surv.time = c("os_yrs", "pfs_yrs"),
  var.names.surv.status = c("os_sts", "pfs_sts"),
  event.codes.surv = c("os.event", "pfs.event"),
  surv.descriptions = c("OS", "PFS"),
  show.var.detail = TRUE,
  show.group.name.for.bin.var = TRUE,
  round.small = TRUE,
  caption = NULL
)

# Covariate variable arguments
var_all <- purrr::set_names(
  c("final", "age_dx", "stage", "cd8", "residual_disease",  "brca_v1"),
  c("Final Subtype", "Age", "Stage", "CD8", "Residual Disease", "BRCA1/BRCA2")
)
var_lists <- tibble::lst(
  var_base = purrr::keep(var_all, ~ . %in% c("final", "age_dx", "stage")),
  var_add_cd8 = c(var_base, var_all["CD8"]),
  var_add_resdx = c(var_add_cd8, var_all["Residual Disease"]),
  var_add_brca = c(var_add_resdx, var_all["BRCA1/BRCA2"])
)

# Base title
title_base <- "Multivariable survival analysis of overall and progression-free survival, adjusting for"

## ----mvs_all-------------------------------------------------------------
# Multivariable Survival for all adexnal sites
mvs_all <- purrr::invoke_map(
  .f = biostatUtil::doCoxphMultivariable,
  .x = var_lists %>% purrr::map(
    ~ purrr::splice(
      mvs_args,
      var.names = .,
      var.descriptions = names(.)
    )
  ),
  input.d = mvs_dat_adnexal_all
) %>% 
  purrr::set_names(names(var_lists))

# Titles for all adnexal sites
titles_all <- var_lists %>%
  purrr::map( ~ {
    paste(title_base,
          paste(names(.), collapse = ", "),
          "(known and presumed adnexal sites)")
  })

## ----mvs_all_base--------------------------------------------------------
cat(purrr::pluck(mvs_all, "var_base", "result.table.bamboo"))

## ----mvs_all_add_cd8-----------------------------------------------------
cat(purrr::pluck(mvs_all, "var_add_cd8", "result.table.bamboo"))

## ----mvs_all_add_resdx---------------------------------------------------
cat(purrr::pluck(mvs_all, "var_add_resdx", "result.table.bamboo"))

## ----mvs_all_add_brca----------------------------------------------------
cat(purrr::pluck(mvs_all, "var_add_brca", "result.table.bamboo"))

## ----mvs_known-----------------------------------------------------------
# Multivariable Survival for known adexnal sites
mvs_known <- purrr::invoke_map(
  .f = biostatUtil::doCoxphMultivariable,
  .x = var_lists %>% purrr::map(
    ~ purrr::splice(
      mvs_args,
      var.names = .,
      var.descriptions = names(.)
    )
  ),
  input.d = mvs_dat_adnexal_only
) %>% 
  purrr::set_names(names(var_lists))

# Titles for known adnexal sites
titles_known <- var_lists %>%
  purrr::map( ~ {
    paste(title_base,
          paste(names(.), collapse = ", "),
          "(known adnexal sites)")
  })

## ----mvs_known_base------------------------------------------------------
cat(purrr::pluck(mvs_known, "var_base", "result.table.bamboo"))

## ----mvs_known_add_cd8---------------------------------------------------
cat(purrr::pluck(mvs_known, "var_add_cd8", "result.table.bamboo"))

## ----mvs_known_add_resdx-------------------------------------------------
cat(purrr::pluck(mvs_known, "var_add_resdx", "result.table.bamboo"))

## ----mvs_known_add_brca--------------------------------------------------
cat(purrr::pluck(mvs_known, "var_add_brca", "result.table.bamboo"))


