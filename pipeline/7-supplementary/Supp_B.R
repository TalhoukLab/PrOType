## ----setup_B, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
params <- list(outputDir = outputDir)

## ----child="Supp_B06.Rmd"------------------------------------------------

## ----setup_B06, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_B06------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

arl_dat <-
  readxl::read_excel(here("data/nstring/Nanostring_ARL-all samples-all gnes_20180607.xlsx"))
load(here("data/supp/expCS.rda"))
load(here("data/supp/exp_CS3_all.rda"))
load(here("data/supp/cs3.norm.rda"))

fig_path <- file.path(params$outputDir, "supplementary/figures/B06")

## ----qc_filter-----------------------------------------------------------
# Filter QC samples
exp_CS3_final <- exp_CS3_all %>%
  dplyr::mutate(
    ottaID = magrittr::inset(
      ottaID,
      match("X20151128_OTTA047AOCSH_TAOC10308_08", File.Name),
      "TAOC10303"
    ),
    lodFlag = factor(ifelse(pergd < 50, "Failed", "Passed")),
    snFlag = factor(ifelse(sn < 170, "Failed", "Passed"))
  ) %>%
  dplyr::anti_join(arl_dat, by = "File.Name")

## ----qc_table------------------------------------------------------------
# QC Table flag counts
counts <- exp_CS3_final %>%
  dplyr::group_by(nanostring.site) %>%
  dplyr::summarize(
    `Imaging Failures` = sum(imagingFlag == "Failed"),
    `Linearity Failures` = sum(linFlag == "Failed"),
    `Smallest PC Failures` = sum(spcFlag == "Failed"),
    `Limit of Detection Failures` = sum(lodFlag == "Failed"),
    `Signal to Noise Failures` = sum(snFlag == "Failed"),
    `Sample Quality Failures` = sum(normFlag == "Failed"),
    `Overall QC Failures` = sum(QCFlag == "Failed")
  ) %>% 
  as.data.frame() %>% 
  tibble::column_to_rownames("nanostring.site") %>% 
  cbind(`Total Samples Run` = as.numeric(table(exp_CS3_final[["nanostring.site"]])), .) %>% 
  rbind(Total = colSums(.), .) %>% 
  t()

# QC Table flag percentages
pcts <- rbind(
  `Total Samples Run` = round(counts[1, ] / counts[1, 1], 3) * 100,
  round(counts[-1, ] %*% diag(1 / counts[1, ]), 3) * 100
) %>% 
  apply(1:2, function(x) paste0("(", format(x, nsmall = 1), "%)"))

# Combined counts and pcts. Custom bolding and line break
counts[] <- matrix(paste(counts, pcts))
rownames(counts)[c(1, 7, 8)] <- paste0("**", rownames(counts)[c(1, 7, 8)], "**")
counts <- rbind(counts[1:6, ], "<br>", counts[7:8, ])

pander::pandoc.table(
  counts,
  justify = "lrrrr",
  emphasize.rownames = FALSE,
  split.tables = Inf
)

## ----snr_vs_pergd, fig.width=7, fig.height=5-----------------------------
# SNR vs %GD
p <- ggplot(exp_CS3_all, aes(sn, pergd, color = nanostring.site)) +
  geom_point(na.rm = TRUE) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_vline(xintercept = 170, color = "brown", size = 1) +
  lims(x = c(0, 5000)) +
  labs(x = "Signal/Noise Ratio",
       y = "Percent of Genes Detected",
       title = "Signal/Noise vs % Genes Detected") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_discrete(
    name = "Site",
    labels = c("Melbourne (AOC)", "Los Angeles (USC)", "Vancouver (UBC)")
  )

# print(p)
ggsave(file.path(fig_path, "snr_vs_pergd.pdf"), p, width = 7, height = 5)

## ----pools_data----------------------------------------------------------
# Pools data
pools <- expCS %>% 
  dplyr::filter(sample.type %in% c("XSITE.ORIG", "XSITE.FOR", "POOL"),
                geneRLF == "CS3") %>% 
  dplyr::mutate(ref.type = factor(dplyr::case_when(
    POOL1 == "Yes" ~ "POOL 1",
    POOL2 == "Yes" ~ "POOL 2",
    POOL3 == "Yes" ~ "POOL 3",
    cross.site.control == "Yes" ~ "Cross Site",
    TRUE ~ NA_character_
  ))) %>% 
  dplyr::filter(ref.type != "Cross Site") %>% 
  dplyr::mutate(
    nanostring.date = as.Date(nanostring.date),
    averageHK = log2(averageHK),
    nanostring.site = factor(dplyr::case_when(
      nanostring.site == "AOC" ~ "Melbourne",
      nanostring.site == "USC" ~ "Los Angeles",
      TRUE ~ "Vancouver"
    ), levels = c("Melbourne", "Los Angeles", "Vancouver"))
  )

## ----pools_gx_time, fig.width=7, fig.height=5----------------------------
# Reference pools gx across site and time
p <- ggplot(pools, aes(nanostring.date, averageHK, colour = nanostring.site)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(vars(ref.type), vars(nanostring.site)) +
  labs(x = "Time",
       y = "Average Expression (log2)",
       title = "CodeSet3 Reference Pool Expression across Site and Time") +
  lims(y = c(13, 19)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_discrete(
    name = "Site",
    labels = c("Melbourne (AOC)", "Los Angeles (USC)", "Vancouver (UBC)")
  )

# print(p)
ggsave(file.path(fig_path, "pools_gx_time.pdf"), p, width = 7, height = 5)

## ----cross-site----------------------------------------------------------
# Cross site
cross.site <- expCS %>% 
  dplyr::filter(sample.type %in%
                  c("Cross site", "XSITE.ORIG", "XSITE.FOR", "POOL")) %>% 
  dplyr::mutate(ref.type = factor(dplyr::case_when(
    POOL1 == "Yes" ~ "POOL 1",
    POOL2 == "Yes" ~ "POOL 2",
    POOL3 == "Yes" ~ "POOL 3",
    cross.site.control == "Yes" ~ "Cross Site",
    TRUE ~ NA_character_
  ))) %>% 
  dplyr::filter(geneRLF == "CS3",
                ref.type == "Cross Site",
                summaryID != "TVAN20681") %>% 
  dplyr::mutate(nanostring.site = factor(dplyr::case_when(
    nanostring.site == "AOC" ~ "Melbourne",
    nanostring.site == "USC" ~ "Los Angeles",
    TRUE ~ "Vancouver"
  )))

## ----cross-site-each-site------------------------------------------------
# Cross-site for each site
VanRefs.gx <- cross.site %>% 
  dplyr::filter(nanostring.site == "Vancouver") %>% 
  dplyr::arrange(summaryID) %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

AOCRefs.gx <- cross.site %>% 
  dplyr::filter(nanostring.site == "Melbourne") %>% 
  dplyr::arrange(summaryID) %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

USCRefs.gx <- cross.site %>% 
  dplyr::filter(nanostring.site == "Los Angeles") %>% 
  dplyr::arrange(summaryID) %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

## ----pools-each-site-----------------------------------------------------
# Pools for each site
vanpools <- pools %>% 
  dplyr::filter(nanostring.site == "Vancouver") %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

AOCpools <- pools %>% 
  dplyr::filter(nanostring.site == "Melbourne") %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

USCpools <- pools %>% 
  dplyr::filter(nanostring.site == "Los Angeles") %>% 
  dplyr::pull(File.Name) %>% 
  dplyr::select(cs3.norm, .)

## ----norm----------------------------------------------------------------
# Everything is calibrated to Vancouver
AOCRefs.gx2 <- 
  t(nanostringr::refMethod(t(AOCRefs.gx), t(vanpools), t(AOCpools)))
USCRefs.gx2 <-
  t(nanostringr::refMethod(t(USCRefs.gx), t(vanpools), t(USCpools)))

## ----cc_combine----------------------------------------------------------
# Combinations of cross-site gene expression
sites <- c("USC", "AOC", "VAN")
all_xsites <- combn(sites, 2) %>% 
  tibble::as_tibble() %>% 
  purrr::set_names(purrr::map_chr(., paste, collapse = "_vs_"))

# Combined gene expression
all_gx <- list(USCRefs.gx2, AOCRefs.gx2, VanRefs.gx) %>% 
  purrr::set_names(sites) %>% 
  purrr::map(~ as.data.frame(t(.)))

# Concordance measures for all genes averaged across samples
all_metrics <- all_xsites %>% 
  purrr::imap_dfr(~ {
    purrr::pmap_dfr(all_gx[.x], ~ {
      R2 <- cor(.x, .y) ^ 2
      ccc <- epiR::epi.ccc(.x, .y)
      Ca <- purrr::pluck(ccc, "C.b")
      Rc <- purrr::pluck(ccc, "rho.c", "est")
      tibble::lst(R2, Ca, Rc)
    }) %>% 
      dplyr::mutate(Sites = .y)
  }) %>% 
  tidyr::gather(key = "Metric", value = "Expression", -Sites)

## ----cc_hist, fig.width=7, fig.height=5----------------------------------
# Plot all combinations of cross-site concordance measure histograms
p <- ggplot(all_metrics, aes(Expression)) +
  geom_histogram(bins = 30, fill = "blue") +
  facet_grid(rows = vars(Sites), cols = vars(Metric), scales = "free_x") +
  labs(y = "Count",
       title = "Cross-Site Concordance Measure Distributions") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# print(p)
ggsave(file.path(fig_path, "cc_hist.pdf"), p, width = 7, height = 5)

## ----avg_scatter, fig.width=7, fig.height=10, results='hide'-------------
# Combined averaged gene expression
all_gx_avg <- purrr::map_dfr(all_gx, rowMeans)
pdf(file.path(fig_path, "avg_scatter.pdf"), width = 7, height = 10)
par(mfrow = c(3, 1))
purrr::walk(all_xsites, ~ {
  nanostringr::CCplot(
    method1 = all_gx_avg[[.[1]]],
    method2 = all_gx_avg[[.[2]]],
    xlabel = .[1],
    ylabel = .[2],
    title = paste("Average Concordance Correlation for", .[1], "vs", .[2]),
    Ptype = "scatter"
  )
})
dev.off()
# dev.print(pdf, file.path(fig_path, "avg_scatter.pdf"), width = 7, height = 10)


## ----child="Supp_B07.Rmd"------------------------------------------------

## ----setup_B07, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_B07------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(survival)
  library(survminer)
  library(ggplot2)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", FALSE)
panderOptions("table.split.table", Inf)
n_genes <- 55
grm <- "CTHRC1"

raw_ns <- load_nanostring()
arl_samples <- readxl::read_excel(here("data/nstring/Nanostring_ARL-all samples-all gnes_20180607.xlsx"))
all_pred <- readr::read_csv(here("data/nstring/predictions.csv"),
                            col_types = list("X1" = readr::col_skip()))
sum_freq <- readr::read_csv(file.path(params$outputDir, "gene_selection/sum_freq/overall_freq.csv"),
                            col_types = readr::cols())
array_probs <- readr::read_csv(file.path(params$outputDir, "nanostring/predictions/aa_probs.csv"),
                               col_types = readr::cols())
tcga_probs <- readr::read_csv(file.path(params$outputDir, "nanostring/predictions/tcga_probs.csv"),
                              col_types = readr::cols())
final_preds <- readr::read_csv(file.path(params$outputDir, "gene_selection/final_model/Final_Predictions.csv"),
                               col_types = list("X1" = readr::col_skip()))
final_model <- readRDS(file.path(params$outputDir, "gene_selection/final_model/final_model.rds"))
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

fig_path <- file.path(params$outputDir, "supplementary/figures/B07")

## ----aa_tcga_table-------------------------------------------------------
pred_tab <- all_pred %>% 
  dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]]) %>% 
  with(., table(
    `Predicted Array` = make.names(Adaboost.xpn),
    `Predicted TCGA` = make.names(TCGA.Predicted.Subtype)
  )) %>% 
  caret::confusionMatrix()

## ----aa_tcga_confmat-----------------------------------------------------
pandoc.table(ftable(pred_tab[["table"]]))

## ----aa_tcga_overall_metrics---------------------------------------------
ov_metrics <- confmat_metrics(pred_tab, metrics = "overall")
pandoc.table(ov_metrics)

## ----aa_tcga_by-class_metrics--------------------------------------------
bc_metrics <- confmat_metrics(pred_tab, metrics = "byclass")
pandoc.table(bc_metrics, keep.trailing.zeros = TRUE)

## ----entropy_compute-----------------------------------------------------
# Merge probabilities and predictions
compare_probs <-
  dplyr::inner_join(array_probs,
                    tcga_probs,
                    by = "ottaID",
                    suffix = c("_array", "_tcga")) %>% 
  dplyr::inner_join(all_pred, by = "ottaID") %>%
  dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]]) %>% 
  dplyr::rename(predicted_array = Adaboost.xpn,
                predicted_tcga = TCGA.Predicted.Subtype) %>%
  dplyr::mutate_at(c("predicted_array", "predicted_tcga"), make.names)

# Compute entropies
entropy_probs <- compare_probs %>%
  dplyr::transmute(
    ottaID,
    entropy_array = apply(.[grep("(?<!predicted)_array", names(.), perl = TRUE)], 1,
                          entropy::entropy, unit = "log2"),
    entropy_tcga = apply(.[grep("(?<!predicted)_tcga", names(.), perl = TRUE)], 1,
                         entropy::entropy, unit = "log2"),
    predicted_array,
    predicted_tcga,
    match = factor(
      ifelse(predicted_array == predicted_tcga, "Consensus", "Non-Consensus"),
      levels = c("Consensus", "Non-Consensus")
    )
  ) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("entropy")), round, digits = 3)

## ----mwu_aa--------------------------------------------------------------
mwu_aa <- wilcox.test(entropy_array ~ match, entropy_probs) %>%
  broom::tidy() %>% 
  dplyr::mutate(p.value = scales::pvalue(p.value, accuracy = 1e-4))
pandoc.table(mwu_aa)

## ----mwu_tcga------------------------------------------------------------
mwu_tcga <- wilcox.test(entropy_tcga ~ match, entropy_probs) %>%
  broom::tidy() %>% 
  dplyr::mutate(p.value = scales::pvalue(p.value, accuracy = 1e-4))
pandoc.table(mwu_tcga)

## ----scatterplot_args----------------------------------------------------
aa_vs_tcga_args <- list(
  labs(x = "Adaboost - All Array",
       y = "Random Forest - TCGA"),
  theme_bw()
)

## ----entropy_scatterplot_agree, fig.width=7, fig.height=5----------------
p <- ggplot(entropy_probs, aes(entropy_array, entropy_tcga, color = match)) +
  geom_point(alpha = 0.5) +
  ggtitle("Entropy on full NanoString data by model agreement") +
  scale_color_brewer(name = NULL, palette = "Set2", drop = FALSE) +
  aa_vs_tcga_args

# print(p)
ggsave(file.path(fig_path, "entropy_scatterplot_agree.pdf"), p, width = 7, height = 5)

## ----entropy_scatterplot_pred_aa, fig.width=7, fig.height=5--------------
p <- ggplot(entropy_probs, aes(entropy_array, entropy_tcga,
                               color = predicted_array)) +
  geom_point(alpha = 0.5) +
  ggtitle("Entropy on full NanoString data by predicted all array class") +
  scale_color_discrete(name = "All Array Model Labels", drop = FALSE) +
  aa_vs_tcga_args

# print(p)
ggsave(file.path(fig_path, "entropy_scatterplot_pred_aa.pdf"), p, width = 7, height = 5)

## ----entropy_scatterplot_pred_tcga, fig.width=7, fig.height=5------------
p <- ggplot(entropy_probs, aes(entropy_array, entropy_tcga,
                               color = predicted_tcga)) +
  geom_point(alpha = 0.5) +
  ggtitle("Entropy on full NanoString data by predicted TCGA class") +
  scale_color_discrete(name = "TCGA Model Labels", drop = FALSE) +
  aa_vs_tcga_args

# print(p)
ggsave(file.path(fig_path, "entropy_scatterplot_pred_tcga.pdf"), p, width = 7, height = 5)

## ----entropy_boxplot-----------------------------------------------------
entropy_probs_grouped <- entropy_probs %>% 
  tidyr::gather(key = Data, value = Entropy, 2:3) %>% 
  tidyr::gather(key = Prediction, value = Subtype, 2:3) %>% 
  dplyr::mutate(Data = factor(
    dplyr::case_when(
      Data == "entropy_array" ~ "All Array",
      Data == "entropy_tcga" ~ "TCGA",
      TRUE ~ NA_character_
    ),
    levels = c("All Array", "TCGA")
  ))

## ----entropy_boxplot_agree, fig.width=7, fig.height=5--------------------
p <- ggplot(entropy_probs_grouped, aes(Data, Entropy, fill = match)) +
  geom_boxplot() +
  ggtitle("Entropy Comparison of Prediction Labels by Data") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_boxplot_agree.pdf"), p, width = 7, height = 5)

## ----entropy_boxplot_pred_aa, fig.width=7, fig.height=5------------------
p <- entropy_probs_grouped %>% 
  dplyr::filter(Data == "All Array") %>% 
  ggplot(aes(Subtype, Entropy, fill = match)) +
  geom_boxplot() +
  ggtitle("Entropy Comparison of All Array Prediction Labels by Subtype") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_boxplot_pred_aa.pdf"), p, width = 7, height = 5)

## ----entropy_boxplot_pred_tcga, fig.width=7, fig.height=5----------------
p <- entropy_probs_grouped %>% 
  dplyr::filter(Data == "TCGA") %>% 
  ggplot(aes(Subtype, Entropy, fill = match)) +
  geom_boxplot() +
  ggtitle("Entropy Comparison of TCGA Prediction Labels by Subtype") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_boxplot_pred_tcga.pdf"), p, width = 7, height = 5)

## ----clin_combine_B07----------------------------------------------------
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

## ----calculate-survival_B07----------------------------------------------
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
    BRCA1_2 = dplyr::case_when(
      brca1_2 %in% c("4", "6", "WT") ~ "all wildtypes",
      brca1_2 %in% c("1", "BRCA 1") ~ "pathogenic BRCA1 mutation",
      brca1_2 == "2" ~ "pathogenic BRCA2 mutation",
      brca1_2 %in% c("3", "BRCA 1 and BRCA 2") ~ "pathogenic NOS",
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

## ----surv_params_B07-----------------------------------------------------
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

## ----km_all_array, fig.width=7, fig.height=10----------------------------
km_aa <- purrr::list_merge(
  km_args,
  fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ array_preds, data = d),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ array_preds, data = d)
  )
)
plots_km_aa <- km_aa %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "All OTTA (All-Array Model)")
ggsave(file.path(fig_path, "km_all_array.pdf"), plots_km_aa, width = 7, height = 10)

## ----km_tcga, fig.width=7, fig.height=10---------------------------------
km_tcga <- purrr::list_merge(
  km_args,
  fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ TCGA_preds, data = d),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ TCGA_preds, data = d)
  )
)
plots_km_tcga <- km_tcga %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "All OTTA (TCGA Model)")
ggsave(file.path(fig_path, "km_tcga.pdf"), plots_km_tcga, width = 7, height = 10)

## ----km_conc, fig.width=7, fig.height=10---------------------------------
km_conc <- purrr::list_merge(
  km_args,
  fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ array_preds, data = conc),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ array_preds, data = conc)
  )
)
plots_km_conc <- km_conc %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "Concordant Samples")
ggsave(file.path(fig_path, "km_conc.pdf"), plots_km_conc, width = 7, height = 10)

## ----km_disc_all_array, fig.width=7, fig.height=10-----------------------
km_disc_aa <- purrr::list_merge(
  km_args,
  fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ array_preds, data = disc),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ array_preds, data = disc)
  )
)
plots_km_disc_aa <- km_disc_aa %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "Discordant Samples (All-Array Model)")
ggsave(file.path(fig_path, "km_disc_all_array.pdf"), plots_km_disc_aa, width = 7, height = 10)

## ----km_disc_tcga, fig.width=7, fig.height=10----------------------------
km_disc_tcga <- purrr::list_merge(
  km_args,
  fit = list(
    survfit(Surv(os_yrs10, os_sts10 == "os.event") ~ TCGA_preds, data = disc),
    survfit(Surv(pfs_yrs10, pfs_sts10 == "pfs.event") ~ TCGA_preds, data = disc)
  )
)
plots_km_disc_tcga <- km_disc_tcga %>% 
  purrr::pmap(~ purrr::invoke(ggsurvplot, surv_args, title = ..1, legend = ..2, fit = ..3)) %>% 
  purrr::invoke(arrange_ggsurvplots, comb_args, x = ., title = "Discordant Samples (TCGA Model)")
ggsave(file.path(fig_path, "km_disc_tcga.pdf"), plots_km_disc_tcga, width = 7, height = 10)

## ----rm_rplots_B07-------------------------------------------------------
file.remove(here::here("Rplots.pdf"))


## ----child="Supp_B08.Rmd"------------------------------------------------

## ----setup_B08, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_B08------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

panderOptions("keep.trailing.zeros", FALSE)
panderOptions("table.split.table", Inf)

eval_overlap_all <- readRDS(file.path(params$outputDir, "nanostring/evals/eval_overlap_all.rds"))
eval_consensus <- readRDS(file.path(params$outputDir, "nanostring/evals/eval_consensus.rds"))

## ----aa_ns---------------------------------------------------------------
aa_ns_pred <- eval_overlap_all %>% 
  purrr::pluck("adaboost", "array_vs_nstring", "confmat")

## ----aa_ns_confmat-------------------------------------------------------
aa_ns_tab <- aa_ns_pred %>% 
  purrr::pluck("table")
names(dimnames(aa_ns_tab)) <- c("Predicted from NanoString",
                                "Predicted from Array")
pandoc.table(ftable(aa_ns_tab))

## ----aa_ns_overall_metrics-----------------------------------------------
aa_ns_ov <- confmat_metrics(aa_ns_pred, metrics = "overall")
pandoc.table(aa_ns_ov)

## ----aa_ns_by-class_metrics----------------------------------------------
aa_ns_bc <- confmat_metrics(aa_ns_pred, metrics = "byclass")
pandoc.table(aa_ns_bc, keep.trailing.zeros = TRUE)

## ----tcga_ns_confmat-----------------------------------------------------
tcga_ns_tab <- matrix(
  c(21, 2, 1, 2, 1, 7, 2, 1, 1, 7, 19, 1, 0, 1, 1, 18),
  nrow = 4, 
  byrow = TRUE,
  dimnames = list(
    `Predicted from NanoString` = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO"),
    `Predicted from Array` = c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO")
  )
) %>% 
  as.table()
tcga_ns_confmat <- caret::confusionMatrix(tcga_ns_tab)
pandoc.table(ftable(tcga_ns_confmat[["table"]]))

## ----tcga_ns_overall_metrics---------------------------------------------
tcga_ns_confmat_ov_metrics <- confmat_metrics(tcga_ns_confmat, metrics = "overall")
pandoc.table(tcga_ns_confmat_ov_metrics)

## ----tcga_ns_by-class_metrics--------------------------------------------
tcga_ns_confmat_bc_metrics <- confmat_metrics(tcga_ns_confmat, metrics = "byclass")
pandoc.table(tcga_ns_confmat_bc_metrics, keep.trailing.zeros = TRUE)

## ----cons_pub------------------------------------------------------------
cons_pred <- eval_consensus %>% 
  purrr::pluck("adaboost", "published_vs_consensus", "confmat")

## ----cons_pub_confmat----------------------------------------------------
cons_tab <- cons_pred %>% 
  purrr::pluck("table")
names(dimnames(cons_tab)) <- c("Consensus Labels", "Published Labels")
pandoc.table(ftable(cons_tab))

## ----cons_pub_overall_metrics--------------------------------------------
cons_ov <- confmat_metrics(cons_pred, metrics = "overall")
pandoc.table(cons_ov)

## ----cons_pub_by-class_metrics-------------------------------------------
cons_bc <- confmat_metrics(cons_pred, metrics = "byclass")
pandoc.table(cons_bc, keep.trailing.zeros = TRUE)


