## ----setup_C, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
params <- list(outputDir = outputDir)

## ----child="Supp_C01.Rmd"------------------------------------------------

## ----setup_C01, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_C01------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(pander)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

raw_ns <- load_nanostring()
arl_samples <- readxl::read_excel(here("data/nstring/Nanostring_ARL-all samples-all gnes_20180607.xlsx"))
all_pred <- readr::read_csv(here("data/nstring/predictions.csv"),
                            col_types = list("X1" = readr::col_skip()))

## ----data_breakdown------------------------------------------------------
counts <- raw_ns %>% 
  dplyr::rename(ottaID = OTTA.ID) %>% 
  dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]]) %>% 
  dplyr::inner_join(all_pred, by = "ottaID") %>% 
  dplyr::group_by(cut) %>% 
  dplyr::summarize(
    studies = dplyr::n_distinct(site),
    n = n(),
    agreement = sum(Adaboost.xpn == TCGA.Predicted.Subtype)
  )
pandoc.table(counts,
             caption = "Summary of All-Array and TCGA studies, cases, and gold-standard labels by dataset")


## ----child="Supp_C02.Rmd"------------------------------------------------

## ----setup_C02, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_C02------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

pdf_files <- fs::dir_ls(
  path = file.path(params$outputDir, "gene_selection/plots"),
  regexp = "pdf$"
)

## ----reorder_C02---------------------------------------------------------
# order used in C02
filenames <- c(
  "LOSO_accuracy",
  "LOSO_F1",
  "Accuracy_boxplots",
  "top_genes_heatmaps"
)
embed_files <- pdf_files[match(filenames, fs::path_ext_remove(fs::path_file(pdf_files)))]

## ----graphics_C02, fig.show='asis'---------------------------------------
knitr::include_graphics(embed_files)


## ----child="Supp_C03.Rmd"------------------------------------------------

## ----setup_C03, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_C03------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))

pdf_files <- fs::dir_ls(
  path = file.path(params$outputDir, "gene_selection/plots"),
  regexp = "pdf$"
)

## ----reorder_C03---------------------------------------------------------
# order used in C03
filenames <- c(
  "Accuracy_cut2",
  "F1_cut2_byclass"
)
embed_files <- pdf_files[match(filenames, fs::path_ext_remove(fs::path_file(pdf_files)))]

## ----graphics_C03, fig.show='asis'---------------------------------------
knitr::include_graphics(embed_files)


## ----child="Supp_C04.Rmd"------------------------------------------------

## ----setup_C04, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_C04------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(randomForest)
  library(pander)
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

fig_path <- file.path(params$outputDir, "supplementary/figures/C04")

## ----pred_labs, include=FALSE--------------------------------------------
# Prediction data
pred_labs <- load_prediction_labels(raw_ns)
preds_new <- pred_labs[["preds_new"]]

# Final gene list
final_glist <- sum_freq %>%
  dplyr::arrange(dplyr::desc(rfFreq), dplyr::desc(lassoFreq)) %>%
  dplyr::pull(genes) %>%
  make.names() %>%
  purrr::discard(~ . %in% grm) %>%
  head(n_genes)

## ----cut1, include=FALSE-------------------------------------------------
# Define cut1
cut1 <- define_batch(preds_new, raw_ns, batch = "b1")
cut1_dat <- cut1[["dat"]]
cut1_lab <- cut1[["lab"]]

x.new <- sl_data(cut1_dat)[final_glist]
y.new <- sl_class(cut1_lab, x.new)

cut1_cm <- cut1_lab %>%
  dplyr::transmute(
    `Predicted with Final Model` = splendid::prediction(final_model, x.new, y.new),
    `Consensus Label` = Adaboost.xpn
  ) %>%
  table() %>%
  caret::confusionMatrix()

## ----cut1_confmat--------------------------------------------------------
pandoc.table(ftable(cut1_cm[["table"]]))

## ----cut1_overall_metrics------------------------------------------------
cut1_ov_metrics <- confmat_metrics(cut1_cm, metrics = "overall")
pandoc.table(cut1_ov_metrics)

## ----cut1_by-class_metrics-----------------------------------------------
cut1_bc_metrics <- confmat_metrics(cut1_cm, metrics = "byclass")
pandoc.table(cut1_bc_metrics, keep.trailing.zeros = TRUE)

## ----cut2, include=FALSE-------------------------------------------------
# Define cut2 excluding overlap
cut2 <- define_batch(preds_new, raw_ns, batch = "b2")
cut2_dat <- cut2[["dat"]]
cut2_lab <- cut2[["lab"]]

x.new <- sl_data(cut2_dat)[final_glist]
y.new <- sl_class(cut2_lab, x.new)

cut2_cm <- cut2_lab %>%
  dplyr::transmute(
    `Predicted with Final Model` = splendid::prediction(final_model, x.new, y.new),
    `Consensus Label` = Adaboost.xpn
  ) %>%
  table() %>%
  caret::confusionMatrix()

## ----cut2_confmat--------------------------------------------------------
pandoc.table(ftable(cut2_cm[["table"]]))

## ----cut2_overall_metrics------------------------------------------------
cut2_ov_metrics <- confmat_metrics(cut2_cm, metrics = "overall")
pandoc.table(cut2_ov_metrics)

## ----cut2_by-class_metrics-----------------------------------------------
cut2_bc_metrics <- confmat_metrics(cut2_cm, metrics = "byclass")
pandoc.table(cut2_bc_metrics, keep.trailing.zeros = TRUE)

## ----cut3, include=FALSE-------------------------------------------------
# Define cut3
cut3 <- define_batch(preds_new, raw_ns, batch = "b3")
cut3_dat <- cut3[["dat"]]
cut3_lab <- cut3[["lab"]]

x.new <- sl_data(cut3_dat)[final_glist]
y.new <- sl_class(cut3_lab, x.new)

cut3_cm <- cut3_lab %>%
  dplyr::transmute(
    `Predicted with Final Model` = splendid::prediction(final_model, x.new, y.new),
    `Consensus Label` = Adaboost.xpn
  ) %>%
  table() %>%
  caret::confusionMatrix()

## ----cut3_confmat--------------------------------------------------------
pandoc.table(ftable(cut3_cm[["table"]]))

## ----cut3_overall_metrics------------------------------------------------
cut3_ov_metrics <- confmat_metrics(cut3_cm, metrics = "overall")
pandoc.table(cut3_ov_metrics)

## ----cut3_by-class_metrics-----------------------------------------------
cut3_bc_metrics <- confmat_metrics(cut3_cm, metrics = "byclass")
pandoc.table(cut3_bc_metrics, keep.trailing.zeros = TRUE)

## ----cut4, include=FALSE-------------------------------------------------
# Define cut4 (remove ARL samples)
cut4 <- define_batch(preds_new, raw_ns, batch = "b4")
cut4_dat <- cut4[["dat"]] %>% dplyr::filter(!OTTA.ID %in% arl_samples[["OTTA ID"]])
cut4_lab <- cut4[["lab"]] %>% dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]])

x.new <- sl_data(cut4_dat)[final_glist]
y.new <- sl_class(cut4_lab, x.new)

cut4_cm <- cut4_lab %>%
  dplyr::transmute(
    `Predicted with Final Model` = splendid::prediction(final_model, x.new, y.new),
    `Consensus Label` = Adaboost.xpn
  ) %>%
  table() %>%
  caret::confusionMatrix()

## ----cut4_confmat--------------------------------------------------------
pandoc.table(ftable(cut4_cm[["table"]]))

## ----cut4_overall_metrics------------------------------------------------
cut4_ov_metrics <- confmat_metrics(cut4_cm, metrics = "overall")
pandoc.table(cut4_ov_metrics)

## ----cut4_by-class_metrics-----------------------------------------------
cut4_bc_metrics <- confmat_metrics(cut4_cm, metrics = "byclass")
pandoc.table(cut4_bc_metrics, keep.trailing.zeros = TRUE)

## ----overlap, include=FALSE----------------------------------------------
# Define overlap
overlap <- define_overlap(preds_new, raw_ns)
overlap_dat <- overlap[["dat"]]
overlap_lab <- overlap[["lab"]]

x.new <- sl_data(overlap_dat)[final_glist]
y.new <- sl_class(overlap_lab, x.new)

overlap_cm <- overlap_lab %>%
  dplyr::transmute(
    `Predicted with Final Model` = splendid::prediction(final_model, x.new, y.new),
    `Consensus Label` = Adaboost.xpn
  ) %>%
  table() %>%
  caret::confusionMatrix()

## ----overlap_confmat-----------------------------------------------------
pandoc.table(ftable(overlap_cm[["table"]]))

## ----overlap_overall_metrics---------------------------------------------
overlap_ov_metrics <- confmat_metrics(overlap_cm, metrics = "overall")
pandoc.table(overlap_ov_metrics)

## ----overlap_by-class_metrics--------------------------------------------
overlap_bc_metrics <- confmat_metrics(overlap_cm, metrics = "byclass")
pandoc.table(overlap_bc_metrics, keep.trailing.zeros = TRUE)

## ----entropy_setup-------------------------------------------------------
# Merge probabilities and predictions
compare_probs <-
  dplyr::inner_join(array_probs,
                    tcga_probs,
                    by = "ottaID",
                    suffix = c("_array", "_tcga")) %>% 
  dplyr::inner_join(all_pred, by = "ottaID") %>% 
  dplyr::inner_join(final_preds, by = "ottaID") %>%
  dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]]) %>% 
  dplyr::rename(predicted_array = Adaboost.xpn,
                predicted_tcga = TCGA.Predicted.Subtype,
                predicted_final = final) %>%
  dplyr::rename_at(c("C1.MES", "C2.IMM", "C4.DIF", "C5.PRO"),
                   ~ paste0(., "_final")) %>%
  dplyr::mutate_at(c("predicted_array", "predicted_tcga"), make.names) %>% 
  dplyr::select(-all_array, -TCGA, -published, -consensus, -prediction)

# Compute entropies
entropy_probs <- compare_probs %>%
  dplyr::transmute(
    ottaID,
    entropy_array = apply(.[grep("(?<!predicted)_array", names(.), perl = TRUE)], 1,
                          entropy::entropy, unit = "log2"),
    entropy_tcga = apply(.[grep("(?<!predicted)_tcga", names(.), perl = TRUE)], 1,
                         entropy::entropy, unit = "log2"),
    entropy_final = apply(.[grep("(?<!predicted)_final", names(.), perl = TRUE)], 1,
                         entropy::entropy, unit = "log2"),
    predicted_array,
    predicted_tcga,
    predicted_final,
    match = factor(
      ifelse(predicted_array == predicted_tcga, "Consensus", "Non-Consensus"),
      levels = c("Consensus", "Non-Consensus")
    )
  ) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("entropy")), round, digits = 3)

## ----entropy_mwu_final---------------------------------------------------
mwu_final <- wilcox.test(entropy_final ~ match, entropy_probs) %>%
  broom::tidy() %>% 
  dplyr::mutate(p.value = ifelse(p.value < 0.001, "< 0.001", p.value))
pandoc.table(mwu_final)

## ----entropy_boxplot_final-----------------------------------------------
entropy_probs_grouped <- entropy_probs %>% 
  tidyr::gather(key = Data, value = Entropy, 2:4) %>% 
  tidyr::gather(key = Prediction, value = Subtype, 2:4) %>% 
  dplyr::mutate(Data = factor(
    dplyr::case_when(
      Data == "entropy_array" ~ "All Array",
      Data == "entropy_tcga" ~ "TCGA",
      Data == "entropy_final" ~ "Final",
      TRUE ~ NA_character_
    ),
    levels = c("All Array", "TCGA", "Final")
  )) %>% 
  dplyr::filter(Data == "Final")

## ----entropy_boxplot_final_data, fig.width=7, fig.height=5---------------
p <- ggplot(entropy_probs_grouped, aes(Data, Entropy, fill = match)) +
  geom_boxplot() +
  ggtitle("Entropy Comparison of Final Prediction Labels") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_boxplot_final_data.pdf"), p, width = 7, height = 5)

## ----entropy_boxplot_final_subtypes, fig.width=7, fig.height=5-----------
p <- ggplot(entropy_probs_grouped, aes(Subtype, Entropy, fill = match)) +
  geom_boxplot() +
  ggtitle("Entropy Comparison of Final Prediction Labels by Subtype") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.title = element_blank())

# print(p)
ggsave(file.path(fig_path, "entropy_boxplot_final_subtypes.pdf"), p, width = 7, height = 5)


## ----child="Supp_C05.Rmd"------------------------------------------------

## ----setup_C05, include=FALSE--------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	results = "asis"
)

## ----load_C05------------------------------------------------------------
# Load packages, helpers, input data
suppressPackageStartupMessages({
  library(ggraph)
  library(here)
})
source(here("pipeline/7-supplementary/utils.R"))
source(here("pipeline/7-supplementary/utils-pathway.R"))

n_genes <- 55
grm <- "CTHRC1"

pdat_raw <- readr::read_csv(here("data/supp/boxplotorder.csv"),
                            col_types = readr::cols())
raw_ns <- load_nanostring()
arl_samples <- readxl::read_excel(here("data/nstring/Nanostring_ARL-all samples-all gnes_20180607.xlsx"))
sum_freq <- readr::read_csv(file.path(params$outputDir, "gene_selection/sum_freq/overall_freq.csv"),
                            col_types = readr::cols())
final_preds <- readr::read_csv(file.path(params$outputDir, "gene_selection/final_model/Final_Predictions.csv"),
                               col_types = list("X1" = readr::col_skip()))
aa_raw <- readRDS(file.path(
  params$outputDir,
  "unsupervised/map_genes/ov.afc1_xpn/npcp-hcNorm_ov.afc1_xpn.rds"
))
aa_clusts <- readRDS(file.path(
  params$outputDir,
  "unsupervised/final/ov.afc1_xpn/all_clusts_ov.afc1_xpn.rds"
)) %>%
  purrr::pluck("kmodes")
load(here("data/nstring/tcga_data_with_molsubtypes.rda")) # eset
mapped_tcga <- readr::read_csv(here("data/nstring/TCGA_sampleIDs_OTTA-Mapped.csv"), col_types = readr::cols())

fig_path <- file.path(params$outputDir, "supplementary/figures/C05")

## ----pathway_munge-------------------------------------------------------
# Munge to needed format
pdat <- pdat_raw %>% 
  tidyr::gather(key = subtype, value = edge, dplyr::matches(".+path.+")) %>%
  dplyr::mutate_at("subtype", forcats::fct_inorder) %>% 
  dplyr::filter(edge == "1") %>% 
  dplyr::select(-Obs, -edge)

## ----pathway_object------------------------------------------------------
# Pathway object for graphing
pdat_graph <- pdat %>% 
  tidyr::separate(col = subtype, into = c("subtype", "direction"), sep = "path")
g <- create_tbl_graph(
  data = pdat_graph,
  node = "path",
  edge = "gene",
  groups = list("subtype", "direction")
)

## ----pathways_graph_combined, fig.width=8, fig.height=10-----------------
set.seed(1)
gg_combined <- create_ggraph(
  graph = g,
  color = "subtype",
  linetype = "direction",
  title = "Gene Pathways"
)
# print(gg_combined)
ggsave(file.path(fig_path, "pathways_graph_combined.pdf"), gg_combined,
       width = 8, height = 10)

## ----pathways_graph_by_subtype, fig.width=8, fig.height=10---------------
set.seed(1)
gg_subtype <- create_ggraph(
  graph = g,
  color = "subtype",
  linetype = "direction",
  title = "Gene Pathways by Subtype",
  facet = "subtype"
)
# print(gg_subtype)
ggsave(file.path(fig_path, "pathways_graph_by_subtype.pdf"), gg_subtype,
       width = 8, height = 10)

## ----pathways_graph_by_direction, fig.width=8, fig.height=10-------------
set.seed(1)
gg_direction <- create_ggraph(
  graph = g,
  color = "subtype",
  linetype = "direction",
  title = "Gene Pathways by Direction",
  facet = "direction"
)
# print(gg_direction)
ggsave(file.path(fig_path, "pathways_graph_by_direction.pdf"), gg_direction,
       width = 8, height = 10)

## ----pathway_heatmap, fig.width=8, fig.height=7--------------------------
hm_pdat <- pdat %>% 
  dplyr::count(gene, subtype) %>% 
  tidyr::complete(gene, subtype, fill = list(n = 0)) %>%
  tidyr::spread(gene, n) %>% 
  as.data.frame() %>% 
  tibble::column_to_rownames("subtype")

hm_ga <- pheatmap::pheatmap(
  hm_pdat,
  fontsize_row = 5,
  fontsize_col = 6,
  main = "Gene Pathways Heatmap"
)
ggsave(here(fig_path, "pathways_heatmap.pdf"), hm_ga, width = 8, height = 7)

gene_order <- hm_ga %>% 
  purrr::pluck("tree_col") %>% 
  magrittr::extract(c("labels", "order")) %>% 
  purrr::invoke(magrittr::extract, .)

## ----final_glist---------------------------------------------------------
# Final gene list
final_glist <- sum_freq %>%
  dplyr::arrange(dplyr::desc(rfFreq), dplyr::desc(lassoFreq)) %>%
  dplyr::pull(genes) %>%
  make.names() %>%
  purrr::discard(~ . %in% grm) %>%
  head(n_genes)

# Ordered by pathway clustering
final_glist_ordered <- gene_order %>%
  match(final_glist) %>%
  c(setdiff(seq_along(final_glist), .)) %>%
  magrittr::extract(final_glist, .)

## ----gene_ex_aa, fig.width=11, fig.height=11-----------------------------
# Joined data (all-array)
aa_dat <- aa_raw %>%
  dplyr::select(final_glist_ordered) %>%
  tibble::rownames_to_column("sample") %>%
  tibble::add_column(Subtype = factor(dplyr::case_when(
    aa_clusts == 1 ~ "C2.IMM",
    aa_clusts == 2 ~ "C4.DIF",
    aa_clusts == 3 ~ "C5.PRO",
    aa_clusts == 4 ~ "C1.MES"
  )), .after = 1) %>%
  tidyr::gather(key = Gene, value = Expression, -c(1:2), factor_key = TRUE) %>%
  tibble::as_tibble()

# Mean expression (all-array)
me_aa <- aa_dat %>%
  dplyr::mutate(Subtype = paste0("me_", Subtype)) %>%
  dplyr::group_by(Gene, Subtype) %>%
  dplyr::summarize(Mean_Expression = mean(Expression)) %>%
  tidyr::spread(key = Subtype, value = Mean_Expression)

# Number of cases and top genes (all-array)
n_cases_aa <- dplyr::n_distinct(aa_dat[["sample"]])
n_tp_aa <- dplyr::n_distinct(aa_dat[["Gene"]])

# Pairwise p-values (all-array)
pp_aa <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = aa_dat,
  method = "t.test",
  group.by = "Gene",
  p.adjust.method = "fdr"
) %>%
  dplyr::inner_join(me_aa, by = "Gene") %>%
  dplyr::select(-.y.) %>%
  dplyr::mutate(p.adj = p.adjust(p, method = "fdr"))
readr::write_csv(pp_aa, file.path(params$outputDir, "supplementary/tables/aa_pairwise_pvals.csv"))

# Largest global p-value (all-array)
max_gp_aa <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = aa_dat,
  method = "anova",
  group.by = "Gene"
) %>%
  dplyr::top_n(n = 1, wt = p) %>%
  dplyr::pull("p.format")

# Gene expression for top genes by predicted final subtype (all-array)
# p_aa <- ggboxplot(aa_dat, x = "Subtype", y = "Expression", fill = "Subtype", facet.by = "Gene", nrow = 5, ncol = 11) +
#   stat_compare_means(method = "anova") +
p_aa <- ggplot(aa_dat, aes(x = Subtype, y = Expression, fill = Subtype)) +
  geom_boxplot() +
  facet_wrap(facets = vars(Gene), nrow = 5, ncol = 11) +
  labs(
    x = "Subtype",
    y = "Expression",
    title = paste0("Full All-Array Gene Expression for Top ", n_tp_aa,
                   " Genes by Subtype"),
    subtitle = paste0("(n=", n_cases_aa, ")"),
    caption = paste("Largest global ANOVA p-value", max_gp_aa)
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
# print(p_aa)
ggsave(file.path(fig_path, "boxplot_subtype_aa.pdf"), p_aa, width = 11, height = 11)

## ----gene_ex_ns, fig.width=11, fig.height=11-----------------------------
# Joined data (NanoString)
dat_ns <- raw_ns %>%
  dplyr::rename(ottaID = OTTA.ID) %>% 
  dplyr::filter(!ottaID %in% arl_samples[["OTTA ID"]]) %>%
  dplyr::inner_join(final_preds, by = "ottaID") %>%
  dplyr::select(ottaID, Subtype = final, final_glist_ordered) %>%
  tidyr::gather(key = Gene, value = Expression, -c(1:2), factor_key = TRUE) %>%
  dplyr::mutate(
    Subtype = factor(Subtype, levels = names(table(final_preds[["final"]])))
  )

# Mean expression (NanoString)
me_ns <- dat_ns %>%
  dplyr::mutate(Subtype = paste0("me_", Subtype)) %>%
  dplyr::group_by(Gene, Subtype) %>%
  dplyr::summarize(Mean_Expression = mean(Expression)) %>%
  tidyr::spread(key = Subtype, value = Mean_Expression)

# Number of cases and top genes (NanoString)
n_cases_ns <- dplyr::n_distinct(dat_ns[["ottaID"]])
n_tp_ns <- dplyr::n_distinct(dat_ns[["Gene"]])

# Pairwise p-values (NanoString)
pp_ns <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = dat_ns,
  method = "t.test",
  group.by = "Gene",
  p.adjust.method = "fdr"
) %>%
  dplyr::inner_join(me_ns, by = "Gene") %>%
  dplyr::select(-.y.) %>%
  dplyr::mutate(p.adj = p.adjust(p, method = "fdr"))
readr::write_csv(pp_ns, file.path(params$outputDir, "supplementary/tables/ns_pairwise_pvals.csv"))

# Largest global p-value (NanoString)
max_gp_ns <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = dat_ns,
  method = "anova",
  group.by = "Gene"
) %>%
  dplyr::top_n(n = 1, wt = p) %>%
  dplyr::pull("p.format")

# Gene expression for top genes by predicted final subtype
p_ns <- ggplot(dat_ns, aes(x = Subtype, y = Expression, fill = Subtype)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(facets = vars(Gene), nrow = 5, ncol = 11) +
  labs(
    x = "Subtype",
    y = "Expression",
    title = paste0("Full NanoString Gene Expression for Top ", n_tp_ns,
                   " Genes by Subtype"),
    subtitle = paste0("(n=", n_cases_ns, ")"),
    caption = paste("Largest global ANOVA p-value", max_gp_ns)
  ) +
  ylim(range(aa_dat[["Expression"]])) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
# print(p_ns)
ggsave(file.path(fig_path, "boxplot_subtype_ns.pdf"), p_ns, width = 11, height = 11)

## ----gene_ex_tcga, fig.width=11, fig.height=11---------------------------
# Data and class (TCGA)
raw_tcga <- Biobase::exprs(eset)
class_tcga <- Biobase::pData(eset) %>%
  tibble::rownames_to_column("sample") %>%
  dplyr::select(sample, Subtype = MolSubtype)

# Joined data (TCGA)
dat_tcga <- raw_tcga %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename_at("CD3E", ~ "CD3e") %>% # Capitalize to match
  dplyr::select(intersect(final_glist_ordered, colnames(.))) %>% # Missing CYTIP, CD8A, CD68 so only 52 top genes
  tibble::rownames_to_column("sample") %>%
  # dplyr::filter(sample %in% unlist(purrr::map(mapped_tcga[["Short-ID"]], ~ grep(., sample, value = TRUE)))) %>% # using only TCGA matched to OTTA
  dplyr::inner_join(class_tcga, ., by = "sample") %>%
  dplyr::mutate(Subtype = factor(dplyr::case_when(
    Subtype == "C1" ~ "C1.MES",
    Subtype == "C2" ~ "C2.IMM",
    Subtype == "C4" ~ "C4.DIF",
    Subtype == "C5" ~ "C5.PRO"
  ))) %>%
  tidyr::gather(key = Gene, value = Expression, -c(1:2), factor_key = TRUE) %>%
  tibble::as_tibble()

# Mean expression (TCGA)
me_tcga <- dat_tcga %>%
  dplyr::mutate(Subtype = paste0("me_", Subtype)) %>%
  dplyr::group_by(Gene, Subtype) %>%
  dplyr::summarize(Mean_Expression = mean(Expression)) %>%
  tidyr::spread(key = Subtype, value = Mean_Expression)

# Number of cases and top genes (TCGA)
n_cases_tcga <- dplyr::n_distinct(dat_tcga[["sample"]])
n_tp_tcga <- dplyr::n_distinct(dat_tcga[["Gene"]])

# Pairwise p-values (TCGA)
pp_tcga <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = dat_tcga,
  method = "t.test",
  # method = "wilcox.test", # use for small samples if filtering TCGA by matched OTTA
  group.by = "Gene",
  p.adjust.method = "fdr"
) %>%
  dplyr::inner_join(me_tcga, by = "Gene") %>%
  dplyr::select(-.y.) %>%
  dplyr::mutate(p.adj = p.adjust(p, method = "fdr"))
readr::write_csv(pp_tcga, file.path(params$outputDir, "supplementary/tables/tcga_pairwise_pvals.csv"))

# Largest global p-value (TCGA)
max_gp_tcga <- ggpubr::compare_means(
  formula = Expression ~ Subtype,
  data = dat_tcga,
  method = "anova",
  group.by = "Gene"
) %>%
  dplyr::top_n(n = 1, wt = p) %>%
  dplyr::pull("p.format")

# Gene expression for top genes by predicted final subtype (TCGA)
# p_tcga <- ggboxplot(dat_tcga, x = "Subtype", y = "Expression", fill = "Subtype", facet.by = "Gene", nrow = 5, ncol = 11) +
#   stat_compare_means(method = "anova") +
p_tcga <- ggplot(dat_tcga, aes(x = Subtype, y = Expression, fill = Subtype)) +
  geom_boxplot() +
  facet_wrap(facets = vars(Gene), nrow = 5, ncol = 11) +
  labs(
    x = "Subtype",
    y = "Expression",
    title = paste0("Full TCGA Gene Expression for Top ", n_tp_tcga,
                   " Genes by Subtype"),
    subtitle = paste0("(n=", n_cases_tcga, ")"),
    caption = paste("Largest global ANOVA p-value", max_gp_tcga)
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
# print(p_tcga)
ggsave(file.path(fig_path, "boxplot_subtype_tcga.pdf"), p_tcga, width = 11, height = 11)


