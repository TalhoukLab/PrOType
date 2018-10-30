# Load and map samples and genes between array and NanoString
library(ggplot2)
source(here::here("pipeline/6-cross_platform/0-cp_map.R"))

# Load Parameters
output_dir <- file.path(outputDir, "cross_platform", "analysis")
n_genes <- 59
grm <- "CTHRC1"
width <- 7
height <- 7

# Determine outliers
is_outlier <- function(x, y, n = 3) {
  res <- residuals(lm(y ~ x))
  tops <- head(sort(res), n)
  bottoms <- tail(sort(res), n)
  res %in% c(tops, bottoms)
}

# Reformat overlap data into long format for specified platform
tidy_overlap <- function(data, genes, platform) {
  data %>%
    `[`(colnames(.) %in% c("ottaID", genes)) %>%
    data.frame(platform = platform, .) %>%
    tidyr::gather(key = "gene", value = "value", -1:-2, factor_key = TRUE)
}

# Load top "n_genes" in classifier
sumFreq <-
  file.path(outputDir, "gene_selection", "sum_freq", "overall_freq.csv") %>%
  readr::read_csv(col_types = readr::cols())
final_glist <- sumFreq %>%
  dplyr::arrange(dplyr::desc(rfFreq), dplyr::desc(lassoFreq)) %>%
  dplyr::pull(genes) %>%
  make.names() %>%
  purrr::discard(~ . %in% grm) %>%
  head(n_genes)

# Reformat overlap data and combine both platforms
overlap_array_long <- tidy_overlap(overlap_array_dat, final_glist, "array")
overlap_nstring_long <- tidy_overlap(overlap_nstring_dat, final_glist, "nstring")
df <- rbind(overlap_array_long, overlap_nstring_long)

# Plot the average expression and point out outliers
df_spr <- df %>%
  tidyr::spread(platform, value) %>%
  dplyr::group_by(gene) %>%
  dplyr::summarize(
    nstring.mean = mean(nstring, na.rm = TRUE),
    array.mean = median(array, na.rm = TRUE),
    concordance_est = epiR::epi.ccc(nstring, array)[["rho.c"]][["est"]],
    accuracy = epiR::epi.ccc(nstring, array)[["C.b"]]
  ) %>%
  dplyr::arrange(dplyr::desc(concordance_est), dplyr::desc(accuracy)) %>%
  dplyr::mutate(outlier = ifelse(is_outlier(nstring.mean, array.mean), gene, NA_real_))

# Mean Expression Plots
p1 <- ggplot(df_spr, aes(x = nstring.mean, y = array.mean)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, colour = "blue", size = 1) +
  geom_text(aes(label = gene[outlier]), na.rm = TRUE, hjust = 1.3,
            colour = "red", size = 3) +
  ggtitle("Nanostring - Array Mean Expression")

ggsave(file.path(output_dir, "meanexpr.pdf"),
       p1,
       width = width,
       height = height)

# Concordance Plots
p2 <- ggplot(df_spr, aes(x = concordance_est, y = accuracy)) +
  geom_point() +
  theme_bw() +
  ggtitle("Accuracy and concordance")

ggsave(file.path(output_dir, "ACCconc.pdf"),
       p2,
       width = width,
       height = height)

# concordance combination of precision (how tight the points together)
# and accuracy how close the line is to the identity line
df_2 <- tidyr::spread(df, platform, value)
n_pages <- ceiling(58 / 25)
# Draw each page
for (i in seq_len(n_pages)) {
  p <- ggplot(df_2, aes(x = array, y = nstring)) +
    ggforce::facet_wrap_paginate(~gene, ncol = 5, nrow = 5, page = i) +
    geom_point() +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0, colour = "blue", size = 1)

  ggsave(file.path(output_dir, paste0("bygene_", i, ".pdf")),
         p,
         width = width,
         height = height)
}

# Reliability summary
reliability <- dplyr::select(df_spr, gene, concordance_est, accuracy)
readr::write_csv(reliability, file.path(output_dir, "reliability.csv"))
