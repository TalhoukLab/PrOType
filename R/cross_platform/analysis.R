rm(list = ls())

# Load and map samples and genes between array and NanoString
library(here)
source(here("CrossPlatform/cp_map.R"))

input_dir <- mkdir(here("Outputs/GeneSelection/output/sumFreq"))
output_dir <- mkdir(here("Outputs/CrossPlatform/output"))
plot_dir <- mkdir(here("Outputs/CrossPlatform/plots"))

# Determine outliers
is_outlier <- function(x, y, n = 3) {
  res <- residuals(lm(y ~ x))
  tops <- head(sort(res), n)
  bottoms <- tail(sort(res), n)
  res %in% c(tops, bottoms)
}

# load genes in the classifier
genes59 <- file.path(input_dir, "overallFreqs.csv") %>%
  read.csv(stringsAsFactors = FALSE) %>%
  arrange(desc(rfFreq)) %>%
  pull(genes) %>%
  head(59) %>%
  make.names()

# Concordance Plots
df <- rbind(
  overlap_array_dat %>%
    `[`(colnames(.) %in% c("ottaID", genes59[!genes59 %in% "CTHRC1"])) %>%
    gather(key = "gene", value = "value", -1, factor_key = TRUE) %>%
    data.frame(platform = "array", .),
  overlap_nstring_dat %>%
    `[`(colnames(.) %in% c("ottaID", genes59[!genes59 %in% "CTHRC1"])) %>%
    gather(key = "gene", value = "value", -1, factor_key = TRUE) %>%
    data.frame(platform = "nstring", .)
)

# Plot the average expression and point out outliers
df_spr <- df %>%
  spread(platform, value) %>%
  group_by(gene) %>%
  summarize(
    nstring.mean = mean(nstring, na.rm = TRUE),
    array.mean = median(array, na.rm = TRUE),
    concordance_est = epiR::epi.ccc(nstring, array)[["rho.c"]][["est"]],
    accuracy = epiR::epi.ccc(nstring, array)[["C.b"]]
  ) %>%
  arrange(desc(concordance_est), desc(accuracy)) %>%
  mutate(outlier = ifelse(is_outlier(nstring.mean, array.mean), gene, NA_real_))

ggplot(df_spr, aes(x = nstring.mean, y = array.mean)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, colour = "blue", size = 1) +
  geom_text(aes(label = gene[outlier]), na.rm = TRUE, hjust = 1.3,
            colour = "red", size = 3) +
  ggtitle("Nanostring - Array Mean Expression")

ggsave(file.path(plot_dir, "meanexpr.pdf"))

ggplot(df_spr, aes(x = concordance_est, y = accuracy)) +
  geom_point() +
  theme_bw() +
  ggtitle("Accuracy and concordance")

ggsave(file.path(plot_dir, "ACCconc.pdf"))

# concordance combination of precision (how tight the points together)
# and accuracy how close the line is to the identity line
df_2 <- spread(df, platform, value)
n_pages <- ceiling(58 / 25)
# Draw each page
for (i in seq_len(n_pages)) {
  ggplot(df_2, aes(x = array, y = nstring)) +
    ggforce::facet_wrap_paginate(~gene, ncol = 5, nrow = 5, page = i) +
    geom_point() +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0, colour = "blue", size = 1)
  ggsave(file.path(plot_dir, paste0("bygene_", i, ".pdf")))
}

# Reliability summary
reliability <- df_spr %>%
  select(gene, concordance_est, accuracy)
write_csv(reliability, file.path(output_dir, "reliability.csv"))
