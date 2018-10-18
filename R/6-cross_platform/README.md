# HGSC molecular classification: Cross Platform

This section of code evaluates expression level of the same genes (and the same samples) measured using NanoString and measured using Affymetrix array. It also evaluates the locked down model on NanoString and on Array and compares the prediction across platforms.

## R package dependencies
- `here`, `tidyverse`
- `splendid` (www.github.com/AlineTalhouk/splendid)

## Cross-Platform Evaluation

The script `cp_analysis.R` does the following:

  - loading and mapping samples and genes between array and NanoString using `cp_map.R`
  - Plot concordance values and produce reliabilty results across platform

The script `cp_predictions.R` does the following:

  - loading and mapping samples and genes between array and NanoString using `cp_map.R`
  - load model obtained after gene selection
  - predict on each platform 
  - compare the predictions across platforms
