# HGSC molecular classification: Gene Selection

This section of code selects a minimal set of genes to predict molecular subtypes of HGSC. We adopt a combination of bootstrap resampling and a leave one study out approach.

## R package dependencies
- `here`, `tidyverse`, `magrittr`, `caret`, `glmnet`, `cli`
- `splendid` (www.github.com/AlineTalhouk/splendid)

## Initial Script

The script `GSTraining_initial.R` contains the following parts:

- Loading the data
    - We select samples from NanoString cut 1 where there was consensus between the TCGA model predictions (using random forest) and the overall array model predictions (using Adaboost)
    - We exclude "overlapping samples", which are samples that have RNA expression on both the array and the NanoString platform
    - We split the data by study 

Iterating over study: 

- We run a bootstrap analysis 
    - We resample with replacement 500 times from all but one study, each time fitting 3 algorithms (`runBoot <- 1`)
    - We consider which genes are included in the top 100 most important genes and count the frequency (`processBoot <- 1`)

- Training models with varying number of genes 
    - For each algorithm, ordering the genes by the proportion of times they were deemed important and fitting algorithms with increasing number of genes (5 at a time starting from 4 to 99) (`finalTraining <- 1`)
    - Predict the left out study (`makePreds <- 1`)
      
- Evaluate predictions
    - Compare predictions with the gold standard (the consensus labels) (`evaluatePreds <- 1`)
    - Make plots of overall and by class accuracy and F1-score respectively (`producePlots <- 1`)
    - Combine most frequently selected genes by averaging frequencies over all studies (`sumOverallFreqs <- 1`)
    - Analyze which genes were ranked as important by each study (`geneAnalysis <- 1`)

## Part 2 Refinement Script

The script `GSTraining_part2.R` refines the gene selection:

- Loading the data
    - Select samples from NanoString cut 1 where there was consensus between the TCGA model predictions (using random forest) and the overall array model predictions (using Adaboost)
    - Define two test cohorts: "overlapping samples", which are samples that have RNA expression on both the array and the NanoString platform, and cut2 test samples.
    - Consider the top 59 genes from each frequency list which were generated in the previous step.
    - Take the union of all the top 59 lists (we highlight on a heatmap which of these would be in the top 59 form the average overall frequency list); this results in ~ 77 genes

- Re-train with more granularity 
    - Train models (`trainModel <- 1`) using random forest only, going from the top 50 to the top 70 genes adding one gene at a time based on frequency
    - Predict overlapping samples (`predOverlap <- 1`) and evaluate the predictions
    - Predict cut 2 (`predCut2 <- 1`) and evaluate the predictions 
    - Refine the model(`refineModel <- 1`) by removing genes that do not correlate well with array measurements.
    - Make predictions and save models.
