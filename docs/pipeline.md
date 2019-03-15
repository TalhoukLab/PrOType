# PrOType Pipeline

The full PrOType pipeline can be run using `make all` at the project root directory. All intermediate targets described below must also be run from the project root.

## Array Classifier

The array classifier is split into three sections: unsupervised learning, supervised learning, post-processing. Run the array classifier with `make array`.

### Unsupervised Learning

The `1-unsupervised` pipeline performs consensus clustering to determine HGSC subtypes using an unsupervised cluster ensemble method. Run the unsupervised learning pipeline with `make unsupervised`.

1. `1-prep_data`: Prepare data for clustering. Creates a transposed and scaled version of your data.
2. `2-map_genes`: Map Affymetrix data to NanoString and normalize to housekeeping genes.
3. `3-clustering`: Consensus clustering by different algorithms and k-Nearest Neighbour imputation of missing samples.
4. `4-consmat`: Consensus matrix computed from the raw consensus clustering output.
5. `5-merge_clust`: Merge consensus clustering results across repetitions and algorithms. Impute remaining missing samples using majority voting.
6. `6a-merge_cm_partial` and `6b-merge_cm_complete`: Individual consensus matrix objects can be considerably large. To avoid memory problems, we merge the consensus matrix results in two stages. The splitting criterion is determined by the `c` parameter.
7. `7-con_fun`: Consensus functions like CSPA, LCE and k-modes are computed from the clustering results to form ensemble cluster assignments.
8. `8-final_clust`: Form a final clustering assignment matrix constructed from base (e.g. k-means) and ensemble (e.g. CSPA) clustering algorithms. Also computes internal validity indices and ranks the algorithms based on performance on these measures.

### Supervised Learning

The `2-supervised` pipeline performs an ensemble classification to classify HGSC subtypes in a validation dataset. Bootstrapped samples are fit on a selection of classification algorithms and the out-of-sample error is used to compare model performance. Run the supervised learning pipeline with `make supervised`.

1. `1-train_eval`: Train the classification algorithms and output the evaluation measures such as accuracy and F1-score.
2. `2-merge_eval`: Merge the evaluation measures and calculate their median, 5% and 95% quantiles across the bootstrapped samples.
3. `3-top_ci`: Rank the top classification algorithms and calculate a 95% bootstrap confidence internal of each evaluation measure.
4. `4-iv_summary`: The internal validity summary shows a table of each algorithm, batch effect correction method, and evaluation measure at the median, 5% and 95% quantiles.
5. `5-combine_ivs`: Combines the internal validity summaries from different data sources.

### Post-Processing

The `3-post_processing` pipeline validates the top performing classifiers for different batch correction methods (XPN and CBT) and selects the candidate model to be used for sample prediction on NanoString. The `data` directory contains all necessary inputs needed for post-processing. Run the post-processing pipeline with `make post_processing`.

1. `0-compare_reference`: Compare results from the default unsupervised pipeline with reference outputs to verify reproducibility of results.
2. `1-internal_validity_plots`: The supervised learning ensemble is evaluated through a series of visualizations of top performing algorithms.
3. `2-evaluate_batch_effects`: Use PVCA and PCA plots to choose the best batch effect correction method.
4. `3-fit_top_models`: The top 5 models from the array classifier are fit to the full cut 1 array data
5. `4-validate_array`: The fitted models are used to predict and evaluated against the overlapping array samples. These results are benchmarked against published labels.
6. `5-predict_C2`: The fitted models are used to predict the cut 2 array data.
7. `6-probe_mapping_C2v2`: Map the cut 2 predictions to subtypes and generate centroid data for each gene.
8. `7-mapping_signatures_C2`: Compare cut 2 predictions with Verhaaks signature. Compute correlation matrices and calculate entropy and quadratic loss.

## NanoString Classifier

The NanoString classifier `4-nanostring_classifier` uses the top array classifier to predict and validate NanoString data. Run the NanoString classifier with `make nanostring`.

1. `1-validate_nanostring`: The optimal model and batch-correction method are validated on the overlapping samples between Affymetrix and Nanostring. Evaluation between array, NanoString, and published labels is conducted.
2. `2-predict_all_nanostring`: The optimal model (adaboost) is used to predict all samples of NanoString (batch 1-4)

## Gene Selection

The `5-gene_selection` pipeline selects a minimal set of genes for the final model used to predict molecular subtypes of HGSC. We adopt a combination of bootstrap resampling and a leave one study out approach. Run the gene selection pipeline with `make gene_selection`.

An initial data setup occurs:
  - We select samples from NanoString cut 1 where there was consensus between the TCGA model predictions (using random forest) and the overall array model predictions (using Adaboost)
  - We exclude "overlapping samples", which are samples that have RNA expression on both the array and the NanoString platform

Then iterating over each study:

1. `1-boot_freq`: We resample with replacement from all but one study, each time fitting classification algorithms. Then we count the frequency of a gene being selected across the bootstrap samples. For random forest and Adaboost, a gene is "selected" if it is included in the top 100 by its respective importance measure. For lasso, a gene is "selected" if it has a non-zero coefficient estimate.
2. `2-sum_freq`: Combine the gene selections across all studies.
3. `3-train`: Train each algorithm on an increasing number of the top genes (4 to 99 in increments of 5).
4. `4-predict`: Predict the left out study using trained models from previous step.
5. `5-evaluate:`: Compare predictions with the gold standard consensus labels, make plots of overall and by-class accuracy and F1-score, analyze genes ranked as important by each study.

The second part of the gene selection pipeline employs a different setup:
  - Define two test cohorts: "overlapping samples", which are samples that have RNA expression on both the array and the NanoString platform, and cut 2 test samples
  - Consider the top 59 genes from each frequency list which were generated in the previous step.
  - Take the union of all the top 59 lists (we highlight on a heatmap which of these would be in the top 59 form the average overall frequency list); this results in ~ 77 genes

6. `6-retrain`: Retrain model with more granularity.
    - Train models using random forest only, going from the top 50 to the top 70 genes adding one gene at a time based on frequency
    - Predict overlapping samples and evaluate the predictions
    - Predict cut 2and evaluate the predictions 
    - Refine the model by removing genes that do not correlate well with array measurements.
7. `7-final_model`: Make predictions on full NanoString data, cut 1-4 data individually, overlap data, and external data using final model trained on cut 1 with top genes.

## Cross-Platform

The `6-cross_platform` pipeline evaluates expression level of the same genes (and the same samples) measured using NanoString and measured using Affymetrix array. It also evaluates the locked down model on NanoString and on Array and compares the prediction across platforms. Run the cross-platform pipeline using `make cross_platform`.

Both steps below first load and map samples and genes between array and NanoString using `0-cp_map.R`.

1. `1-cp_analysis`: Plot concordance values and produce reliabilty results across platform.
2. `2-cp_predictions`: Load model obtained after gene selection, predict on each platform, compare predictions across platforms

## Supplementary

The `7-supplementary` pipeline runs a number of post-hoc analyses on the results of previous pipelines and summarizes the information for the Supplementary sections of the paper. Run the supplementary pipeline using `make supplementary`.

1. `1-supp_a`: Supplementary A - Molecular Classifier Development on Legacy Gene Expression Datasets
2. `2-supp_b`: Supplementary B - NanoString Sample Processing and Classifier Portability
3. `3-supp_c`: Supplementary C - Development of a minimal predictive model on NanoString Data
4. `4-supp_d`: Supplementary D - Biological Correlates of Molecular Subtypes 
