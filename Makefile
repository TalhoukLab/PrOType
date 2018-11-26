# Makefile to run PrOType pipeline

# Run full pipeline
all: array nanostring gene_selection cross_platform

# Intermediate targets
array: unsupervised supervised post_processing

unsupervised: prep_data map_genes cluster consmat merge_clust merge_cm con_fun final_clust

supervised: train_eval merge_eval top_ci iv_summary

post_processing: compare plot_iv batch_effects validation

nanostring: ns_validate ns_all_array ns_tcga

gene_selection: boot_freq sum_freq train predict evaluate retrain final_model

cross_platform: cp_analysis cp_predictions


# Part 1: Unsupervised Learning

# Prepare data (pre-processing and filtering)
prep_data:
	./pipeline/1-unsupervised/1-prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Map genes to NanoString
map_genes:
	./pipeline/1-unsupervised/2-map_genes.sh $(filter-out $@,$(MAKECMDGOALS))

# Consensus clustering
cluster:
	./pipeline/1-unsupervised/3-clustering.sh $(filter-out $@,$(MAKECMDGOALS))

# Consensus matrices
consmat:
	./pipeline/1-unsupervised/4-consmat.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge cluster assignments
merge_clust:
	./pipeline/1-unsupervised/5-merge_clust.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices
merge_cm:
	./pipeline/1-unsupervised/6a-merge_cm_partial.sh $(filter-out $@,$(MAKECMDGOALS)) && \
	./pipeline/1-unsupervised/6b-merge_cm_complete.sh $(filter-out $@,$(MAKECMDGOALS))

# Calculate consensus function ensembles
con_fun:
	./pipeline/1-unsupervised/7-con_fun.sh $(filter-out $@,$(MAKECMDGOALS))

# Final clusters and internal validity indices
final_clust:
	./pipeline/1-unsupervised/8-final_clust.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 2: Supervised Learning

# Train models and return evaluations
train_eval:
	./pipeline/2-supervised/1-train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge evaluations across reps and algorithms
merge_eval:
	./pipeline/2-supervised/2-merge_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# Confidence intervals for top supervised learning algorithms
top_ci:
	./pipeline/2-supervised/3-top_ci.sh $(filter-out $@,$(MAKECMDGOALS))

# Internal validation summary and combined across datasets
iv_summary:
	./pipeline/2-supervised/4-iv_summary.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 3: Post-processing

# Compare reference outputs
compare:
	./pipeline/3-post_processing/0-compare_reference.sh $(filter-out $@,$(MAKECMDGOALS))

# Internal validity plots
plot_iv:
	./pipeline/3-post_processing/1-internal_validity_plots.sh $(filter-out $@,$(MAKECMDGOALS))

# Evaluate batch effects
batch_effects:
	./pipeline/3-post_processing/2-evaluate_batch_effects.sh $(filter-out $@,$(MAKECMDGOALS))

# Validate overlap array and cut 2 samples
validation:
	./pipeline/3-post_processing/3-validation.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 4: Validate NanoString classifier

# NanoString overlap validation
ns_validate:
	./pipeline/4-nanostring_classifier/1-validate_nanostring.sh $(filter-out $@,$(MAKECMDGOALS))

# Predict NanoString data using all-array model
ns_all_array:
	./pipeline/4-nanostring_classifier/2-all_array_model.sh $(filter-out $@,$(MAKECMDGOALS))

# Predict NanoString data using TCGA model
ns_tcga:
	./pipeline/4-nanostring_classifier/3-tcga_model.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 5: Gene Selection in full NanoString

# Bootstrap frequencies of top selected genes
boot_freq:
	./pipeline/5-gene_selection/1-boot_freq.sh $(filter-out $@,$(MAKECMDGOALS))

# Summarized frequencies across studies
sum_freq:
	./pipeline/5-gene_selection/2-sum_freq.sh $(filter-out $@,$(MAKECMDGOALS))

# Train models using top genes
train:
	./pipeline/5-gene_selection/3-train.sh $(filter-out $@,$(MAKECMDGOALS))

# Predict left out study using trained models
predict:
	./pipeline/5-gene_selection/4-predict.sh $(filter-out $@,$(MAKECMDGOALS))

# Evaluate predictions using gene analysis, visualizations
evaluate:
	./pipeline/5-gene_selection/5-evaluate.sh $(filter-out $@,$(MAKECMDGOALS))

# Retrain models using top genes with finer granularity
retrain:
	./pipeline/5-gene_selection/6-retrain.sh $(filter-out $@,$(MAKECMDGOALS))

# Fit a final model and predict all NanoString and external data
final_model:
	./pipeline/5-gene_selection/7-final_model.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 6: cross-platform verification

# Analyze cross-platform
cp_analysis:
	./pipeline/6-cross_platform/1-cp_analysis.sh $(filter-out $@,$(MAKECMDGOALS))

# Predict cross-platform
cp_predictions:
	./pipeline/6-cross_platform/2-cp_predictions.sh $(filter-out $@,$(MAKECMDGOALS))


# Clean target
%:
		@:
clean:
	./assets/clean.sh
