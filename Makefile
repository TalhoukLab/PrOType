# Makefile to run PrOType pipeline

# Run full pipeline
all: array nanostring gene_selection cross_platform

# Intermediate targets
array: unsupervised supervised post_processing

unsupervised: prep_data map_genes cluster CMmerge merge ConFun FinalClust

supervised: SLtrain SLreduce trainEval supLearn IVsummary

post_processing: compare plot_iv validation

gene_selection: train sum_freqs final_train predict gs_pp part2 final_model

cross_platform: cp_analysis cp_predictions


# Part 1: unsupervised learning

# Prepare data (pre-processing and filtering)
prep_data:
	./R/1-unsupervised/1-prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Map genes to NanoString
map_genes:
	./R/1-unsupervised/2-map_genes.sh $(filter-out $@,$(MAKECMDGOALS))

# Consensus clustering
cluster:
	./R/1-unsupervised/3-clustering.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices
CMmerge:
	./R/1-unsupervised/4-merge_cons_mat.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge cluster assignments and all consensus matrices
merge:
	./R/1-unsupervised/5-merge_final.sh $(filter-out $@,$(MAKECMDGOALS))

# Calculate ensembles
ConFun:
	./R/1-unsupervised/6-con_fun.sh $(filter-out $@,$(MAKECMDGOALS))

# Final clusters and internal validitiy indices
FinalClust:
	./R/1-unsupervised/7-final_clust.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 2: supervised learning

# Train models and return evaluations
SLtrain:
	./R/2-supervised/1-train.sh $(filter-out $@,$(MAKECMDGOALS))

# Combine evaluations across reps
SLreduce:
	./R/2-supervised/2-reduce.sh $(filter-out $@,$(MAKECMDGOALS))

# Combine all training evaluations
trainEval:
	./R/2-supervised/3-train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# Confidence intervals for top supervised learning algorithms
supLearn:
	./R/2-supervised/4-CIsupLearn.sh $(filter-out $@,$(MAKECMDGOALS))

# Internal validation summary
IVsummary:
	./R/2-supervised/5-ivSummary.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 3: Post-processing

# compare reference outputs
compare:
	./R/3-post_processing/0-compare_reference.sh $(filter-out $@,$(MAKECMDGOALS))

# internal validity plots
plot_iv:
	./R/3-post_processing/1-internal_validity_plots.sh $(filter-out $@,$(MAKECMDGOALS))

# validate overlap array and cut2 samples
validation:
	./R/3-post_processing/2-validation.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 4: validate NanoString classifier

# NanoString overlap validation and prediction
nanostring:
	./R/4-nanostring_classifier/run_nanostring.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 5: gene selection in full NanoString
train:
	./R/5-gene_selection/1-run_bootstrap.sh $(filter-out $@,$(MAKECMDGOALS))

sum_freqs:
	./R/5-gene_selection/2-process_bootstrap.sh $(filter-out $@,$(MAKECMDGOALS))

final_train:
	./R/5-gene_selection/3-run_final_training.sh $(filter-out $@,$(MAKECMDGOALS))

predict:
	./R/5-gene_selection/4-make_predictions.sh $(filter-out $@,$(MAKECMDGOALS))

gs_pp:
	./R/5-gene_selection/5-training_post_processing.sh $(filter-out $@,$(MAKECMDGOALS))

part2:
	./R/5-gene_selection/6-training_part2.sh $(filter-out $@,$(MAKECMDGOALS))

final_model:
	./R/5-gene_selection/7-build_final_model.sh $(filter-out $@,$(MAKECMDGOALS))


# Part 6: cross-platform verification
cp_analysis:
	./R/6-cross_platform/1-cp_analysis.sh $(filter-out $@,$(MAKECMDGOALS))

cp_predictions:
	./R/6-cross_platform/2-cp_predictions.sh $(filter-out $@,$(MAKECMDGOALS))


# ---------- DEBUG CHECKPOINTS ---------
from-Unsupervised: Unsupervised from-Supervised

from-Supervised: Supervised from-IVSummary

from-IVSummary: IVSummary from-post_processing

from-post_processing: post_processing gene_selection


# Clean target
%:
		@:
clean:
	./assets/clean.sh
