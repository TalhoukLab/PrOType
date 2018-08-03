# Makefile to test parallel pipeline
all: clustering post_processing gene_selection cross_platform

# ------------- PART 1 ----------------
Unsupervised: init cluster CMmerge merge ConFun FinalClust

Supervised: SLtrain SLreduce

IVSummary: trainEval supLearn IVsummary

clustering: Unsupervised Genemapping Supervised IVSummary

# Prepare data (pre-processing and filtering)
prep_data:
	./R/unsupervised/prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for pipeline
init: prep_data Genemapping

# Running consensus clustering on the queue
cluster:
	./R/unsupervised/submit_clustering.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices on the queue (not used)
CMmerge:
	./R/unsupervised/merge_cons_mat.sh $(filter-out $@,$(MAKECMDGOALS))

merge:
	./R/unsupervised/merge_final.sh $(filter-out $@,$(MAKECMDGOALS))

ConFun:
	./R/unsupervised/con_fun.sh $(filter-out $@,$(MAKECMDGOALS))

FinalClust:
	./R/unsupervised/final_clust.sh $(filter-out $@,$(MAKECMDGOALS))

Genemapping:
	./R/genemapping/map.sh $(filter-out $@,$(MAKECMDGOALS))

# Run scripts to train models
SLtrain:
	./R/supervised/train.sh $(filter-out $@,$(MAKECMDGOALS))

SLreduce:
	./R/supervised/reduce.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
trainEval:
	./R/IV_summary/train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
supLearn:
	./R/IV_summary/CIsupLearn.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
IVsummary:
	./R/IV_summary/ivSummary.sh $(filter-out $@,$(MAKECMDGOALS))


# ----------- PART 2 -------------
post_processing:
	./R/post_processing/run_post_processing.sh $(filter-out $@,$(MAKECMDGOALS))

nanostring:
	./R/nanostring_classifier/run_nanostring.sh $(filter-out $@,$(MAKECMDGOALS))


# ----------- PART 4 -------------
gene_selection:
	./R/gene_selection/run_gene_selection.sh $(filter-out $@,$(MAKECMDGOALS))

cross_platform:
	./R/cross_platform/run_cross_platform.sh $(filter-out $@,$(MAKECMDGOALS))


# ---------- DEBUG CHECKPOINTS ---------
from-Unsupervised: Unsupervised from-Supervised

from-Supervised: Supervised from-IVSummary

from-IVSummary: IVSummary from-post_processing

from-post_processing: post_processing gene_selection

%:
		@:
clean:
	./assets/clean.sh
