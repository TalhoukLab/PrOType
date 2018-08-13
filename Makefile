# Makefile to test parallel pipeline
all: clustering post_processing gene_selection cross_platform

# ------------- PART 1 ----------------
Unsupervised: init cluster CMmerge merge ConFun FinalClust

Supervised: SLtrain SLreduce trainEval

IVSummary: supLearn IVsummary

clustering: Unsupervised Genemapping Supervised IVSummary

# Prepare data (pre-processing and filtering)
prep_data:
	./R/1_unsupervised/prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for pipeline
init: prep_data Genemapping

# Running consensus clustering on the queue
cluster:
	./R/1_unsupervised/submit_clustering.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices on the queue (not used)
CMmerge:
	./R/1_unsupervised/merge_cons_mat.sh $(filter-out $@,$(MAKECMDGOALS))

merge:
	./R/1_unsupervised/merge_final.sh $(filter-out $@,$(MAKECMDGOALS))

ConFun:
	./R/1_unsupervised/con_fun.sh $(filter-out $@,$(MAKECMDGOALS))

FinalClust:
	./R/1_unsupervised/final_clust.sh $(filter-out $@,$(MAKECMDGOALS))

Genemapping:
	./R/2_genemapping/map.sh $(filter-out $@,$(MAKECMDGOALS))

# Run scripts to train models
SLtrain:
	./R/3_supervised/train.sh $(filter-out $@,$(MAKECMDGOALS))

SLreduce:
	./R/3_supervised/reduce.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
trainEval:
	./R/3_supervised/train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
supLearn:
	./R/3_supervised/CIsupLearn.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
IVsummary:
	./R/3_supervised/ivSummary.sh $(filter-out $@,$(MAKECMDGOALS))


# ----------- PART 2 -------------
post_processing:
	./R/5_post_processing/run_post_processing.sh $(filter-out $@,$(MAKECMDGOALS))

nanostring:
	./R/nanostring_classifier/run_nanostring.sh $(filter-out $@,$(MAKECMDGOALS))


# ----------- PART 4 -------------
gene_selection:
	./R/6_gene_selection/run_gene_selection.sh $(filter-out $@,$(MAKECMDGOALS))

cross_platform:
	./R/7_cross_platform/run_cross_platform.sh $(filter-out $@,$(MAKECMDGOALS))


# ---------- DEBUG CHECKPOINTS ---------
from-Unsupervised: Unsupervised from-Supervised

from-Supervised: Supervised from-IVSummary

from-IVSummary: IVSummary from-post_processing

from-post_processing: post_processing gene_selection

%:
		@:
clean:
	./assets/clean.sh
