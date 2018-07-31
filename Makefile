# Makefile to test parallel pipeline
all: clustering post_processing nanostring gene_selection

# ------------- PART 1 ----------------
Unsupervised: files cluster CMmerge merge ConFun FinalClust

Genemapping: map

Supervised: SLtrain SLreduce

IVSummary: trainEval supLearn IVsummary

clustering: Unsupervised Genemapping Supervised IVSummary

# Prepare data (pre-processing and filtering)
prep_data:
	./R/unsupervised/prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for pipeline
files:	prep_data
	./R/unsupervised/create_scripts.sh $(filter-out $@,$(MAKECMDGOALS))

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
	./R/unsupervised/FinalClust.sh $(filter-out $@,$(MAKECMDGOALS))

map:
	./R/genemapping/map.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for SL pipeline
SLfiles:
	./R/supervised/create_scripts.sh $(filter-out $@,$(MAKECMDGOALS))

# Run scripts to train models
SLtrain: SLfiles
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


%:
		@:
clean:
		rm -r ./TCGA; . ./Parameters && rm -r $logDir && rm -r $outputDir && rm -r $workDir

