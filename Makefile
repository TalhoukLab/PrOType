# Makefile to test parallel pipeline
all: Unsupervised Genemapping Supervised IVSummary

Unsupervised: files cluster CMmerge merge ConFun FinalClust

Genemapping: map

Supervised: SLtrain SLreduce trainEval

IVSummary: trainEval supLearn IVsummary


# Prepare data (pre-processing and filtering)
prep_data:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for pipeline
files:	prep_data
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/CreateScripts.sh $(filter-out $@,$(MAKECMDGOALS))

# Running consensus clustering on the queue
cluster:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/subJobs.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices on the queue (not used)
CMmerge:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/MergeConsMat.sh $(filter-out $@,$(MAKECMDGOALS))

merge:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/MergeFinal.sh $(filter-out $@,$(MAKECMDGOALS))

ConFun:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/ConFun.sh $(filter-out $@,$(MAKECMDGOALS))

FinalClust:
	./array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/FinalClust.sh $(filter-out $@,$(MAKECMDGOALS))

map:
	./array_classifier/2_Genemapping/map.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for SL pipeline
SLfiles:
	./array_classifier/3_Supervised/CreateSLScripts.sh $(filter-out $@,$(MAKECMDGOALS))

# Run scripts to train models
SLtrain: SLfiles
	./array_classifier/3_Supervised/train.sh $(filter-out $@,$(MAKECMDGOALS))

SLreduce:
	./array_classifier/3_Supervised/reduce.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
trainEval:
	./array_classifier/4_IVSummary/train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
supLearn:
	./array_classifier/4_IVSummary/CIsupLearn.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
IVsummary:
	./array_classifier/4_IVSummary/ivSummary.sh $(filter-out $@,$(MAKECMDGOALS))

%:
		@:
clean:
		rm -rf ./TCGA; #rm -rf /Users/atalhouk/Desktop/HGSCdata/outputs/TCGA
