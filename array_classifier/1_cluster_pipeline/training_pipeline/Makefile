# Makefile to test parallel pipeline

all: files clustQ CMmergeQ merge ConFun FinalClust map SLfiles SLtrain trainEval supLearn IVsummary

# Prepare data (pre-processing and filtering)
prep_data:
	./1_Unsupervised/prep_data.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for pipeline
files:	prep_data
	./1_Unsupervised/CreateScripts.sh $(filter-out $@,$(MAKECMDGOALS))

# Running consensus clustering on the queue
clustQ:
	./1_Unsupervised/subJobs.sh $(filter-out $@,$(MAKECMDGOALS))

# Merge consensus matrices on the queue (not used)
CMmergeQ:
	./1_Unsupervised/MergeConsMat.sh $(filter-out $@,$(MAKECMDGOALS))

merge:
	./1_Unsupervised/MergeFinal.sh $(filter-out $@,$(MAKECMDGOALS))

ConFun:
	./1_Unsupervised/ConFun.sh $(filter-out $@,$(MAKECMDGOALS))

FinalClust:
	./1_Unsupervised/FinalClust.sh $(filter-out $@,$(MAKECMDGOALS))

map:
	./2_Genemapping/map.sh $(filter-out $@,$(MAKECMDGOALS))

# Create scripts needed for SL pipeline
SLfiles:
	./3_Supervised/CreateSLScripts.sh $(filter-out $@,$(MAKECMDGOALS))

# Run scripts to train models
SLtrain:
	./3_Supervised/train.sh $(filter-out $@,$(MAKECMDGOALS))

SLreduce:
	./3_Supervised/reduce.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
trainEval:
	./4_IVSummary/train_eval.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
supLearn:
	./4_IVSummary/CIsupLearn.sh $(filter-out $@,$(MAKECMDGOALS))

# return internal validation summary
IVsummary:
	./4_IVSummary/ivSummary.sh $(filter-out $@,$(MAKECMDGOALS))

%:
		@:
clean:
		rm -rf ./TCGA; #rm -rf /Users/atalhouk/Desktop/HGSCdata/outputs/TCGA

