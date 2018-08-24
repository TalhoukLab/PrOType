This document outlines the input/outputs from each step (R file) of the pipeline.

### TODO: Create a pretty image of this...


# Clustering
## Unsupervised

### read_data.R
- Inputs
* {dataset}.RData
- Outputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/tdat_{dataset}.rds
	* {outputDir}/{dataset}/{data_pr_{dataset}/cdat_{dataset}.rds

### clust_data.R
- Inputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/cdat_{dataset}.rds
- Outputs
	* (ssclust) -> {outputDir}/{dataset}/{rds_out_{dataset}/{unsupervised_algorithm}{repetition}_{dataset}.rds

### impute_missing.R
- Inputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/cdat_{dataset}.rds
	* (ssclust)
- Outputs
	* {outputDir}/{dataset}/imputed_clust_{dataset}/E_knn_{unsupervised_algorithm}{repetition}_{dataset}.rds

### con_mat.R
- Inputs
	* (ssclust)
- Outputs
	* {outputDir}/{dataset}/con_mat_{dataset}/CM_{unsupervised_algorithm}{repetition}_{dataset}.rds

### merge_partial_consmat.R
- Inputs
	* [{outputDir}/{dataset}/rds_out_{dataset}/{unsupervised_algorithm}{*}_{dataset}.rds]
- Outputs
	* {outputDir}/{dataset}/con_mat_merged_{dataset}/{repetition}_{unsupervised_algorithm}_consmat_{dataset}.rds}

### merge_clust.R
- Inputs
	* [{outputDir}/{dataset}/rds_out_{dataset}/{unsupervised_algorithm}{*}_{dataset}.rds]
	* [{outputDir}/{dataset}/imputed_clust_{dataset}/{*}{*}_{dataset}.rds]
	* {outputDir}/{dataset}/{data_pr_{dataset}/cdat_{dataset}.rds
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/E__{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/E_knn_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/Ecomp_{dataset}.rds

### merge_complete_consmat.R
- Inputs
	* [{outputDir}/{dataset}/con_mat_merged_{dataset}/{repetition}_{unsupervised_algorithm}_consmat_{dataset}.rds}]
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/Final_CM__{dataset}.rds

### con_fun.R
- Inputs
	* {outputDir}/{dataset}/data_pr_{dataset}/Ecomp_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/Final_CM__{dataset}.rds
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/cons_{consensus_function}_{dataset}.rds

### Evaluation.R
- Inputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/cdat_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/Final_CM__{dataset}.rds
	* [{outputDir}/{dataset}/data_pr_{dataset}/cons_{*}_{dataset}.rds]
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/all_clusts_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/ii__{dataset}.rds

## GeneMapping
### GeneMapping.R
- Inputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/tdat_{dataset}.rds
	* assets/data/HGNC-Affymetrix-NanoString-OTTA_Map.csv
- Outputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/npcp-hcNorm_{dataset}.rds
	* {outputDir}/{dataset}/{data_pr_{dataset}/tdat_mapped_{dataset}.rds

## Supervised
### model_train.R
- Inputs
	* {outputDir}/{dataset}/{data_pr_{dataset}/npcp-hcNorm_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/all_clusts_{dataset}.rds
- Outputs
	* {outputDir}/{dataset}/Model-hc_{dataset}/c1_{supervised_algorithm}{repetition}_{dataset}.rds

### reduce.R
- Inputs
	* [{outputDir}/{dataset}/Model-hc_{dataset}/c1_{supervised_algorithm}{*}_{dataset}.rds]
- Outputs
	* {outputDir}/{dataset}/Model-hc_{dataset}/{supervised_algorithm}_train_eval_{dataset}.rds

## IVSummary
### train_eval.R
- Inputs
	* [{outputDir}/{dataset}/Model-hc_{dataset}/c1_{*}{*}_{dataset}.rds]
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/train_eval_{dataset}.rds

### CIsupLearn.R
- Inputs
	* {outputDir}/{dataset}/data_pr_{dataset}/all_clusts_{dataset}.rds
	* {outputDir}/{dataset}/data_pr_{dataset}/train_eval_{dataset}.rds
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/sup_lrn_{dataset}.csv

### ivSummary.R
- Inputs
	* [{outputDir}/{dataset}/data_pr_{dataset}/train_eval_{dataset}.rds}]
- Outputs
	* {outputDir}/{dataset}/data_pr_{dataset}/iv_summary_{dataset}{threshold}?.rds

### ivCombine.R
- Inputs
	* cp {outputDir}/{dataset}/data_pr_{dataset} {outputDir}/iv_summary
	* {outputDir}/iv_summary/data_pr_{dataset}/iv_summary_{dataset}{threshold}?.rds'
- Outputs
	* {outputDir}/iv_summary/iv_summary_COMBINED.rds

## Post-Processing
#### TODO



# Makefile Mapping
prep_data
	- Unsupervised/read_data.R
	- Unsupervised/prep_data.R
files
	- <<prep_data>>
	- Unsupervised/CreateScripts.R
cluster
	- Unsupervised/clust_data.R
	- Unsupervised/impute_missing.R
	- Unsupervised/con_mat.R
CMmerge
	- Unsupervised/merge_partial_consmat.R
merge
	- Unsupervised/merge_clust.R
	- Unsupervised/merge_complete_consmat.R
ConFun
	- Unsupervised/con_fun.R
FinalClust
	- Unsupervised/Evalutuation.R
map
	- Genemapping/GeneMapping.R

SLfiles
	- Supervised/CreateSLScripts.sh

SLtrain
	- <<SLfiles>>
	- Supervised/model_train.R

SLreduce
	- Supervised/reduce.R

trainEval
	- IVSummary/train_eval.R

supLearn
	- IVSummary/CIsupLearn.R

IVSummary
	- IVSummary/ivSummary.R
	- IVSummary/ivCombine.R

#### TODO:// Postprocessing, Nanostring, testing_hgsc
