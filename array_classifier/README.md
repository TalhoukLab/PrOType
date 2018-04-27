# Array Classification Pipeline Instructions

This documentation will guide you through the process of running the array classification pipeline, which entails the following:

1. `1_cluster_pipeline`: performs consensus clustering to determine HGSC subtypes then an ensemble supervised scheme to classify these subtypes, all of which are conducted in a cluster environment sequentially. 
2. `2_post_processing`: validates the top performing classifiers across batch correction methods (XPN and CBT) and selects the candidate model to be used with cross-platform prediction on Nanostring.

Please follow the manual below to run the pipeline on your system.

Data required by the pipeline will be provided upon request.



## Cluster Pipeline

---


`1_cluster_pipeline` requires the following directories:

1. `outputs`: output from analyses are generated into this outputs directory
2. `raw_data`: data provided to you with the name `raw_data` should replace this directory, which contains the processed and batch-corrected studies of interest.
3. `training_pipeline`: scripts necessary to run the analysis accompanied by a Makefile.

Each step below is contained in `training_pipeline` and belongs to its self-contained directory. Within each self-contained directory, there exists a `Parameter.sh` script which must be filled out prior to executing the particular job. 

---

### 1. Unsupervised Analysis

The unsupervised analysis performs consensus clustering to obtain the diceR labels 
in a distributed, cluster environment. The steps to execute this pipeline are as
follows:

1. `cd` into the `1_Unsupervised` directory from the main directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line 
          of the server if you are uncertain.
    * **type**: specify the platform type of the gene expression data you require (affy or agil)
    * **dataSet**: specify the name of the dataset you are calling
    * **reps**: number of bootstrap replications to run for each algorithm in the ensemble. (>=100)
    * **k**: the number of clusters (subtypes) you aim to identify (default to 4)
    * **workDir**: your working directory (path where this pipeline exists)
    * **outputDir**: the output directory (path where pipeline outputs will be stored)
    * **inputDir**: path of the raw input data used for clustering
    * **logDir**: path of your log files
    * **RPath**: path of your R executable
4. Once parameters have been specified, return to working directory (`cd ..`).
5. Run unsupervised analysis sequentially, ensuring each step is completed before continuing.
    1. `make files`
    2. `make clustQ`
    3. `make CMmergeQ`
    4. `make merge`
    5. `make ConFun`
    6. `make FinalClust`
6. Examine results in output folder. Final results are stored in `data_pr_nameOfStudy`, where `nameOfStudy` is of course, the name of the study.

---

### 2. Gene Mapping

This step will map the genes of your data to Nanostring in preparation for cross-platform analysis. The mapping identifies and selects those genes that intersect between both platforms. To run gene mapping, follow the steps below:

1. `cd` into the `2_Genemapping` directory from the main directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line 
          of the server if you are uncertain.
    * **type**: specify the platform type of the gene expression data you require (affy or agil)
    * **dataSet**: specify the name of the dataset you are calling
    * **housekeepingNormalize**: "TRUE" performs normalization with respect to the housekeeping genes, otherwise housekeeping genes are simply dropped.
    * **workDir**: your working directory (path where this pipeline exists)
    * **outputDir**: the output directory (path where pipeline outputs will be stored)
    * **inputDir**: path of the raw input data used for clustering
    * **logDir**: path of your log files
    * **RPath**: path of your R executable
4. Once parameters have been specified, return to working directory (`cd ..`).
5. Run `make map`
6. Examine results in output folder. Final results are stored in `data_pr_nameOfStudy`. 

---

### 3. Supervised Analysis

The supervised analysis performs an ensemble classification scheme which performs a specified number of bootstrap sampled fits for a selection of classification algorithms. Bootstrapping provides the user with out-of-sample error which provides a basis to compare models. The steps to run the supervised analysis are as follows:

1. `cd` into the `3_Supervised` directory from the main working directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line of the server if you are uncertain.
    * type: specify the platform type of the gene expression data you require (affy or agil)
    * **dataSet**: specify the name of the dataset you are calling
    * **reps**: number of bootstrap resamples to perform per algorithm
    * **IsHousekeepingNormalized**: "TRUE" indicates you are using housekeeping normalized data for your analysis. The normalization/no-normalization is decided during the gene mapping section.
    * **normalizeBy**: The type of normalization to perform on the data set. No normalization is recommended. See the package `splendid` for further details.
    * **minVar**: The minimum variance a gene should have across samples. Genes with variance less than `minVar` are dropped.
    * **threshold**: Predicted labels receive a class of "unclassified" whenever the largest predicted probability of a sample is less than the specified threshold. See implementation in `splendid:::class_threshold`.
    * **normType**: if `normalizeBy` is not None, then select the type of normalization (conventional or robust)
    * **workDir**: your working directory (path where this pipeline exists)
    * **outputDir**: the output directory (path where pipeline outputs will be stored)
    * **inputDir**: path of the raw input data used for clustering (may be same as output directory for the unsupervised analysis, since results of that analysis are being called here)
    * **logDir**: path of your log files
    * **RPath**: path of your R executable
4. Once parameters have been specified, return to working directory (`cd ..`).
5. Run supervised analysis sequentially, ensuring each step is completed before continuing.
    1. `make SLfiles`
    2. `make SLtrain`
    3. `make SLreduce`
6. Examine results in output folder. Final results are stored in `data_pr_nameOfStudy`. 
  
---

### 4. Internal Validation Summary

Internal validation will return a summary of the results for the supervised learning analysis conducted previously. These will include a ranking of the algorithms and the bootstrap confidence intervals for various metrics. The follow the steps below to return the IV summary:

1. `cd` into the `4_IVSummary` directory from the main working directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line of the server if you are uncertain.
    * **type**: specify the platform type of the gene expression data you require (affy or agil)
    * **dataSet**: specify a list of data sets separated by a space. i.e `dataSet=("ov.afc2_xpn ov.afc1_xpn")`
    * **top**: select the top N number of algorithms you wish to return.
    * **workDir**: your working directory (path where this pipeline exists)
    * **outputDir**: the output directory (path where pipeline outputs will be stored)
    * **inputDir**: path of the raw input data used for clustering
    * **logDir**: path of your log files
    * **RPath**: path of your R executable
4. Once parameters have been specified, return to working directory (`cd ..`).
5. Run the following:
    1. `make trainEval`
    2. `make supLearn`: returns ranked order of the top N models with bootstrap confidence intervals
    3. `IVsummary`: returns an R data.frame containing all evaluation metrics and models for simple analysis for the user.
6. Examine results in output folder. Final results are stored in `data_pr_nameOfStudy`. 


---

---


## Post Processing

---


`2_post_processing` contains the following directories:

* `outputs`: outputs generated by the analysis are placed here
* `utils`: utility/helper functions required for the analysis
* `data`: data provided to you with the name `data` should replace this directory, which contains all necessary data dependencies for the post processing.

---

#### Transferring `1_cluster_pipeline` results to `2_post_processing`

The `data` folder provided to you should contain all necessary data dependencies to run the post processing. If you are interested in examining the results returned after running the pipeline yourself, you must transfer the `data_pr_studyname`, where `studyname` is the name of the study, from the outputs of the pipeline and replace those corresponding directories (with the same name) located in the data folder. The `data_pr_studyname` contains all necessary outputs required for post-processing.

---

The following should be run in sequential order:

1. `step0_InternalValidation`: The supervised learning ensemble is evaluated through a series of visualizations of top performing algorithms.
2. `step1_BuildFinalModel.R`: The top models from the previous step and fit to the cut 1 array data and evaluated.
3. `step2_ArrayValidation.R`: The fitted models are evaluated against "cut 3", which contains the overlap samples. These results are benchmarked against published labels.
4. Transfer the outputs folder containing all outputs of steps 1 to 3 and the data folder to `PrOType/nanostring_classifier` and navigate to this directory and continue the following steps provided in that `README.md`.
