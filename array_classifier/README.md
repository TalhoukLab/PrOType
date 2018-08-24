# Array Classification Pipeline Instructions

This documentation will guide you through the process of running the array classification pipeline, which entails the following:

1. `1_cluster_pipeline`: performs consensus clustering to determine HGSC subtypes using an unsupervised ensemble  method and performs supervised analysis to classify these subtypes, all of which are conducted in a computer cluster environment sequentially. 

2. `2_post_processing`: validates the top performing classifiers for different batch correction methods (XPN and CBT) and selects the candidate model to be used for prediction on Nanostring.

Please follow the manual below to run the pipeline on your system.

Data required by the pipeline is provided upon request.



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
    2. `make cluster`
    3. `make CMmerge`
    4. `make merge`
    5. `make ConFun`
    6. `make FinalClust`
6. Examine results in output folder. Final results are stored in `data_pr_<name_of_study>`, where `<name_of_study>` is of course, the name of the study.

---

### 2. Gene Mapping

This step will map the genes of your data to Nanostring in preparation for cross-platform analysis. The mapping identifies and selects those genes that intersect between both platforms. To run gene mapping, follow the steps below:

1. `cd` into the `2_Genemapping` directory from the main directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line 
          of the server if you are uncertain.
    * **dataSet**: specify the name of the dataset you are calling
    * **workDir**: your working directory (path where this pipeline exists)
    * **outputDir**: the output directory (path where pipeline outputs will be stored)
    * **inputDir**: path of the raw input data used for clustering
    * **logDir**: path of your log files
    * **RPath**: path of your R executable
4. Once parameters have been specified, return to working directory (`cd ..`).
5. Run `make Genemapping`
6. Examine results in output folder. Final results are stored in `data_pr_<name_of_study>`.

---

### 3. Supervised Analysis

The supervised analysis performs an ensemble classification scheme which performs a specified number of bootstrap sampled fits for a selection of classification algorithms. Bootstrapping provides the user with out-of-sample error which provides a basis to compare models. The steps to run the supervised analysis are as follows:

1. `cd` into the `3_Supervised` directory from the main working directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line of the server if you are uncertain.
    * **dataSet**: specify the name of the dataset you are calling
    * **reps**: number of bootstrap resamples to perform per algorithm
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
    1. `make SLtrain`
    2. `make SLreduce`
6. Examine results in output folder. Final results are stored in `data_pr_<name_of_study>`.

---

### 4. Internal Validation Summary

Internal validation will return a summary of the results for the supervised learning analysis conducted previously. These will include a ranking of the algorithms and the bootstrap confidence intervals for various metrics. The follow the steps below to return the IV summary:

1. `cd` into the `4_IVSummary` directory from the main working directory.
2. Open `Parameters.sh` with your favourite text editor.
3. Specify the parameters specific to your system and purpose.
    * **user**: enter your username for your system. Type `whoami` in the command line of the server if you are uncertain.
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
6. Examine results in output folder. Final results are stored in `data_pr_<name_of_study>`.


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

1. `0-validate_baseline_results.R`: Compare results from cluster pipeline to see if they changed from repeated runs
2. `1-evaluate_batch_effects.R`: Use PVCA and PCA plots to choose the best batch effect correction method
3. `2-internal_validitiy_plots.R`: The supervised learning ensemble is evaluated through a series of visualizations of top performing algorithms
4. `3-fit_top_models.R`: The top 5 models from the array classiifer are fit to the full cut 1 array data
5. `4-validate_array.R`: The fitted models are used to predict and evaluated against the overlapping array samples. These results are benchmarked against published labels.
6. `5-predict_C2.R`: The fitted models are used to predict the cut 2 array data
7. `6-probe_mapping_C2v2.R`: Map the cut 2 predictions to subtypes and generate centroid data for each gene
8. `7-mapping_signatures_C2.R`: Compare cut 2 predictions with Verhaaks signature and compute correlation matrices and calculate entropy and quadratic loss

Transfer the outputs folder containing all outputs of steps 1 to 3 and the data folder to `PrOType/nanostring_classifier` and navigate to this directory and continue the following steps provided in that `README.md`.
