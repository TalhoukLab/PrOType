# Parameters

## Analytical Parameters

The default parameters in `Parameters.sh` can be used to reproduce all our results from the PrOType pipeline. However, these parameters are intended to be modified as needed for your own analysis. Pay close attention to the directory parameters as absolute file paths need to exist on your machine.

```
# Data parameters
trainSet="ov.afc1_xpn"
trainSet2="ov.afc1_cbt"
testSet="ov.afc2_xpn"
dataSets=($trainSet $trainSet2 $testSet)

# Unsupervised parameters
reps=1000
k=4
c=50
referenceClass="majority"

# Supervised parameters
supervised_reps=500
normalizeBy="None"
threshold=0
minVar=0
normType="conventional"
top=5

# Gene Selection parameters
numBootstraps=500

# Directory parameters
user="$(whoami)"
rootDir="/extscratch/shahlab/huntsman"/$user
inputDir=$rootDir"/Projects/PrOType/raw_data"
scriptDir=$rootDir"/scripts"
RDir=$scriptDir"/R"
shDir=$scriptDir"/sh"
outputDir=$rootDir"/outputs"
logDir=$rootDir"/logs"
```

## Developer Parameters

There are additional parameters in `assets/dev_params.sh` that are not intended to be modified on a regular basis. Developers who are familiar with the pipeline can choose to change the algorithms that are used in each step, the colours of screen outputs, or tune the performance of the queue.

```
# Developer parameters
shouldCompute=FALSE
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
supervisedAlgs=(first second third fourth)
geneSelectionAlgs=(lasso rf ada)

# Bash parameters
RPath="$(which R | sort | tail -n 1)"
GREEN_TICK='\033[0;32m\xe2\x9c\x94\033[0m'
GREEN_BULLET='\033[0;32m\xe2\x80\xa2\033[0m'
BLUE_BULLET='\033[0;34m\xe2\x80\xa2\033[0m'
RED_CROSS='\033[0;31m\xe2\x9c\x96\033[0m'

# Queue parameters
maxQueueLength=8000
shouldWait=TRUE
mem_free="4G"
mem_token="4G"
h_vmem="8G"
```

# Parameter Definitions

## Data

- `trainSet`: the first training set is XPN-corrected cut 1 data
- `trainSet2`: the second training set is CBT-corrected cut 1 data
- `testSet`: the test set is XPN-corrected cut 2 data
- `dataSets`: an array of all datasets

## Unsupervised

- `reps`: number of bootstrap repetitions to run for each algorithm in the consensus clustering. (Recommended >= 100)
- `k`: number of clusters (subtypes) you aim to identify
- `c`: number of splits in two-stage of merging of consensus matrices. Note that `reps` must be divisible by `c`. For example, if `reps = 1000` and `c = 50`, we first merge the consensus matrices in 20 groups of 50, and then merge the 20 groups to collect all 1000.
- `referenceClass`: the reference class to use when relabelling final clustering assignments.

## Supervised
- `supervised_reps`: number of bootstrap samples to run classification algorithms on 
- `normalizeBy`: normaliztion to perform on the data set. No normalization is recommended. See the package `splendid` for further details.
- `threshold`: predicted labels receive a class of "unclassified" whenever the largest predicted probability of a sample is less than a specified threshold. See implementation in `splendid:::class_threshold`.
- `minVar`: minimum variance a gene should have across samples. Genes with variance less than `minVar` are dropped.
- `normType`: if `normalizeBy` is not "None", then select the type of normalization to use for scaling genes ("conventional" uses the standard deviation, "robust" uses the median absolute deviation).
- `top`: number of top algorithms from supervised pipeline as determined by rank aggregation on internal validity indices.

## Gene Selection
- `numBootstraps`: number of bootstrap samples to run gene selection algorithms on

## Directory
- `inputDir`: input directory where raw data exists for clustering.
- `scriptDir`: script directory where R and bash scripts exist. Used to submit batch jobs to the queue.
- `outputDir`: output directory where outputs are written to.
- `logDir`: log directory where queue logs are written to. Logs are written in a directory hierarchy similar to `outputDir`.

## Developer
- `shouldCompute`: should outputs be computed if they already exist from previous runs of the pipeline?
- `algs`: unsupervised learning algorithms
- `cons`: consensus functions used for ensemble clustering
- `supervisedAlgs`: supervised learning algorithms
- `geneSelectionAlgs`: gene selection algorithms

## Bash
- `user`: your user name on a Linux distribution
- `RPath`: path to R executable. This parameter is robust to aliases set to `R`.
- `GREEN_TICK`: green tick printed during job submission to indicate success.
- `GREEN_BULLET`: green bullet printed at end of job submission to indicate completion of task.
- `BLUE_BULLET`: blue bullet printed during job submission to indicate pending jobs.
- `RED_CROSS`: red cross printed at end of job submission if there are log files with lines written to standard error.

## Queue
- `maxQueueLength`: maximum queue length. Job submission is delayed if number of running jobs exceeds this value. This value is not recommended for modification as a large value may crash the server.
- `shouldWait`: should a task wait for the previous pipeline step to finish before proceeding?
- `mem_free`: amount of free memory currently available
- `mem_token`: amount of memory intended for consumption
- `h_vmem`: total virtual memory allowed for a job
