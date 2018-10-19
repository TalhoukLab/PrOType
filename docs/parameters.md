
# Parameters
Below are the default parameters from `Parameters.sh` that can be used to reproduce our results from the PrOType pipeline. You can modify these parameters as needed for your own analysis, paying close attention to absolute file paths appropriate for your machine.

```
# data information
dataSets=(ov.afc1_cbt ov.afc1_xpn)
shouldCompute=FALSE

# Unsupervised parameters
reps=1000
k=4
c=50

# Supervised parameters
supervised_reps=500
normalizeBy="None"
threshold=0.0
minVar=0
normType="conventional"

# IV Summary
top=5

# Post Processing
trainSet="ov.afc1_xpn"
trainSet2="ov.afc1_cbt"
testSet="ov.afc2_xpn"

# Gene Selection
numBootstraps=500

# directory inputs
workDir="/extscratch/shahlab/huntsman/dchiu/workDir"
outputDir="/extscratch/shahlab/huntsman/dchiu/outputs"
inputDir="/home/dchiu/Projects/PrOType/raw_data"
baseLogDir="/extscratch/shahlab/huntsman/dchiu/logs"
```

There are additional parameters in `assets/dev_params.sh` that can be configured to alter the way the pipeline is run:

```
# Developer params
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
classificationAlgs=(adaboost rf mlr_ridge mlr_lasso)
supervisedAlgs=(first second third fourth)
```
