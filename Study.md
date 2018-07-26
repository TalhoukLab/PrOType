# Getting Started
This guide is the steps to replicate the published study using this pipeline.

# Installation

If you are running on Genesis, run: (changing parameters where necessary)
`
rsync -azv --checksum --progress -e 'ssh -o "ProxyCommand ssh -A jsmith@ssh.bcgsc.ca -W %h:%p"' ~/Documents/Projects/PrOType jsmith@genesis2:~/
bash /gsc/software/linux-x86_64-centos5/R-3.5.1/source_me.sh
`

In your terminal run:
`
Rscript install_packages.R
`
and ensure that all dependencies are installed successfully.

Alternatively, you can use the docker image, and run the code in there.  This docker image is already configured with all the dependencies already installed.

# Running
Below is the parameters, to be updated in `Parameters.sh` needed to replicate our results.  Please ensure that these match your `Parameters.sh` file, and that appropriate filepaths are set for your machine.
```
dataSets=(ov.afc1_cbt ov.afc1_xpn)
shouldCompute=TRUE

# Unsupervised parameters
reps=1000
k=4
c=25 # use for determining splitting criterion (min 100 reps required)

# Supervised parameters
supervised_reps=500
normalizeBy="None"
threshold=0.0
minVar=0
normType="conventional"

# IV Summary
top=3

# Post Processing
trainSet="ov.afc1_xpn"
testSet="ov.afc2_xpn"

# directory inputs
workDir="/PrOType/array_classifier/1_cluster_pipeline/"
outputDir="/outputs/"
inputDir="/PrOType/raw_data/"
logDir="/logs/"
```

Additionally, there are more parameters in `assets/dev_params.sh` that need to be configured as such:
```
# Developer params
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
referenceClass="majority"
classificationAlgs=(adaboost rf mlr_ridge mlr_lasso)
supervisedAlgs=(first second third fourth)
```

After you have successfully configured `Parameters.sh`, run `make all`.  This will run the entire pipeline end-to-end, replicating our results.  This may take some time to complete.  Using a cluster with `qsub` is highly encouraged.  You will also require the `.RData` files needed for each study to be in your `inputDir` directory on your machine.  Please contact us to request access to these files.
