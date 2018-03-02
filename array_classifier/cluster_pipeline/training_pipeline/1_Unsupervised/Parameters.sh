#!/bin/sh

# user information
user="djohnson"

# data information
type="affy"
dataSet="ov.afc2_cbt"

# Unsupervised parameters
reps=100
k=4

# directory inputs
workDir="/share/lustre/backup/ovcare/HGSC_Classifier/final_pipeline/"
outputDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/"
inputDir="/share/lustre/backup/ovcare/HGSC_Classifier/raw_data/"
logDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/logs/"
RPath="/shahlab/djohnson/software/R-3.4.1/builddir/bin"
