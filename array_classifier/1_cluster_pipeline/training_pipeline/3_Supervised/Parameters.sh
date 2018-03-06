#!/bin/sh

# user information
user="djohnson"

# data information
type="affy"
dataSet="ov.afc2_cbt"

# Supervised parameters
reps=100
IsHousekeepingNormalized="TRUE"
normalizeBy="None"
threshold="0.0"
minVar="0.0"
normType="conventional"

# directory inputs
workDir="/share/lustre/backup/ovcare/HGSC_Classifier/final_pipeline/"
outputDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/"
inputDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/" # notice same as outputDir
logDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/logs/"
RPath="/shahlab/djohnson/software/R-3.4.1/builddir/bin"
