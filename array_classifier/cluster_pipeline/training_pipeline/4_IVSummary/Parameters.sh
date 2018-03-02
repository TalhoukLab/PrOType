#!/bin/sh

# user information
user="djohnson"

# data information
type="affy"
dataSet=("ov.afc2_cbt ov.afc1_cbt ov.af_cbt ov.afc2_xpn ov.afc1_xpn ov.af_xpn")

# Training Eval parameters
IsHousekeepingNormalized="TRUE"
normalizeBy="None"
top=5

# directory inputs
workDir="/share/lustre/backup/ovcare/HGSC_Classifier/final_pipeline/"
outputDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/"
inputDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/" # notice same as outputDir
logDir="/share/lustre/backup/ovcare/HGSC_Classifier/outputs/logs/"
RPath="/shahlab/djohnson/software/R-3.4.1/builddir/bin"
