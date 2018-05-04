ndat<- "ov.afc1_cbt"
dir <- "/share/scratch/mpaletta_temp/outputs/ov.afc1_cbt"
algs<- strsplit("nmf dist other", " ")[[1]]
merge <- "complete"
source("~/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/1_Unsupervised/5_merge_consmat.R")
