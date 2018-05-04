ndat<- "ov.afc1_cbt"
dir <- "/share/scratch/mpaletta_temp/outputs/ov.afc1_cbt"
algs<- strsplit("nmfbrunet nmflee distalgs rest", " ")[[1]]
merge <- "complete"
source("/home/mpaletta/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/5_merge_consmat.R")
