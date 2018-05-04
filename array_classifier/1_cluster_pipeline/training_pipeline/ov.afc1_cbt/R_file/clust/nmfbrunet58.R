s<-58
algs<- "nmfbrunet"
pr<- "cs"
sfdir<- "/share/scratch/mpaletta_temp/outputs/ov.afc1_cbt"
ndat<- "ov.afc1_cbt"
datadir<- "/share/scratch/mpaletta_temp/outputs/ov.afc1_cbt/data_pr_ov.afc1_cbt"
cdat<- readRDS(paste0(datadir,"/cdat_","ov.afc1_cbt",".rds"))
source("/home/mpaletta/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/1_clust_data.R")
source("/home/mpaletta/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/2_impute_missing.R")
source("/home/mpaletta/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/3_con_mat.R")
