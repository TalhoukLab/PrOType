s<-87
algs<- "dist"
pr<- "cs"
sfdir<- "/share/scratch/mpaletta_temp/outputs/ov.afc2_cbt"
ndat<- "ov.afc2_cbt"
datadir<- "/share/scratch/mpaletta_temp/outputs/ov.afc2_cbt/data_pr_ov.afc2_cbt"
cdat<- readRDS(paste0(datadir,"/cdat_","ov.afc2_cbt",".rds"))
source("~/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/1_Unsupervised/1_clust_data.R")
source("~/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/1_Unsupervised/2_impute_missing.R")
source("~/Projects/PrOType/array_classifier/1_cluster_pipeline/training_pipeline/1_Unsupervised/1_Unsupervised/3_con_mat.R")
