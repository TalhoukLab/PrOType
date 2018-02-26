
# Compute the consensus Matrix
conmat <-diceR:::consensus_summary(ssclust) 
saveRDS(conmat,paste0(sfdir,"/con_mat_",ndat,"/CM_",algs,s,"_",ndat,".rds"))
