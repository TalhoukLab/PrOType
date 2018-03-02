E_knn <- apply(ssclust, 2:4, impute_knn, data = cdat, seed = 123)
saveRDS(E_knn,paste0(sfdir,"/imputed_clust_",ndat,"/E_knn_",algs,s,"_",ndat,".rds"))
