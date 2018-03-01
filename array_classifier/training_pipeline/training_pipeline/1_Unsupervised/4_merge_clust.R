# Merge results together to form array

# Inputs: dir, reps, algs

library(abind)
library(dplyr)
library(magrittr)

multMerge <- function(algs, fnames, newdir){
#Seperate the algorithms
algF <- switch (algs,
	nmfbrunet = unique(grep(c("nmfbrunet"), fnames, value=TRUE)),
	nmflee = unique(grep(c("nmflee"), fnames, value=TRUE)),
	distalgs = unique(grep(c("dist"), fnames, value=TRUE)),
	rest = unique(grep(c("rest"), fnames, value=TRUE))
)

#Get the seeds
temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
Seeds <- as.numeric(purrr::map_chr(temp,`[`,1))
error <- 0

# Merge the seeds within algorithm when all have completed
if (!all(c(1:reps)%in%Seeds)){
	cat (paste(algs,"failed:"))
	cat(which(!(c(1:reps)%in%Seeds)))
	error <- 1
}

# Merge RDS_out
lalgo <- lapply(paste0(newdir,algF),readRDS) %>% abind(., along = 2)
dimnames(lalgo)[[2]] = paste0("R",Seeds)
return(lalgo)
}

#Merge the raw clustering	
fnames <- gtools::mixedsort(list.files(path = paste0(dir,"/rds_out_",ndat)))
newdir <- paste0(dir,"/rds_out_",ndat,"/")
E <- abind(lapply(algs, multMerge, fnames = fnames, newdir=newdir), along = 3)
saveRDS(E,file = paste0(dir,paste0("/data_pr_",ndat,"/E_",ndat,".rds")))

rm(fnames,E, newdir)

#Merge KNN_imputed clustering
fnames <- gtools::mixedsort(list.files(path = paste0(dir,"/imputed_clust_",ndat,"/")))
newdir <- paste0(dir,"/imputed_clust_",ndat,"/")
E_knn <- abind(lapply(algs, multMerge, fnames = fnames, newdir=newdir), along = 3)
cdat <- readRDS(paste0(dir,"/data_pr_",ndat,"/cdat_",ndat,".rds"))
saveRDS(E_knn,file = paste0(dir,"/data_pr_",ndat,"/E_knn_",ndat,".rds"))
Ecomp <- diceR::impute_missing(E_knn, data=cdat,nk=4)
saveRDS(Ecomp,file = paste0(dir,"/data_pr_",ndat,"/Ecomp_",ndat,".rds"))

rm(fnames,E_knn, newdir,Ecomp)
