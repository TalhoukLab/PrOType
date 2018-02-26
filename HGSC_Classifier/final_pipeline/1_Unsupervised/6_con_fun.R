require(diceR)
require(dplyr)
require(magrittr)
require(mclust)

#inputs: cons.funs, sim.mat ,k ,dir

hc <- function(d, k, method = "average") {
	return(as.integer(stats::cutree(stats::hclust(dist(d), method = method), k)))
}

#cons.fun
Ecomp <-  readRDS(paste0(dir,paste0("/Ecomp","_",ndat,".rds")))
CM <- readRDS(paste0(dir,paste0("/Final_CM","_",ndat,".rds")))#Read in the consensus matrices
cdat <- readRDS(paste0(dir,paste0("/cdat","_",ndat,".rds"))) #Read in the data used for clustering

#Obtain HC from each algorithm and relabel
cl.mat <- purrr::map(CM, hc, k=4) %>% 
	lapply(., diceR::relabel_class, ref.cl = .$NMF_Brunet) %>% 
	data.frame()


# Run LCE on cl.mat
cts <- hc(cts(as.matrix(cl.mat),dc=0.8),4)
srs <- hc(srs(as.matrix(cl.mat),dc=0.8,R=10),4)
asrs <- hc(asrs(as.matrix(cl.mat),dc=0.8),4)

#Consensus Function
Consensus <- switch(cons.funs,
	CSPA = CM %>% 
		Reduce("+", .) %>% 
		magrittr::divide_by(length(CM)) %>% 
		stats::dist() %>% 
		hc(k = k),
	kmodes = k_modes(Ecomp),
	majority = majority_voting(Ecomp),
	LCEcts= hc(cts(as.matrix(cl.mat),dc=0.8),4),
	LCEsrs= hc(srs(as.matrix(cl.mat),dc=0.8,R=10),4),
	LCEasrs= hc(asrs(as.matrix(cl.mat),dc=0.8),4)
)

saveRDS(Consensus,paste0(dir,"/cons_",cons.funs,"_",ndat,".rds"))
