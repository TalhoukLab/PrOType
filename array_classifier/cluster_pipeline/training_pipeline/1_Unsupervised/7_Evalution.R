#rm(list=ls(all=TRUE))

#Packages----
library(tidyverse)
library(magrittr)
library(diceR)

# Functions----
hc <- function(d, k, method = "average") {
	return(as.integer(stats::cutree(stats::hclust(dist(d), method = method), k)))
}

# Input-----
alldat <-c(dat)

# Final Clustering ----
for(i in seq_along(alldat)){
	
ddir <- paste0(fdir,alldat[i],"/data_pr_",alldat[i])
print(ddir)

#Read in the data used for clustering
cdat<-  readRDS(paste0(ddir,"/cdat_",alldat[i],".rds"))

#Read in the consensus matrices
cons.mat <- readRDS(paste0(ddir,"/Final_CM_",alldat[i],".rds"))

#Obtain HC from each algorithm and relabel
cl.mat <- purrr::map(cons.mat, hc, k=4) %>% 
	lapply(., diceR::relabel_class, ref.cl = .$NMF_Brunet) %>% 
	data.frame()

final <- data.frame(
	CSPA = readRDS(paste0(ddir,"/cons_CSPA_",alldat[i],".rds")),
	kmodes = readRDS(paste0(ddir,"/cons_kmodes_",alldat[i],".rds")),
	majority = readRDS(paste0(ddir,"/cons_majority_",alldat[i],".rds")),
	cts= readRDS(paste0(ddir,"/cons_LCEcts_",alldat[i],".rds")),
	srs= readRDS(paste0(ddir,"/cons_LCEsrs_",alldat[i],".rds")),
	asrs= readRDS(paste0(ddir,"/cons_LCEasrs_",alldat[i],".rds")),
	cl.mat)

# relabel the elements of the data frame
finalR <- final
finalR[] <- apply(final, 2, diceR::relabel_class, ref.cl = final[,"majority"])

#Cluster evaluate at this point
an <- names(finalR)
x <- data.matrix(cdat)

ii <- data.frame(
	purrr::map(finalR, function(cl){clusterCrit::intCriteria(
		traj = x, part = cl,
		crit = c("C_index", "Calinski_Harabasz", "Davies_Bouldin", "Dunn",
						 "McClain_Rao", "PBM", "SD_Dis", "Ray_Turi", "Tau", "Gamma",
						 "G_plus", "Silhouette"))}) %>% 
		do.call(rbind.data.frame,.), 
	Compactness = apply(finalR, 2, compactness, data = x),
	Connectivity = apply(finalR, 2, function(cl) clValid::connectivity(Data = x, clusters = cl)))


k <- as.character(4)
zk <- ii
alg.all <- names(finalR)

# Separate algorithms into those from clusterCrit (main), and (others)
z.main <- zk %>% 
	magrittr::extract(!names(.) %in% c("Algorithms", "Compactness", "Connectivity") 
										& purrr::map_lgl(., ~ all(!is.nan(.x))))

z.other <- zk %>% 
	magrittr::extract(c("Compactness", "Connectivity"))

# Which algorithm is the best for each index?
bests <- purrr::map2_int(z.main, names(z.main), clusterCrit::bestCriterion)

max.bests <- z.main %>% 
	magrittr::extract(purrr::map_int(., which.max) == bests)%>% 
	cbind(z.other) %>% 
	magrittr::multiply_by(-1)

min.bests <- z.main %>% 
	magrittr::extract(purrr::map_int(., which.min) == bests)

rank.agg <- cbind(max.bests, min.bests,z.other) %>% 
	scale(center = FALSE, scale = TRUE) %>% 
	as.data.frame() %>% 
	purrr::map_df(~ alg.all[order(.x)]) %>% 
	# purrr::map_df(~ alg.all[rank(.x, ties.method = "random")]) %>% 
	t()

top.list <- rank.agg %>% 
	RankAggreg::RankAggreg(., ncol(.), method = "CE", verbose = FALSE, maxIter = 2000) %>% 
	magrittr::use_series("top.list")

ii <- ii[top.list,]
finalR <- finalR[,top.list]
print("Saving")
print(paste0(ddir,"/all_clusts.rds"))
saveRDS(finalR, paste0(ddir,"/all_clusts_",alldat[i],".rds"))
saveRDS(ii, paste0(ddir,"/ii_",alldat[i],".rds"))
}
