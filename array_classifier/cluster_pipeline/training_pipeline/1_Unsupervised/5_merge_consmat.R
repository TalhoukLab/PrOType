#need dir algs s
library(dplyr)
library(magrittr)
newdir <- paste0(dir,"/con_mat_",ndat,"/")

multMergeCM <- function(algs, fnames, newdir){
	#Seperate the algorithms
	algF <- switch (algs,
	nmfbrunet = unique(grep(c("nmfbrunet"), fnames, value=TRUE)),
	nmflee = unique(grep(c("nmflee"), fnames, value=TRUE)),
	distalgs = unique(grep(c("dist"), fnames, value=TRUE)),
	rest = unique(grep(c("rest"), fnames, value=TRUE))
	)
	
MetaConsMat <- switch(algs, 
	nmfbrunet = paste0(newdir,algF) %>% #Read the files 
		purrr::map(readRDS) %>% #Sum the entries of the list
		Reduce("+",.) %>% 
		list(.) %>% 
		set_names("NMF_Brunet"),
	nmflee = paste0(newdir,algF) %>% #Read the files
		purrr::map(readRDS) %>% #Sum the entries of the list
		Reduce('+',.) %>%
		list(.) %>% 
		set_names("NMF_Lee"),
	distalgs = paste0(newdir,algF) %>% #Read the files 
		purrr::map(readRDS) %>% #Sum the entries of the list
		purrr::transpose(.) %>% 
		purrr::map(function(x) Reduce("+",x)),
	rest = paste0(newdir,algF) %>% #Read the files 
		purrr::map(readRDS) %>% #Sum the entries of the list
		purrr::transpose(.) %>% 
		purrr::map(function(x) Reduce("+",x))
)
}



if (merge=="partial"){
#list all the files in the path
fnames <- gtools::mixedsort(list.files(path = paste0(dir,"/rds_out_",ndat)))
#part <- (r*c-(c-1)):reps
part <- (r*c-(c-1)):(r*c)
algF <- switch (algs,
	nmfbrunet = unique(grep(c("nmfbrunet"), fnames, value=TRUE)),
	nmflee = unique(grep(c("nmflee"), fnames, value=TRUE)),
	distalgs =unique(grep(c("dist"), fnames, value=TRUE)),
	rest = unique(grep(c("rest"), fnames, value=TRUE))
)


#Get the seeds
temp <- regmatches(algF, gregexpr("[[:digit:]]+", algF))
Seeds <- as.numeric(purrr::map_chr(temp,`[`,1))
#x <- matrix(1:10, ncol = 5)
#saveRDS(x,paste0(dir,"/haha.rds"))
print("haha")
consmat <- switch(algs,
	nmfbrunet = lapply(paste0(dir,"/con_mat_",ndat,"/CM_",
		algF[part]),readRDS) %>%
		lapply("[[", as.character(k)) %>%
		lapply("[[", "NMF_Brunet")%>%
		lapply("[[", "consensus_matrix")%>%
		Reduce('+',.),
	nmflee = lapply(paste0(dir,"/con_mat_",ndat,"/CM_",
		algF[part]),readRDS) %>%
		lapply("[[", as.character(k)) %>%
		lapply("[[", "NMF_Lee")%>%
		lapply("[[", "consensus_matrix")%>%
		Reduce('+',.),
	rest = lapply(paste0(dir,"/con_mat_",ndat,"/CM_",
		algF[part]),readRDS)%>%
		set_names(Seeds[part]) %>%
		purrr::at_depth(.,3,"consensus_matrix") %>%
		lapply("[[", as.character(k)) %>%
		purrr::transpose(.) %>%
		lapply(., function(x) Reduce('+',x)),
	distalgs = lapply(paste0(dir,"/con_mat_",ndat,"/CM_",
		algF[part]),readRDS) %>%
		set_names(Seeds[part]) %>%
		purrr::at_depth(.,3,"consensus_matrix") %>%
		lapply("[[", as.character(k)) %>%
		purrr::transpose(.) %>%
		lapply(., function(x) Reduce('+',x))
)

print("haha")
ifile <- paste0(dir,"/con_mat_",ndat,"/",r,"_",algs,"_consmat_",ndat,".rds")
saveRDS(consmat,ifile)
} else {
ifile <- paste0(dir,paste0("/data_pr_",ndat,"/Final_CM","_",ndat,".rds"))
	fnames <- gtools::mixedsort(list.files(path = paste0(dir,"/con_mat_",ndat))) %>% 	grep(pattern="^[[:digit:]]", x=., value=TRUE)
consmatF <- lapply(algs, multMergeCM, fnames = fnames, newdir=newdir) %>%
	unlist(.,recursive=FALSE)
saveRDS(consmatF,ifile)
}
