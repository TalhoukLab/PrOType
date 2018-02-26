#Packages----
library(tidyverse)

#Inputs-----
fdir <- fdir
ndat <- ndat
mname <- mname


for(i in seq_along(ndat))
{
  print(ndat[i])
  f <- grep(paste0("_train_eval_", ndat[i], ".rds"), list.files(paste0(fdir, ndat[i], "/", mname, "_", ndat[i], "/")), value = TRUE)
  print(f)
  algs <- data.frame(f=f) %>%
	separate(f,c("algs"),sep="_", extra="drop")
  algs$algs[algs$algs=="second"] <- "svm"
  f_Rfe <- grep("Rfe",f,value = TRUE)
  f_rest <- f[!f%in%f_Rfe]

  m <- purrr::map(paste0(fdir,ndat[i],"/", mname, "_", ndat[i],  "/",f_rest), readRDS) %>%
	unlist(.,recursive=FALSE)

  #mRFE <- purrr::map(paste0(fdir,ndat[i],"/", mname, "_", ndat[i],  "/",f_Rfe), readRDS) %>%
  #	unlist(.,recursive=FALSE) %>% set_names(paste0(names(.),"Rfe"))

  #res <- c(m, mRFE)
  res <- m

  saveRDS(res,paste0(fdir, ndat[i], "/data_pr_", ndat[i], "/train_eval_", ndat[i],  ".rds"))
}
