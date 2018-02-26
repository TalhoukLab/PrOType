# This file is called in the beginning and not processed as part of the Q
# input ndat and datadir pr
# output a saved cdat

library(diceR)
library(tidyverse)

# load data
#fname <- paste0(dpath, ndat, ".rds")
#tdat <- readRDS(fname) %>% t(.)

# load data
fname <- paste0(dpath, ndat, ".RData")
tdat <- load(fname) %>% get(.) %>% t(.) 


#Center and scale
cdat <- switch (pr,
	cs = prepare_data(tdat, scale = TRUE, type = "conventional"),
	rs = prepare_data(tdat, scale = TRUE, type = "robust"),
	ns = prepare_data(tdat, scale = FALSE)
)

saveRDS(tdat,paste0(datadir,"/tdat_",ndat,".rds"))
saveRDS(cdat,paste0(datadir,"/cdat_",ndat,".rds"))


