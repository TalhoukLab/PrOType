###############################################################
################ STEP 4: Predict All Nanostring ###############
###############################################################

suppressPackageStartupMessages({
  source("utils/PredictNanoString.R")
  require(tidyverse)
  require(splendid)
  require(caret)
})

set.seed(2017)



# load vancouver model
Van_mod <- readr::read_rds("./outputs/fits/ov.afc1_xpn_adaboost.rds")
genes <- Van_mod$names

# load nanostring data
nsdat <- load_nanostring("./data/nanostring/",genes)

# predict nanostring data
pred.nano <- predict(Van_mod,nsdat,type = "class") %>% 
  data_frame(ottaID=rownames(nsdat),preds=.)

# Prediction file circulated to check that prediction match
#preds <- read.csv("/Users/atalhouk/Repositories/NanoString/HGSCS/data/intermediate/predictions.csv", stringsAsFactors = TRUE)
#table(preds$Adaboost.xpn,pred.ns_Van$preds)

# write to file
readr::write_rds(pred.nano, "outputs/predictions/nstring_allbatches.rds")
