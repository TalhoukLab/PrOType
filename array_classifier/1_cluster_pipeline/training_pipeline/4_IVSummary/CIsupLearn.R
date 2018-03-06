library(tidyverse)
library(magrittr)

#Functions----
matrix.sort <- function(matrix) {
  
  if (nrow(matrix) != ncol(matrix)) stop("Not diagonal")
  if(is.null(rownames(matrix))) rownames(matrix) <- 1:nrow(matrix)
  
  row.max <- apply(matrix,1,which.max)
  if(all(table(row.max) != 1)) stop("Ties cannot be resolved")
  
  matrix[names(sort(row.max)),]
}

# Create CIs
create_ci <- function(df){
  res<- paste(sprintf("%.2f",df[,"50%"]),paste0("(",sprintf("%.2f",df[,"5%"]),"-",sprintf("%.2f",df[,"95%"]),")")) %>% 
    data.frame(.) %>% 
    magrittr::set_rownames(rownames(df))
  return(res)
}

# Sort best
sort_best <- function(train_eval, top=5){
 te <-  purrr::map(train_eval,function(x) {
  x %>% 
    as.data.frame()%>%
    dplyr::mutate(logloss = -logloss) %>% 
    t() %>%    
    magrittr::extract(,1)}) %>% 
  data.frame(.) 
indices <- c("auc","accuracy","f1.1","f1.2","f1.3","f1.4")
algsfull <-colnames(te)

df <- te %>% 
  apply (.,1, function(x){algsfull[order(rank(-x, ties.method = "random"))]}) %>% 
  t() %>% 
  RankAggreg::RankAggreg(., ncol(.), method = "GA", 
                         verbose = FALSE, maxIter = 2000)
best <- te[,df$top.list]
res <- best[indices,1:top]%>% data.frame(.) %>% set_colnames(colnames(best)[1:top]) 
return(list(best=best,res=res))
}

# Relabel classes
relabel_classes <- function(df,FinalR_lab){
  a <- table(FinalR_lab[,1], FinalR_lab[,"CL"])
  equi <- data.frame(class=rownames(matrix.sort(a)),label=colnames(matrix.sort(a)))
  equi <- equi[order(equi$class),]
  rownames(df)[grep(".\\d",rownames(df))] <- paste("F1",paste0(equi$class,"-",equi$label),sep=".") 
  return(df)}

# Inputs ----
inDir <- inDir
outDir <- outDir
fdat <- fdat
top <- top


# Read in  ----
# data
final <- map(fdat, function(x) {
  print(x)
  FinalR_lab <- readRDS(paste0(inDir, x, "/data_pr_", x, "/all_clusts_", x, ".rds"))
  train_eval <- readRDS(paste0(inDir, x, "/data_pr_", x, "/train_eval_", x, ".rds")) 
  bests <- sort_best(train_eval , top=top)
  ci <- map(train_eval,function(x) {
      x %>% 
      data.frame(.) %>% 
      t(.) %>% 
      create_ci
    }) %>%
    data.frame(.) %>% 
    set_names(names(train_eval))
    ci_3 <- ci[,colnames(bests$res)]
  return(ci_3)
  }) %>% set_names(fdat) 


purrr::map2(final,names(final), function(x,y){write.csv(x,paste0(outDir, y, "/data_pr_",  y, "/sup_lrn_",y,".csv"))})

