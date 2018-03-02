# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
})


# Functions ----

# build mapping
build_mapping <- function(train.set)
{
  # label mapping
  labs <- c(1, 2, 3, 4)
  if(train.set == "ov.afc1_cbt")
  {
    map <- data.frame(labs, labels = c("C1-MES",	"C5-PRO",	"C4-DIF",	"C2-IMM"))
  } else if(train.set == "ov.afc1_xpn")
  {
    map <- data.frame(labs, labels = c("C2-IMM",	"C4-DIF", "C5-PRO",	"C1-MES"))
  } else {
    print("No valid training set specified")
  }
  
  return(map)
}