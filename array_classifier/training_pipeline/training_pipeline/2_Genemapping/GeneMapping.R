#' Mapping to Nanostring
#' 
#' Last updated on 27/10/2017 by Dustin Johnson
#'
#' Maps affy or agile data frame to nanostring data frame
#'
#' @param x is a data frame with feature as row and sample as column
#' @param type the type of probe, affy or agil, used in x
#' @param dataSet
#' @param inDir
#' @param outDir
#' @param housekeeping.normalize
#' @return a data frame with nanostring probe as feature and same number of sample as X

map_to_nano <- function(x, type, dataSet, inDir, outDir, housekeeping.normalize = TRUE)
{
  # import dependencies
  suppressPackageStartupMessages({
    require(tidyverse)
    #require(purrrlyr)
    require(reshape2)
  })
  
  # Perform mapping according to affy vs agilent
  if(type == "affy")
  {
    # Join mapping table with data table and take median of any repeat probe IDs
    df <- readr::read_csv(paste0(inDir, "HGNC-Affymetrix-NanoString-OTTA_Map.csv")) %>%
      filter(!is.na(`NanoString ProbeID`)) %>% select(-index) %>% 
      set_names(c("nanoID", "affy.probe", "symbol", "symbol.single", "nanostring.probeID", "housekeeping")) %>%
      mutate(nanoID = as.factor(nanoID)) %>%
      inner_join(data.frame(affy.probe = stringr::word(rownames(x), 1, sep = "\\|"), x), by = "affy.probe")
          
    dat.mapped <- filter(df, is.na(housekeeping)) %>%
      select(-c(nanoID, symbol.single, housekeeping))  

    dat <- mutate(df, housekeeping = ifelse(housekeeping == "yes" & !is.na(housekeeping), 1, 0)) %>%
      select(-c(nanoID, affy.probe, symbol, symbol.single)) %>%
      group_by(nanostring.probeID) %>%
      summarise_all(.funs = c("median")) %>%
      mutate(housekeeping = ifelse(housekeeping != 0 & housekeeping <= 1, 1, 0) %>% as.factor)

    # normalize wrt housekeeping genes if set to TRUE, otherwise just remove housekeeping
    if(housekeeping.normalize)
    {
      # extract housekeeping from table
      housekeeping <- filter(dat, housekeeping == 1) %>%
        select(-c(housekeeping, nanostring.probeID)) %>%
        colMeans() %>% data.frame(HCmean = .)
      
      # normalize wrt mean of housekeeping and return npcp
      dat.clean <- filter(dat, housekeeping == 0) %>%
        droplevels() %>%
        select(-c(housekeeping)) %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
      	purrr::map_if(is.numeric, function(x) x - housekeeping$HCmean) %>%
      	cbind.data.frame() %>%
        #purrrlyr::dmap_if(is.numeric, function(x) x - housekeeping$HCmean) %>%
        tibble::column_to_rownames(var = "rowname")
    } else {
      
      # drop housekeeping and return non-normalized npcp
      dat.clean <- filter(dat, housekeeping == 0) %>%
        droplevels() %>%
        select(-c(housekeeping)) %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
        tibble::column_to_rownames(var = "rowname") 
    }
  } else if(type == "agil") {

    # Join mapping table with data table and take median of any repeat probe IDs
    dat <- readr::read_csv(paste0(inDir, "HGNC-Agilent-NanoString-OTTA_Map.csv")) %>%
      filter(!is.na(`NanoString-Probe-ID`)) %>% select(-Index) %>% 
      set_names(c("agilID", "symbol", "nanostring.probeID")) %>%
      inner_join(data.frame(agilID = rownames(x), x), by = "agilID") 
    
    housekeeping <- c('ACTB', 'RPL19', 'PGK1', 'SDHA', 'POLR1B')
    dat.mapped <- filter(dat, !nanostring.probeID %in% housekeeping)
    
    dat <- select(dat, -c(agilID, symbol)) %>%
      group_by(nanostring.probeID) %>%
      summarize_all(.funs = c("median"))
    
    # normalize wrt housekeeping genes if set to TRUE, otherwise just remove housekeeping
    if(housekeeping.normalize)
    {
      # extract housekeeping (must be explicitly stated as agil mapping does not provide)
      housekeeping <- c('ACTB', 'RPL19', 'PGK1', 'SDHA', 'POLR1B')
      housekeeping.extract <- filter(dat, nanostring.probeID %in% housekeeping) %>%
        select(-nanostring.probeID) %>%
        colMeans() %>% data.frame(HCmean = .)
      
      # normalize wrt mean of housekeeping and return npcp
      dat.clean <- filter(dat, !(nanostring.probeID %in% housekeeping)) %>%
        droplevels() %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
      	purrr::map_if(is.numeric, function(x) x - housekeeping.extract$HCmean) %>%
      	cbind.data.frame() %>%
        #purrrlyr::dmap_if(is.numeric, function(x) x - housekeeping.extract$HCmean) %>%
        tibble::column_to_rownames(var = "rowname")
    } else {
      
      # drop housekeeping and return non-normalized npcp
      dat.clean <- filter(dat, !(nanostring.probeID %in% housekeeping)) %>%
        droplevels() %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
        tibble::column_to_rownames(var = "rowname")
    }
  } else {
    stop("Only affy and agil types are accepted.")
  }

  # write npcp to file
  if(housekeeping.normalize)
  {
    readr::write_rds(dat.clean, paste0(outDir, dataSet, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds"))
  } else {
    readr::write_rds(dat.clean, paste0(outDir, dataSet, "/data_pr_", dataSet, "/npcp_", dataSet, ".rds"))
  }

  # write mapping: tdat_mapped.rds
  readr::write_rds(dat.mapped, paste0(outDir, dataSet, "/data_pr_", dataSet, "/tdat_mapped_", dataSet, ".rds"))

  print("Mapping is complete.")
}
