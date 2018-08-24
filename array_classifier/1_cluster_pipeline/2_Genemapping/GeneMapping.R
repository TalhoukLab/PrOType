#' Mapping to Nanostring
#'
#' Maps affy data frame to nanostring data frame and normalize housekeeping
#' genes
#'
#' @param x is a data frame with feature as row and sample as column
#' @param dataSet dataset
#' @param inDir input directory
#' @param outDir output directory
#' @return a data frame with nanostring probe as feature and same number of
#'   sample as X
#' @author Last updated on 27/10/2017 by Dustin Johnson. Edited by Derek Chiu.
library(magrittr)

map_to_nano <- function(x, dataSet, inDir, outDir) {
    # Perform mapping according to affy
    # Join mapping table with data table and take median of any repeat probe IDs
    df <- readr::read_csv(paste0(inDir, "HGNC-Affymetrix-NanoString-OTTA_Map.csv")) %>%
      dplyr::filter(!is.na(`NanoString ProbeID`)) %>%
      dplyr::select(-index) %>%
      magrittr::set_names(c("nanoID", "affy.probe", "symbol", "symbol.single", "nanostring.probeID", "housekeeping")) %>%
      dplyr::mutate(nanoID = as.factor(nanoID)) %>%
      dplyr::inner_join(data.frame(affy.probe = stringr::word(rownames(x), 1, sep = "\\|"), x, stringsAsFactors = FALSE),
                        by = "affy.probe")

    dat.mapped <- df %>%
      dplyr::filter(is.na(housekeeping)) %>%
      dplyr::select(-c(nanoID, symbol.single, housekeeping))

    dat <- df %>%
      dplyr::mutate(housekeeping = ifelse(housekeeping == "yes" & !is.na(housekeeping), 1, 0)) %>%
      dplyr::select(-c(nanoID, affy.probe, symbol, symbol.single)) %>%
      dplyr::group_by(nanostring.probeID) %>%
      dplyr::summarise_all("median") %>%
      dplyr::mutate(housekeeping = as.factor(ifelse(housekeeping != 0 & housekeeping <= 1, 1, 0)))

    # extract housekeeping from table
    housekeeping <- dat %>%
      dplyr::filter(housekeeping == 1) %>%
      dplyr::select(-c(housekeeping, nanostring.probeID)) %>%
      colMeans(na.rm = TRUE) %>%
      data.frame(HCmean = .)

    print(dim(housekeeping))

    # normalize wrt mean of housekeeping and return npcp
    dat.clean <- dat %>%
      dplyr::filter(housekeeping == 0) %>%
      droplevels() %>%
      dplyr::select(-housekeeping) %>%
      reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
      reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
      purrr::map_if(is.numeric, ~ . - housekeeping$HCmean) %>%
      cbind.data.frame() %>%
      tibble::column_to_rownames(var = "rowname")

    # write npcp to file
    readr::write_rds(dat.clean, paste0(outDir, dataSet, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds"))

    # write mapping to file
    readr::write_rds(dat.mapped, paste0(outDir, dataSet, "/data_pr_", dataSet, "/tdat_mapped_", dataSet, ".rds"))

    print("Mapping is complete.")
}
