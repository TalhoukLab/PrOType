#' Mapping to Nanostring
#'
#' Maps affy or agile data frame to nanostring data frame
#'
#' @param x is a data frame with feature as row and sample as column
#' @param type the type of probe, affy or agil, used in x
#' @param dataSet dataset
#' @param inDir input directory
#' @param outDir output directory
#' @param housekeeping.normalize logical; whether to normalize housekeeping
#'   genes
#' @return a data frame with nanostring probe as feature and same number of
#'   sample as X
#' @author Last updated on 27/10/2017 by Dustin Johnson. Edited by Derek Chiu.
map_to_nano <- function(x, type, dataSet, inDir, outDir,
                        housekeeping.normalize = TRUE) {
  # Perform mapping according to affy vs agilent
  if (type == "affy") {
    # Join mapping table with data table and take median of any repeat probe IDs
    df <- readr::read_csv(paste0(inDir, "HGNC-Affymetrix-NanoString-OTTA_Map.csv")) %>%
      dplyr::filter(!is.na(`NanoString ProbeID`)) %>%
      dplyr::select(-index) %>%
      magrittr::set_names(c("nanoID", "affy.probe", "symbol", "symbol.single", "nanostring.probeID", "housekeeping")) %>%
      dplyr::mutate(nanoID = as.factor(nanoID)) %>%
      dplyr::inner_join(data.frame(affy.probe = stringr::word(rownames(x), 1, sep = "\\|"), x), by = "affy.probe")

    dat.mapped <- df %>%
      dplyr::filter(is.na(housekeeping)) %>%
      dplyr::select(-c(nanoID, symbol.single, housekeeping))

    dat <- df %>%
      dplyr::mutate(housekeeping = ifelse(housekeeping == "yes" & !is.na(housekeeping), 1, 0)) %>%
      dplyr::select(-c(nanoID, affy.probe, symbol, symbol.single)) %>%
      dplyr::group_by(nanostring.probeID) %>%
      dplyr::summarise_all("median") %>%
      dplyr::mutate(housekeeping = as.factor(ifelse(housekeeping != 0 & housekeeping <= 1, 1, 0)))

    # normalize wrt housekeeping genes if set to TRUE, otherwise just remove housekeeping
    if (housekeeping.normalize) {
      # extract housekeeping from table
      housekeeping <- dat %>%
        dplyr::filter(housekeeping == 1) %>%
        dplyr::select(-c(housekeeping, nanostring.probeID)) %>%
        colMeans() %>%
        data.frame(HCmean = .)

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
    } else {
      # drop housekeeping and return non-normalized npcp
      dat.clean <- dat %>%
        dplyr::filter(housekeeping == 0) %>%
        droplevels() %>%
        dplyr::select(-housekeeping) %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
        tibble::column_to_rownames(var = "rowname")
    }
  } else if (type == "agil") {
    # Join mapping table with data table and take median of any repeat probe IDs
    dat <- readr::read_csv(paste0(inDir, "HGNC-Agilent-NanoString-OTTA_Map.csv")) %>%
      dplyr::filter(!is.na(`NanoString-Probe-ID`)) %>%
      dplyr::select(-Index) %>%
      magrittr::set_names(c("agilID", "symbol", "nanostring.probeID")) %>%
      dplyr::inner_join(data.frame(agilID = rownames(x), x), by = "agilID")

    housekeeping <- c("ACTB", "RPL19", "PGK1", "SDHA", "POLR1B")
    dat.mapped <- dplyr::filter(dat, !nanostring.probeID %in% housekeeping)

    dat <- dat %>%
      dplyr::select(-c(agilID, symbol)) %>%
      dplyr::group_by(nanostring.probeID) %>%
      dplyr::summarize_all("median")

    # normalize wrt housekeeping genes if set to TRUE, otherwise just remove housekeeping
    if (housekeeping.normalize) {
      # extract housekeeping (must be explicitly stated as agil mapping does not provide)
      housekeeping <- c("ACTB", "RPL19", "PGK1", "SDHA", "POLR1B")
      housekeeping.extract <- dat %>%
        dplyr::filter(nanostring.probeID %in% housekeeping) %>%
        dplyr::select(-nanostring.probeID) %>%
        colMeans() %>%
        data.frame(HCmean = .)

      # normalize wrt mean of housekeeping and return npcp
      dat.clean <- dat %>%
        dplyr::filter(!(nanostring.probeID %in% housekeeping)) %>%
        droplevels() %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
      	purrr::map_if(is.numeric, ~ . - housekeeping.extract$HCmean) %>%
      	cbind.data.frame() %>%
        tibble::column_to_rownames(var = "rowname")
    } else {
      # drop housekeeping and return non-normalized npcp
      dat.clean <- dat %>%
        dplyr::filter(!(nanostring.probeID %in% housekeeping)) %>%
        droplevels() %>%
        reshape2::melt(id.vars = "nanostring.probeID", variable.name = "rowname", value.name = "val") %>%
        reshape2::dcast(rowname ~ nanostring.probeID, value.var = "val") %>%
        tibble::column_to_rownames(var = "rowname")
    }
  } else {
    stop("Only affy and agil types are accepted.")
  }

  # write npcp to file
  if (housekeeping.normalize) {
    readr::write_rds(dat.clean, paste0(outDir, dataSet, "/data_pr_", dataSet, "/npcp-hcNorm_", dataSet, ".rds"))
  } else {
    readr::write_rds(dat.clean, paste0(outDir, dataSet, "/data_pr_", dataSet, "/npcp_", dataSet, ".rds"))
  }

  # write mapping: tdat_mapped.rds
  readr::write_rds(dat.mapped, paste0(outDir, dataSet, "/data_pr_", dataSet, "/tdat_mapped_", dataSet, ".rds"))

  print("Mapping is complete.")
}
