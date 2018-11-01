#' Mapping to Nanostring
#'
#' Maps affy data frame to nanostring data frame and normalize housekeeping
#' genes
#'
#' @param dataSet name of dataset
#' @param inputDir input directory
#' @param outputDir output directory
#' @param shouldCompute should the function run regardless of whether output
#'   already exists?
#' @return a data frame with nanostring probe as feature and same number of
#'   sample as X
#' @author Last updated on 27/10/2017 by Dustin Johnson. Edited by Derek Chiu.
source(here::here("pipeline/1-unsupervised/utils.R"))
map_to_nano <- function(dataSet, inputDir, outputDir, shouldCompute) {
  tdat <- readRDS(file.path(inputDir, paste0("tdat_", dataSet, ".rds")))
  x <- data.frame(t(tdat))

  tdat_mapped_outfile <- file.path(outputDir,
                                   paste0("tdat_mapped_", dataSet, ".rds"))
  npcp_out <-  file.path(outputDir,
                         paste0("npcp-hcNorm_", dataSet, ".rds"))

  if (file.exists(tdat_mapped_outfile) && file.exists(npcp_out) && !shouldCompute) {
    cli::cat_line("File already exists, skipping.")
    q("no", 0, FALSE)
  }

  # Perform mapping according to affy
  # Join mapping table with data table and take median of any repeat probe IDs
  df <- readr::read_csv(here::here("data", "hgnc", "HGNC-Affymetrix-NanoString-OTTA_Map.csv"), col_types = readr::cols()) %>%
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

  # write npcp and mapped data to file
  saveRDS(dat.clean, npcp_out)
  saveRDS(dat.mapped, tdat_mapped_outfile)
}
