runTrainingSequence <- function(runBoot, processBoot, finalTraining, makePreds,
                                output_dir, studies, train_dat, train_lab, B,
                                algs) {
  cli::cat_rule("Running Training Sequence")
  for (study in studies) {
    cli::cat_line("Train ", study, " study")
    # Generate a subset for training
    x <- sl_data(train_dat, study, "training")
    y <- sl_class(train_lab, x)

    # Bootstrap several algos
    if (runBoot) {
      runBootstrap(output_dir, study, x, y, B, algs)
    }

    # Generate Prop of Gene Frequency
    if (processBoot) {
      runProcessBoot(output_dir, study, train_dat, B, algs)
    }

    # Train models with different number of genes
    if (finalTraining) {
      runFinalTraining(output_dir, study, x, y, algs)
    }

    # Predict left out study
    if (makePreds) {
      makePredictions(output_dir, study, train_dat, train_lab, algs)
    }
  }
}

runProcessBoot <- function(output_dir, study, train_dat, B,
                           algs = c("lasso", "rf", "ada")) {
  cli::cat_line("Processing Boot")
  fnames <- list.files(
    path = file.path(output_dir, "GeneSelection/output/training"),
    pattern = paste0(study, ".*(", paste(algs, collapse = "|"), ")"),
    full.names = TRUE
  )
  genes <- get_genes(train_dat)
  geneFreq <- purrr::map(fnames, ~ {
    m <- readr::read_rds(.)
    gene_freq(m$models, genes, B)
  })
  sumFreq <- data.frame(genes, geneFreq) %>%
    dplyr::arrange(dplyr::desc(rfFreq))
  sumFreq_dir <- mkdir(file.path(output_dir, "GeneSelection/output/sumFreq"))
  readr::write_csv(sumFreq, file.path(sumFreq_dir, paste0(study, "_sumFreq.csv")))
}

runFinalTraining <- function(output_dir, study, x, y,
                             algs = c("lasso", "rf", "ada"), seed_alg = 2018) {
  cli::cat_line("Starting Final Training")
  sumFreq <- readr::read_csv(file.path(output_dir, "GeneSelection/output/sumFreq",
                                       paste0(study, "_sumFreq.csv")),
                             col_types = readr::cols())

  final_dir <- mkdir(file.path(output_dir, "GeneSelection/output/finalTraining"))
  args <- tibble::lst(x, y, sumFreq, final_dir, study, seed_alg)
  purrr::walk(algs, ~ purrr::invoke(classify_top_genes, args, alg = .))
}

classify_top_genes <- function(x, y, sumFreq, outputDir, study, seed_alg, alg,
                               ng = seq(4, 100, 5), shouldCompute=TRUE) {
  output_file <- file.path(outputDir, "GeneSelection/output/finalTraining", paste0(study, "_final_fit_", alg, ".rds"))
  if (file.exists(output_file) && !shouldCompute) {
    cli::cat_line("Output already exists.")
  } else {
    ng %>%
      purrr::set_names(paste0("ng_", .)) %>%
      purrr::map(~ {
        top_genes <- sumFreq %>%
          dplyr::arrange(dplyr::desc(!!dplyr::sym(paste0(alg, "Freq")))) %>%
          dplyr::pull(genes) %>%
          head(.x) %>%
          make.names()
        splendid::classification(x[, top_genes], y, match_alg(alg), seed_alg = seed_alg)
      }) %>%
      readr::write_rds(output_file)
  }
}

makePredictions <- function(output_dir, study, train_dat, train_lab,
                            algs = c("lasso", "rf", "ada")) {
  cli::cat_line("Making Predictions")
  preds_dir <- mkdir(file.path(output_dir, "GeneSelection/output/studyPreds"))
  fnames <- list.files(
    path = file.path(output_dir, "GeneSelection/output/finalTraining"),
    pattern = paste0(study, ".*(", paste(algs, collapse = "|"), ")"),
    full.names = TRUE
  )
  # Generate a subset for testing
  x_new <- sl_data(train_dat, study, "test")
  y_new <- sl_class(train_lab, x_new)
  # Model predictions on test set for each algorithm
  mod.pred <- algs %>%
    purrr::set_names() %>%
    purrr::map(function(a) {
      sp_alg <- match_alg(a)
      a %>%
        grep(fnames, value = TRUE) %>%
        readr::read_rds() %>%
        purrr::set_names(paste(sp_alg, names(.), sep = "_")) %>%
        purrr::map(~ splendid::prediction(., x_new[, which_genes(., sp_alg)], y_new) %>%
                      purrr::set_names(rownames(x_new)))
    })
  readr::write_rds(mod.pred, file.path(preds_dir, paste0(study, "_mod_pred.rds")))
}

#' Supervised learning data
#'
#' Create supervised learning training/test data
#'
#' @param train_dat input data
#' @param study study site. If `NULL` (default), no filtering is performed and
#'   all cases from `train_dat` are returned.
#' @param type the type of SL data to produce. If type is "training" (default),
#'   the site matching `study` is filtered out, and the rest of the observations
#'   are used as training data. If type is "test", the site matching `study` is
#'   kept and used as testing data.
#' @return supervised learning data with cases potentially filtered out, the
#'   variables "site" and "cut" removed, and unique identifier "OTTA.ID" is
#'   stored in the rownames
sl_data <- function(train_dat, study = NULL, type = c("training", "test")) {
  type <- match.arg(type)
  filter_fun <- switch(type, training = `!=`, test = `==`)
  if (is.null(study)) filter_fun <- function(x, y) TRUE

  train_dat %>%
    dplyr::filter(filter_fun(site, study)) %>%
    dplyr::select(-c(site, cut)) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("OTTA.ID")
}

#' Supervised learning class
#'
#' @param train_lab data with supervised learning labels
#' @param data expression data
#' @return A vector of classes for Adaboost after filtering `train_lab` for
#'   cases found in `data`
sl_class <- function(train_lab, data) {
  train_lab %>%
    dplyr::filter(ottaID %in% rownames(data)) %>%
    dplyr::pull(Adaboost.xpn)
}
