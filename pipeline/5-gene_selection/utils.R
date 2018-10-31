# Gene Selection utility functions ----------------------------------------

# Load packages and project wide utility functions
source(here::here("assets/utils.R"))


# General Functions -------------------------------------------------------

define_batch <- function(preds_new, nsdat, batch = "b1") {
  cli::cat_line("Selecting batch ", batch, " data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(is.na(published), agree == 1, Batch == batch) %>%
    dplyr::select(ottaID, Adaboost.xpn, published) %>%
    dplyr::mutate_at("Adaboost.xpn", dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

define_overlap <- function(preds_new, nsdat) {
  cli::cat_line("Selecting overlap data and labels where there is consensus")
  lab <- preds_new %>%
    dplyr::mutate(agree = ifelse(Adaboost.xpn == TCGA.Predicted.Subtype, 1, 0)) %>%
    dplyr::filter(!is.na(published), agree == 1) %>%
    dplyr::mutate_at(c("Adaboost.xpn", "published"),
                     dplyr::funs(factor(gsub("-", "\\.", .))))
  dat <- filter_nsdat(nsdat, lab)
  check_data_order(lab, dat)
}

filter_nsdat <- function(nsdat, lab) {
  nsdat %>%
    dplyr::filter(`OTTA.ID` %in% lab$ottaID) %>%
    `colnames<-`(make.names(colnames(.)))
}

check_data_order <- function(lab, dat) {
  if (!all(lab$ottaID == dat$OTTA.ID)) {
    stop("Data is in the wrong order.")
  } else {
    tibble::lst(lab, dat)
  }
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

#******************************************************************
#Function to determine which genes were used in training
# Inputs: model fit and algorithm
# output: gene list
#*****************************************************************
which_genes <- function(fits, algo) {
  genes <- switch(
    algo,
    mlr_lasso = fits$glmnet.fit$beta[[1]]@Dimnames[[1]],
    rf = if (inherits(fits, "randomForest")) {
      rownames(fits$importance)
    } else if (inherits(fits, "train")) {
      rownames(fits$finalModel$importance)
    },
    adaboost = fits$names
  )
  make.names(genes)
}

loso_plot <- function(file_name, data, group, main,
                      xlab = "#genes", ylab = "F1 Score",
                      xbreaks = seq(4, 100, 5), ylim = c(0, 1),
                      col_alg = c("red", "blue", "green"), col_max = "black",
                      show_max = TRUE, legend_border = TRUE) {
  pdf(file_name)
  plot(
    1,
    type = "n",
    xlim = range(xbreaks),
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    main = main
  )
  col_alg <- tail(col_alg, length(data))
  purrr::walk2(data, col_alg, ~ {
    points(xbreaks, .x[[group]], pch = 18, col = .y)
    max_ind <- which.max(.x[[group]])
    if (show_max) {
      points(xbreaks[max_ind], .x[[group]][[max_ind]], pch = 19, col = col_max)
    }
  })
  legend("bottomright",
         pch = 18,
         purrr::imap_chr(data, ~ {
           max_ind <- which.max(.x[[group]])
           paste0(.y, " ", round(.x[[group]][[max_ind]], 2),
                  " #", xbreaks[max_ind])
         }),
         col = col_alg,
         bty = ifelse(legend_border, "o", "n"))
  dev.off()
}


# 0 - Setup ---------------------------------------------------------------


#******************************************************************
# Load Nanostring data - all batches
# Inputs:
# the directory of the Nanostring data
#*****************************************************************
load_nanostring <- function(cut = "all") {
  clinical_vars <- 3:37
  nstring_dir <- "data/nstring"
  # import cut 1 nanostring
  c1 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch1_20170217_updated.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut1")

  # import batch 2 nanostring
  c2 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch2_20170221.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut2")

  # import batch 3 nanostring
  c3 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch3_20170307_updated_NCO.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars) %>%
    dplyr::mutate(cut = "cut3")

  # import batch 4 nanostring
  c4 <- readr::read_csv(
    file = here::here(nstring_dir, "nanostring_classifier_data_batch4_20170512.csv"),
    col_types = readr::cols()
  ) %>%
    dplyr::select(-clinical_vars, -ARL_block) %>%
    dplyr::mutate(cut = "cut4")

  # combine into list and row bind
  tibble::lst(c1, c2, c3, c4) %>%
    dplyr::bind_rows() %>%
    `names<-`(make.names(names(.)))
}

# Load published subtype data (and merged with prediction labels)
load_prediction_labels <- function(nsdat) {
  cli::cat_line("Loading Prediction Labels")
  preds <- read.csv(here::here("data/nstring/predictions.csv"),
                    stringsAsFactors = FALSE)
  if (!all(nsdat$`OTTA.ID` %in% preds$ottaID)) {
    stop("Missing OTTA ID cases")
  }
  published <- read.csv(here::here("data/nstring/Subtype_Original.csv"),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(ottaID, published) %>%
    dplyr::mutate_at("published", make.names)
  preds_new <- dplyr::left_join(preds, published, by = "ottaID") %>%
    dplyr::mutate_at(c("Adaboost.xpn", "Array.Pred.Subtype",
                       "TCGA.Predicted.Subtype"), make.names)
  tibble::lst(published, preds_new)
}


# 1 - Bootstrap Frequencies -----------------------------------------------

get_genes <- function(data) {
  purrr::discard(colnames(data), ~ . %in% c("OTTA.ID", "site", "cut"))
}

match_alg <- function(alg) {
  switch(alg, lasso = "mlr_lasso", rf = "rf", ada = "adaboost")
}

# Gene Frequency
# Number of times a gene is chosen in the top 100 out of B bootstraps
gene_freq <- function(fit, alg, genes, B, ntop = 100) {
  alg_name <- match_alg(alg)
  boot_tops <- switch(
    alg_name,
    adaboost = {
      fit[[alg_name]] %>%
        purrr::map(
          maboost::varplot.maboost,
          plot.it = FALSE,
          type = "scores",
          max.var.show = ntop
        ) %>%
        purrr::map(names)
    },
    mlr_lasso = {
      fit[[alg_name]] %>%
        purrr::map(~ {
          purrr::map2(.[["glmnet.fit"]][["beta"]],
                      which(.[["glmnet.fit"]][["lambda"]] %in% .[["lambda.1se"]]),
                      ~ names(which(.x[, .y] != 0)))
        }) %>%
        purrr::map(purrr::reduce, union)
    },
    rf = {
      fit[[alg_name]] %>%
        purrr::map(~ {
          if (inherits(., "randomForest")) {
            importance <- .[["importance"]]
          } else if (inherits(., "train")) {
            importance <- .[["finalModel"]][["importance"]]
          }
          importance %>%
            magrittr::extract(order(., decreasing = TRUE), ) %>%
            head(ntop) %>%
            names()
        })
    }
  )
  boot_tops %>%
    purrr::map_dfc(~ ifelse(genes %in% ., 1 / B, 0)) %>%
    dplyr::transmute(!!dplyr::sym(paste0(alg, "Freq")) := purrr::pmap_dbl(., sum)) %>%
    data.frame(genes, .)
}

# Bootstrap frequencies for each class in lasso
lasso_freq <- function(fit, genes) {
  by_class_tops <- fit %>%
    purrr::pluck("models", "mlr_lasso") %>%
    purrr::map(~ {
      purrr::map2(.[["glmnet.fit"]][["beta"]],
                  which(.[["glmnet.fit"]][["lambda"]] %in% .[["lambda.1se"]]),
                  ~ names(which(.x[, .y] != 0)))
    })
  by_class <- by_class_tops %>%
    purrr::map(function(x)
      purrr::map_dfc(x, function(y)
        ifelse(genes %in% y, 1 / length(.), 0))) %>%
    purrr::reduce(`+`) %>%
    data.frame(genes, .)
}




# 3 - Train on Top Genes --------------------------------------------------

classify_top_genes <- function(x, y, sum_freq, outputDir, study, seed_alg, alg,
                               ng = seq(4, 100, 5), shouldCompute = TRUE) {
  output_file <- file.path(outputDir, "gene_selection", "train", paste0(study, "_final_fit_", alg, ".rds"))
  if (file.exists(output_file) && !shouldCompute) {
    cli::cat_line("Output already exists.")
  } else {
    ng %>%
      purrr::set_names(paste0("ng_", .)) %>%
      purrr::map(~ {
        top_genes <- sum_freq %>%
          dplyr::arrange(dplyr::desc(!!dplyr::sym(paste0(alg, "Freq")))) %>%
          dplyr::pull(genes) %>%
          head(.x) %>%
          make.names()
        splendid::classification(x[, top_genes], y, match_alg(alg), seed_alg = seed_alg)
      }) %>%
      saveRDS(output_file)
  }
}


# 4 - Predictions ---------------------------------------------------------

predict_top_genes <- function(output_dir, study, train_dat, train_lab,
                            algs = c("lasso", "rf", "ada")) {
  cli::cat_line("Making Predictions")
  preds_dir <- file.path(output_dir, "gene_selection", "predict")
  fnames <- list.files(
    path = file.path(output_dir, "gene_selection", "train"),
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
        readRDS() %>%
        purrr::set_names(paste(sp_alg, names(.), sep = "_")) %>%
        purrr::map(~ splendid::prediction(., x_new[, which_genes(., sp_alg)], y_new) %>%
                     purrr::set_names(rownames(x_new)))
    })
  saveRDS(mod.pred, file.path(preds_dir, paste0(study, "_mod_pred.rds")))
}


# 5 - Evaluate Results ----------------------------------------------------

evaluate_predictions <- function(output_dir, train_dat, train_lab, algs,
                                 producePlots = TRUE) {
  cli::cat_rule("Evaluating Predictions")
  pred_filenames <-
    list.files(path = file.path(output_dir, "gene_selection", "predict"),
               full.names = TRUE)

  cli::cat_line("Getting training data names")
  site_names <- table(train_dat$site) %>% paste0(names(.), .)
  l <- pred_filenames %>%
    purrr::set_names(site_names) %>%
    purrr::map(readRDS) %>%
    purrr::modify_depth(2, data.frame) %>%
    purrr::transpose() %>%
    purrr::map_at("lasso", purrr::modify_depth, 2,
                  ~ forcats::fct_expand(., levels(attr(., "class.thres"))))

  l_train <- l %>%
    purrr::map(Reduce, f = rbind) %>%
    purrr::map(tibble::rownames_to_column, var = "ottaID") %>%
    purrr::map(dplyr::inner_join, x = train_lab, by = "ottaID")

  res <- l[algs] %>%
    purrr::modify_depth(2, ~ {
      tbs <- tibble::rownames_to_column(., var = "ottaID") %>%
        dplyr::inner_join(train_lab, by = "ottaID")
      tbs[, 2:ncol(.)] %>%
        purrr::map(caret::confusionMatrix, data = tbs[["Adaboost.xpn"]]) %>%
        purrr::map_dbl(~ .[["overall"]]["Accuracy"])
    }) %>%
    purrr::map(~ t(data.frame(.)))

  if (producePlots) {
    plot_dir <- file.path(output_dir, "gene_selection", "plots")

    if ("lasso" %in% algs) {
      cli::cat_line("Plotting lasso figures")

      pdf(file.path(plot_dir, "lasso_heatmap.pdf"))
      pheatmap::pheatmap(res[["lasso"]], main = "Accuracy By Study - Lasso")
      dev.off()

      pdf(file.path(plot_dir, "lasso_boxplot.pdf"))
      boxplot(res[["lasso"]], names = seq(4, 94, 5), main = "Lasso")
      dev.off()
    }
    if ("rf" %in% algs) {
      cli::cat_line("Plotting rf figures")

      pdf(file.path(plot_dir, "rf_heatmap.pdf"))
      pheatmap::pheatmap(res[["rf"]], main = "Accuracy By Study - Random Forest")
      dev.off()

      pdf(file.path(plot_dir, "rf_boxplot.pdf"))
      boxplot(res[["rf"]], names = seq(4, 94, 5), main = "Random Forest")
      dev.off()
    }

    cli::cat_line("Producing loso plots")
    # Overall accuracy
    oacc <- l_train %>%
      purrr::map(function(alg) {
        dplyr::select(alg, dplyr::matches("ng")) %>%
          purrr::map(caret::confusionMatrix, data = alg[["Adaboost.xpn"]]) %>%
          purrr::map_dbl(~ .[["overall"]]["Accuracy"]) %>%
          list(Accuracy = .)
      })
    loso_plot(
      file_name = file.path(plot_dir, "LOSO_accuracy.pdf"),
      data = oacc,
      group = "Accuracy",
      main = "Overall Accuracy\n by # of genes",
      ylab = "Accuracy"
    )

    # F1 Score by class
    byclass <- l_train %>%
      purrr::map(function(alg) {
        dplyr::select(alg, dplyr::matches("ng")) %>%
          purrr::map(caret::confusionMatrix, data = alg[["Adaboost.xpn"]]) %>%
          purrr::map(~ .[["byClass"]][, "F1"]) %>%
          purrr::transpose() %>%
          purrr::map(unlist) %>%
          purrr::set_names(gsub("\\.", "-", names(.)))
      })
    f1_args <- c("C1-MES", "C2-IMM", "C4-DIF", "C5-PRO") %>%
      purrr::map(~ list(
        file_name = file.path(plot_dir, paste0("LOSO_F1_", ., ".pdf")),
        group = paste("Class:", .),
        main = paste0("F1 Score for ", ., "\nby # of genes")
      ))
    purrr::walk(f1_args, ~ purrr::invoke(loso_plot, ., data = byclass))
  }
}

summarize_freqs <- function(output_dir, train_dat, algs) {
  cli::cat_rule("Calculating summary frequencies")
  fnames <- list.files(
    path = file.path(output_dir, "gene_selection", "sum_freq"),
    pattern = "[A-Z]{3}_sum_freq.csv",
    full.names = TRUE
  )
  overall <- overall_freq(fnames)

  cli::cat_line("Overall frequencies")
  readr::write_csv(overall,
                   file.path(output_dir, "gene_selection", "sum_freq", "overall_freq.csv"))

  if ("lasso" %in% algs) {
    fnames <- list.files(
      path = file.path(output_dir, "gene_selection", "boot_freq"),
      pattern = "lasso_by_class",
      full.names = TRUE
    )
    overall_lasso <- overall_freq(fnames)

    cli::cat_line("Overall lasso by-class frequencies")
    readr::write_csv(overall_lasso,
                     file.path(output_dir, "gene_selection", "sum_freq", "overall_lasso_by_class.csv"))
  }
}

overall_freq <- function(files) {
  files %>%
    purrr::map(readr::read_csv, col_types = readr::cols()) %>%
    purrr::map(dplyr::arrange, .data$genes) %>%
    purrr::map(as.data.frame) %>%
    purrr::map(tibble::column_to_rownames, "genes") %>%
    purrr::map(function(x) x / length(.)) %>%
    purrr::reduce(`+`) %>%
    tibble::rownames_to_column("genes") %>%
    dplyr::mutate_at(-1, round, digits = 5)
}

analyze_genes <- function(output_dir, train_dat, algs) {
  cli::cat_rule("Running Gene analysis")
  plot_dir <- file.path(output_dir, "gene_selection", "plots")
  fnames <- list.files(file.path(output_dir, "gene_selection", "sum_freq"),
                       pattern = "_sum_freq.csv", full.names = TRUE)
  site_names <- table(train_dat$site) %>% paste0(names(.), .)

  if ("lasso" %in% algs) {
    cli::cat_line("lasso")
    lasso30 <- purrr::map(fnames, ~ {
      readr::read_csv(file = ., col_types = readr::cols()) %>%
        dplyr::arrange(dplyr::desc(lassoFreq)) %>%
        dplyr::pull(genes) %>%
        head(30)
    }) %>%
      purrr::set_names(site_names)

    genes30 <- Reduce(union, lasso30)
    geneRanks_lasso <- lasso30 %>%
      purrr::map(match, x = genes30, nomatch = 50) %>%
      as.data.frame() %>%
      magrittr::set_rownames(genes30)

    pdf(file.path(plot_dir, "lasso30_heatmap.pdf"))
    pheatmap::pheatmap(geneRanks_lasso, fontsize_row = 7, main = "Lasso")
    dev.off()
  }
  if ("rf" %in% algs) {
    cli::cat_line("rf60")
    rf60 <- purrr::map(fnames, ~ {
      readr::read_csv(file = ., col_types = readr::cols()) %>%
        dplyr::arrange(dplyr::desc(rfFreq)) %>%
        dplyr::pull(genes) %>%
        head(60)
    }) %>%
      purrr::set_names(site_names)

    genes60 <- Reduce(union, rf60)
    geneRanks_rf <- rf60 %>%
      purrr::map(match, x = genes60, nomatch = 70) %>%
      as.data.frame() %>%
      magrittr::set_rownames(genes60)

    pdf(file.path(plot_dir, "rf60_heatmap.pdf"))
    pheatmap::pheatmap(geneRanks_rf, fontsize_row = 6, main = "Random Forest")
    dev.off()
  }
}


# 7 - Final Model ---------------------------------------------------------

#' Prepare NanoString samples for prediction: make the column names
#' syntactically valid, coerce to data frame, move OTTA id to row names, and
#' keep only numeric columns (expression data)
prepare_samples <- function(data) {
  data %>%
    `colnames<-`(make.names(colnames(.))) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("OTTA.ID") %>%
    dplyr::select_if(is.numeric)
}

#' Predict NanoString samples from final model. Combine the predicted
#' assignments with predicted probability matrix.
predict_samples <- function(mod, data) {
  predict(mod, data) %>%
    tibble::enframe(name = "ottaID", value = "predicted") %>%
    cbind(predict(mod, data, type = "prob")) %>%
    tibble::as_tibble()
}
