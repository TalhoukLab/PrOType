# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  require(randomForest)
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

import_study <- function(dir, study = "ov.afc1_cbt", hc.normalize = TRUE)
#********************************************************************
# Import specified study with housekeeping normalization or not.
#     is.test.set: required to specify format if test set should be
#                  returned.
#********************************************************************
{
  # specify hc normalized npcp or not 
  if(hc.normalize)
  {
    hc <- "-hcNorm"
  } else {
    hc <- ""
  }
  
  # import the npcp and diceR labels
  dat <- readr::read_rds(
    paste0(dir, "data_pr_", study, "/npcp", hc, "_", study, ".rds")
  )
  rownames(dat) <- rownames(dat) %>% stringr::str_sub(0, nchar(.) - 7)
  
  # select k-modes as best enemble algorithm
  labs <- readr::read_rds(
    paste0(dir, "data_pr_", study, "/all_clusts_", study, ".rds")
  )$kmodes 
  
  # map labels to npcp
  mapping <- build_mapping(study)
  labs.mapped <- data.frame(labs = labs) %>%
    inner_join(., mapping, by = "labs") %>%
    select(labels)
  df <- data.frame(y = labs.mapped$labels, dat)
  
  return(df)
}

train_final <- function(x.processed, alg)
#********************************************************************
# train a given algorithm on a given training set
#   x.processed: training set
#   alg: algorithm of interest (see splendid docs for further details)
#********************************************************************
{
  x <- x.processed[,-1]
  lab <- x.processed[,1]
  fit <- splendid::classification(
    data = x, class = lab, 
    algorithms = alg, standardize = FALSE
  )
  return(fit)
}

get_mapping <- function(dir = "data/")
#********************************************************************
# Import overlapping samples from TCGA and GSE and combine. Table
# also includes published labels.
#********************************************************************
{
  # TCGA overlap
  tcga.mapped <- readr::read_csv(paste0(dir, "TCGA_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(
      sampleID = TCGA, 
      ottaID = `OTTA-ID`, 
      published = `MOL-SUBTYPE-NAME (published)`
      ))
  
  # GSE overlap
  gse.mapped <- readr::read_csv(paste0(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(
      sampleID = GSE9891, 
      ottaID = `OTTA ID`, 
      published = `MOL-SUBTYPE-NAME (published)`
      ))
  
  # combine & drop NAs
  map <- bind_rows(tcga.mapped, gse.mapped) %>% 
    filter(published != "n/a")
  
  return(map)
}

import_array <- function(dir = "data/", map)
#********************************************************************
# Import array data of overlapped samples and select those that
# match the mapping table returned from get_mapping()
#********************************************************************
{
  # GSE
  validation.gse <- readr::read_rds(
    paste0(dir, "ValidationSet/validation_gse.rds")
    ) %>% tibble::rownames_to_column("sampleID")
  
  # TCGA
  validation.tcga <- readr::read_rds(
    paste0(dir, "ValidationSet/validation_tcga.rds")
    ) %>% tibble::rownames_to_column("sampleID")
  
  # combine and match with mapping table
  validation.set <- bind_rows(validation.gse, validation.tcga) %>%
    inner_join(map, ., by = "sampleID") %>%
    select(-c(ottaID, published)) %>%
    tibble::column_to_rownames("sampleID")
  
  return(validation.set)
}

get_overlap <- function(array, pred, map)
#********************************************************************
# Combine array with predictions and and join with mapping table
#********************************************************************
{
  # join the validation set
  overlap <- array %>%
    tibble::rownames_to_column("sampleID") %>%
    data.frame(., array = pred) %>%
    inner_join(., map, by = "sampleID") %>%
    select(sampleID, ottaID, published, array) %>%
    filter(published != "n/a") %>%
    mutate(published = as.factor(published))
  return(overlap)
}

predict_overlap <- function(fit, new.data)
#********************************************************************
# Simple predict function to take it a fit and predict on new.data
#********************************************************************
{
  pred <- splendid::prediction(
    mod = fit, 
    data = new.data, 
    class = 1:nrow(new.data), 
    threshold = 0
    ) %>% data.table::setattr(., "sampleID", rownames(new.data))
  return(pred)
}

evaluate_array <- function(x)
#********************************************************************
# Return list of evaluation measures. Output is required for ploting.
#********************************************************************
{
  published_vs_array <- list(
    splendid::evaluation(x$published, x$array),
    caret::confusionMatrix(x$published, x$array)
  )
  return(list(
    published_vs_array = published_vs_array
  ))
}

top_algo_plot <- function(dir = "./", threshold = TRUE, plot.title, 
                          print = TRUE, save = TRUE, col.cust = NULL)
#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
{
  if(!threshold)
  {
    # import IV results
    sup.iv.xpn <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds")
      )
    sup.iv.cbt <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds")
      )
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
      )
    sup.iv.cbt <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
      )
  }
  
  # create mapping tables for xpn & cbt
  xpn.map <- build_mapping("ov.afc1_xpn") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  cbt.map <- build_mapping("ov.afc1_cbt") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  
  # process data for general metrics
  iv.xpn <- sup.iv.xpn %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.cbt <- sup.iv.cbt %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.combine <- rbind.data.frame(iv.xpn, iv.cbt) %>%
    mutate(batch_correction = as.factor(batch_correction))
  
  # plot general metrics
  pd <- position_dodge(width = 0.5)
  if(is.null(col.cust))
  {
    if(threshold)
    {
      col <- c("#e66b00", "#efa667",
               "#05660e", "#7acc81",
               "#878787", "#c6c6c6",
               "blue", "#6f95e8", 
               "magenta", "#f49ae3", 
               "purple", "#b296d3", 
               "#ffbe00", "#ffd866",
               "brown", "#ce9565",
               "red", "#dd9d9d")
    } else {
      col <- c("#878787", "#c6c6c6",
               "magenta", "#f49ae3", 
               "blue", "#6f95e8", 
               "purple", "#b296d3", 
               "#05660e", "#7acc81",
               "#e66b00", "#efa667",
               "#ffbe00", "#ffd866",
               "brown", "#ce9565",
               "red", "#dd9d9d")
    }
  } else {
    col <- col.cust
  }
  
  # prepare df for ggplot2
  df <- iv.combine %>%
    group_by(batch_correction, mod, measure)
  df$mod  <- with(df, reorder(mod, -percentile_50))
  
  # plot evaluation measures
  p1 <- df %>% ggplot(aes(x = reorder(mod, -percentile_50), y = percentile_50, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(position = pd) +
    facet_wrap(~measure) +
    geom_errorbar(aes(ymin=percentile_5, ymax=percentile_95), width=0.4, position = pd) +
    theme_bw() +
    ylim(0.6, 1) +
    scale_colour_manual(values = col, name = "Batch and Model") +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title) +
    theme(legend.position = 'bottom') +
    guides(color=guide_legend(nrow=2,byrow=FALSE))
  
  # save plot
  if(save) ggsave(p1, filename = paste0("outputs/plots/all_algos_ranked.png"))
  
  if(print) print(p1)
  
  return(p1)
}

sup_plots <- function(dir = "./", threshold = TRUE, plot.title, 
                      algs = c("mlr_ridge", "mlr_lasso"), 
                      print = TRUE, save = TRUE, col.cust = NULL)
#********************************************************************
# Plot evaluation measures by class and overall for top algorithms
# across bootstrap samples (output retrieved from supervised pipeline)
#********************************************************************
{
  if(!threshold)
  {
    # import IV results
    sup.iv.xpn <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds")
      )
    sup.iv.cbt <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds")
      )
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
      )
    sup.iv.cbt <- readr::read_rds(
      paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
      )
  }
  
  # create mapping tables for xpn & cbt
  xpn.map <- build_mapping("ov.afc1_xpn") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  cbt.map <- build_mapping("ov.afc1_cbt") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  
  # process the xpn data
  iv.xpn.class <- sup.iv.xpn %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(stringr::str_detect(measure, '\\.')) %>%
    mutate(
      class = stringr::str_split(measure, "\\.") %>% 
        purrr::map(., ~.x[2]) %>% unlist %>% as.factor
      ) %>%
    mutate(
      measure = stringr::str_split(measure, "\\.") %>% 
        purrr::map(., ~.x[1]) %>% unlist %>% as.factor
      ) %>%
    inner_join(., xpn.map, by = "class")
  
  # process the cbt data
  iv.cbt.class <- sup.iv.cbt %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(stringr::str_detect(measure, '\\.')) %>%
    mutate(
      class = stringr::str_split(measure, "\\.") %>% 
        purrr::map(., ~.x[2]) %>% unlist %>% as.factor
      ) %>%
    mutate(
      measure = stringr::str_split(measure, "\\.") %>% 
        purrr::map(., ~.x[1]) %>% unlist %>% as.factor
      ) %>%
    inner_join(., cbt.map, by = "class")
  
  # combine xpn and cbt
  iv.combine.class <- rbind.data.frame(iv.xpn.class, iv.cbt.class) %>%
    mutate(batch_correction = as.factor(batch_correction))
  
  # plot class-wise metrics
  pd <- position_dodge(width = 0.5)
  if(is.null(col.cust))
  {
    if(threshold)
    {
      col <- c("#05660e", "#7acc81", "#e66b00", "#efa667")
    } else {
      col <- c("#878787", "#c6c6c6","magenta", "#f49ae3")
    }
  } else {
    col <- col.cust
  }
  
  # create iv plot
  p1 <- iv.combine.class %>%
    group_by(batch_correction, mod, measure) %>%
    ggplot(aes(x = labels, y = percentile_50, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(stat='summary', fun.y=mean, position = pd) +
    #stat_summary(fun.y=mean, geom="line") +
    geom_errorbar(aes(ymin=percentile_5, ymax=percentile_95), width=.4, position = pd) +
    theme_bw() +
    facet_wrap(~measure, scales = "free") + 
    scale_colour_manual(values = col, name = "Batch and Model") +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Subtype") +
    ggtitle(plot.title)
  
  # process xpn data for general metrics
  iv.xpn <- sup.iv.xpn %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  # process cbt data for general metrics
  iv.cbt <- sup.iv.cbt %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  # combine xpn and cbt for general metrics
  iv.combine <- rbind.data.frame(iv.xpn, iv.cbt) %>%
    mutate(batch_correction = as.factor(batch_correction))
  
  # plot general metrics
  p2 <- iv.combine %>%
    group_by(batch_correction, mod, measure) %>%
    ggplot(aes(x = mod, y = percentile_50, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(position = pd) +
    geom_errorbar(aes(ymin=percentile_5, ymax=percentile_95), width=.4, position = pd) +
    theme_bw() +
    facet_wrap(~measure, scales = "free") +
    scale_colour_manual(values = col, name = "Batch and Model") +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title)
  
  # save plots
  if(save)
  {
    ggsave(p1, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png")) 
  }
  
  # print plots
  if(print) {print(p1);print(p2)}
  
  return(list(
    p1, p2
  ))
}

plot_evals <- function(eval.dir, plot.title, algs = c("mlr_ridge", "mlr_lasso"), 
                       threshold = TRUE, print = TRUE, save = TRUE, col.cust = NULL)
#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.dir: path to evaluation list
#********************************************************************
{
  # import eval list
  evals <- readr::read_rds(eval.dir) %>%
    purrr::map2(., names(.), function(x, y) {
      if(grepl(paste(algs, collapse="|"), y))
      {
        purrr::map(x, ~.x)
      }
    }) %>% purrr::compact()
  
  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~.x$cs %>% data.frame(cs = .) %>% 
                 tibble::rownames_to_column("measure"))
  
  # grab the names of the list
  names.list <- names(evals.extract)
  
  # prepare the eval list for plotting
  evals.prep <- purrr::map2(names.list, evals.extract, 
                            ~.y %>% data.frame(alg = .x,.)) %>%
    bind_rows() %>% mutate(alg = as.factor(alg), measure = as.factor(measure)) %>%
    mutate(class = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(measure = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    mutate(mod = stringr::str_split(alg, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(batch_correction = stringr::str_split(alg, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    mutate(value = cs) %>%
    select(-c(cs, alg))
  
  # reorganize factor levels
  evals.prep$mod <- factor(evals.prep$mod, levels = rev(levels(evals.prep$mod)))
  
  # specify positioning and colour
  pd <- position_dodge(width = 0.5)
  if(is.null(col.cust))
  {
    if(threshold)
    {
      col <- c("#05660e", "#7acc81", "#e66b00", "#efa667")
    } else {
      col <- c("#c6c6c6", "#878787", "#f49ae3", "magenta")
    }
  } else {
    col <- col.cust
  }
  
  brks <- paste(rep(c("xpn", "cbt"), 2), rep(algs, each = 2), sep = ".")
  
  # create class-wise plots for class eval measures
  p1 <- evals.prep %>%
    ggplot(aes(class, value, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(position = pd, size = 3) + 
    facet_wrap(~measure, scales = "free") +
    theme_bw() +
    facet_wrap(~measure, scales = "free") +
    scale_colour_manual(values = col, name = "Batch and Model", breaks = brks) +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title) + ylim(c(0, 1))
  
  # prepare eval list for plottin overall evaluation measures
  overall <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~data.frame(accuracy = .x$accuracy, 
                           auc = .x$auc, 
                           macro_f1 = .x$macro_f1)) %>%
    purrr::map2(., names.list, ~data.frame(mod = .y, .x)) %>%
    dplyr::bind_rows() %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    mutate(alg = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(batch_correction = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    select(-mod) 
  
  # reorganize factor levels
  overall$alg <- factor(overall$alg, levels = rev(levels(overall$alg)))
  
  # specify colouring
  if(is.null(col.cust))
  {
    if(threshold)
    {
      col <- c("#7acc81", "#05660e", "#efa667", "#e66b00")
    } else {
      col <- c("#f49ae3", "magenta", "#c6c6c6", "#878787")
    }
  } else {
    col <- col.cust
  }
  
  # plot overall evaluation measures
  p2 <- ggplot(overall, aes(x = alg, y = value, 
                            colour = interaction(batch_correction, alg), 
                            group=interaction(batch_correction, alg))) +
    geom_point(position = pd, size = 3) +
    facet_wrap(~measure, scales = "free") +
    theme_bw() +
    scale_colour_manual(values = col, name = "Batch and Model", breaks = brks) +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title) + ylim(c(0.3, 0.9))
  
  # save plots
  if(save)
  {
    ggsave(p1, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png"))
  }
  
  # print plots
  if(print) {print(p1); print(p2)}
  
  return(list(
    p1, p2
  ))
}


plot_evals_noCBT <- function(dir, plot.title, algs = c("mlr_ridge", "mlr_lasso", "adaboost", "rf"), 
                             print = TRUE, save = TRUE, col.cust = NULL, 
                             y.lim.class = c(0.1, 1), y.lim.all = c(0.25, 1))
#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.dir: path to evaluation list
#********************************************************************
{
  # import eval list
  evals <- readr::read_rds(dir)
  
  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(
      ~.x$cs %>% data.frame(cs = .) %>% 
        tibble::rownames_to_column("measure")
      )
  
  # grab the names of the list
  names.list <- names(evals.extract)
  
  # prepare the eval list for plotting
  evals.prep <- purrr::map2(names.list, evals.extract, function(x,y) {
    y %>% data.frame(alg = x,.)
  }) %>%
    bind_rows() %>% 
    mutate(alg = as.factor(alg), measure = as.factor(measure)) %>%
    mutate(class = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(measure = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    mutate(mod = stringr::str_split(alg, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(batch_correction = stringr::str_split(alg, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    mutate(value = cs) %>%
    select(-c(cs, alg)) %>%
    filter(batch_correction == "xpn")
  #evals.prep <- evals.prep.1
  
  # reorganize factor levels
  evals.prep$mod <- factor(evals.prep$mod, levels = rev(levels(evals.prep$mod)))
  
  # specify positioning and colour
  pd <- position_dodge(width = 0.5)
  col <- c("#878787", "magenta", "#05660e", "#e66b00")
  brks <- paste(rep(c("xpn"), 4), algs, sep = ".")
  
  # create class-wise plots for class eval measures
  p1 <- evals.prep %>%
    ggplot(aes(class, value, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(position = pd, size = 3) + 
    facet_wrap(~measure, scales = "free") +
    theme_bw() +
    facet_wrap(~measure, scales = "free") +
    scale_colour_manual(values = col, name = "Batch and Model", breaks = brks) +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title) + ylim(y.lim.class)
  
  # prepare eval list for plottin overall evaluation measures
  overall.prep <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~data.frame(accuracy = .x$accuracy, 
                           auc = .x$auc, 
                           macro_f1 = .x$macro_f1)
               ) %>%
    purrr::map2(., names.list, ~data.frame(mod = .y, .x)) %>%
    dplyr::bind_rows() %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    mutate(alg = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(batch_correction = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    select(-mod)  %>%
    filter(batch_correction == "xpn")
  
  # reorganize factor levels
  overall.prep$alg <- factor(overall.prep$alg, levels = rev(levels(overall.prep$alg)))
  
  # specify colouring
  col <- c("#05660e", "magenta", "#878787", "#e66b00")
  
  # plot overall evaluation measures
  p2 <- ggplot(overall.prep, 
               aes(x = alg, y = value, 
                   colour = interaction(batch_correction, alg), 
                   group=interaction(batch_correction, alg))) +
    geom_point(position = pd, size = 3) +
    facet_wrap(~measure, scales = "free") +
    theme_bw() +
    scale_colour_manual(values = col, name = "Batch and Model", breaks = brks) +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title) + ylim(y.lim.all)
  
  # save plots
  if(save)
  {
    ggsave(p1, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png"))
  }
  
  # print plots
  if(print) {print(p1); print(p2)}
  
  return(list(
    p1, p2
  ))
}
