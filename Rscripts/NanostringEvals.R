#Supporting functions
library(tidyverse)
library(splendid)

# Functions----
build_mapping <- function(train.set)
  #********************************************************************
  # Generate substrain mapping table for specified study based on 
  #Tim' s pathway analysis
  #********************************************************************
{
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

import_study <- function(dir, study)
  #********************************************************************
  # Import specified study with housekeeping normalization or not.
  #     is.test.set: required to specify format if test set should be
  #                  returned.
  #********************************************************************
{ # import the npcp and diceR labels
  dat <- readr::read_rds(paste0(dir, "data_pr_", study, "/npcp-hcNorm_", study, ".rds"))
  rownames(dat) <- rownames(dat) %>% stringr::str_sub(0, nchar(.) - 7)
  labs <- readr::read_rds(paste0(dir, "data_pr_", study, "/all_clusts_", study, ".rds"))$kmodes
  
  # if test set, perform mapping
    mapping <- build_mapping(study)
    labs.mapped <- data.frame(labs = labs) %>%
      inner_join(., mapping, by = "labs") %>%
      select(labels)
    df <- data.frame(y = labs.mapped$labels, dat)
  
  return(df)
}

train <- function(x.processed, alg)
  #********************************************************************
  # train a given algorithm on a given training set
  #   x.processed: training set
  #   alg: algorithm of interest (see splendid docs for further details)
  #********************************************************************
{
  x <- x.processed[,-1]
  lab <- x.processed[,1]
  fit <- splendid::classification(data = x, class = lab, algorithms = alg, standardize = FALSE)
  return(fit)
}

load_nanostring <- function(dir,genes)
  #******************************************************************
  # Load Nanostring data - all batches
  # Inputs:
  # the directory of the Nanostring data
  # the genes needed for the model
  #*****************************************************************
{
  # import batch 1 nanostring
  b1 <- read.csv(paste0(dir,"nanostring_classifier_data_batch1_20170217_updated.csv"), header = TRUE,stringsAsFactors = FALSE) 
  
  # import batch 2 nanostring
  b2 <- read.csv(paste0(dir,"nanostring_classifier_data_batch2_20170221.csv"),header = TRUE,stringsAsFactors = FALSE) 
  
  # import batch 3 nanostring
  b3 <- read.csv(paste0(dir,"nanostring_classifier_data_batch3_20170307_updated_NCO.csv"),header = TRUE,stringsAsFactors = FALSE) 
  
  # import batch 4 nanostring
  b4 <- read.csv(paste0(dir,"nanostring_classifier_data_batch4_20170512.csv"), header = TRUE, stringsAsFactors = FALSE) 
  
  # combine into list
  test.dat <- list(
    b1 = b1,
    b2 = b2,
    b3 = b3,
    b4 = b4
  )
  
  # bind the data and make rownames= otta.id
  matched <- purrr::map(test.dat, function(x) x %>% select(c("OTTA.ID", genes))) %>%
    bind_rows() %>%
      tibble::column_to_rownames("OTTA.ID") %>% 
    na.omit()
  
  return(matched)
}

get_mapping <- function(dir="./data/nanostring/")
  #********************************************************************
  # Import overlapping samples from TCGA and AOCS and combine. Table
  # also includes published labels.
  #********************************************************************
{
  # TCGA overlap
  tcga.mapped <- readr::read_csv(paste0(dir, "TCGA_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(sampleID = TCGA, ottaID = `OTTA-ID`, published = `MOL-SUBTYPE-NAME (published)`))%>% 
    filter(published != "n/a")
  
  # GSE overlap
  aocs.mapped <- readr::read_csv(paste0(dir, "GSE9891_sampleIDs_OTTA-Mapped.csv")) %>%
    select(c(sampleID = GSE9891, ottaID = `OTTA ID`, published = `MOL-SUBTYPE-NAME (published)`))%>% 
    filter(published != "n/a") %>% 
    filter(sampleID != "OV_GSE9891_GSM249786_X60174.CEL.gz")
  
  # combine & drop NAs
  overall.mapped <- bind_rows(aocs.mapped,tcga.mapped) 
  
  return(list(tcga=tcga.mapped,aocs=aocs.mapped,overall=overall.mapped))
}

import_array <- function(dir="./data/", mapping)
  #********************************************************************
  # Import array data of overlapped samples and select those that
  # match the mapping table returned from get_mapping()
  #********************************************************************
{
  # GSE
  validation.gse <- readr::read_rds(paste0(dir, "ValidationSet/validation_gse.rds")) %>%
    tibble::rownames_to_column("sampleID") %>% 
    inner_join(mapping$aocs,., by="sampleID")
  
  # TCGA
  validation.tcga <- readr::read_rds(paste0(dir, "ValidationSet/validation_tcga.rds")) %>%
    tibble::rownames_to_column("sampleID") %>% 
    inner_join(mapping$tcga,., by="sampleID")
  
  
  # combine and match with mapping table
  validation.set <- bind_rows(validation.gse, validation.tcga) %>%
    tibble::column_to_rownames("sampleID")
  
  return(list(aocs.array=validation.gse, tcga.array= validation.tcga, all.array=validation.set))
}

get_nstring_overlap <- function(dir, mapping, nsdat)
  #********************************************************************
  # Import nanostring data of overlapped samples and select those that
  # match the mapping table returned from get_mapping()
  #********************************************************************
{
  nanostring <- nsdat %>%
    tibble::rownames_to_column("ottaID") %>%
    inner_join(mapping, ., by = "ottaID") %>%
    tibble::column_to_rownames("ottaID")
  return(nanostring)
}


# Plots ----

reliability_plot <- function (x, pred.probs, title="")
  #********************************************************************
  # Reliability plot
  #********************************************************************
{
  df <- levels(x) %>% 
    purrr::set_names() %>% 
    purrr::map(~{
      prob <- pred.probs[, .x]
      cl <- ifelse(x == .x, 1, 0)
      bin.pred <- cut(prob, 10)
      purrr::map_df(levels(bin.pred), ~{
        idx <- .x == bin.pred
        data.frame(V1 = mean(prob[idx]), V2 = sum(cl[idx])/length(cl[idx]))
      }) %>% 
        dplyr::filter(!is.nan(V1)) %>% 
        with(., stats::lowess(V1, V2))
    }) %>% 
    purrr::map2(names(.), ~c(.x, class = .y)) %>% 
    purrr::map(tibble::as.tibble) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(class = factor(class))
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df, aes_(~x, ~y, group = ~class, colour = ~class)) + 
    geom_line(lwd = 2) + 
    geom_abline(intercept = 0, slope = 1, color = "grey") + 
    scale_color_manual(values = cols) + 
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) + 
    scale_y_continuous(breaks = seq(0, 1, 0.2)) + 
    labs(x = "Mean Prediction", y = "Observed Fraction") + 
    theme_bw() + theme(plot.title = element_text(face = "bold"), 
                       panel.grid = element_blank(), 
                       legend.title = element_blank(), 
                       legend.position = c(0, 1), 
                       legend.justification = c(0, 1)) +
    ggtitle(title)
  return(p)
}

discrimination_plot <- function (x, pred.probs, title="") 
  #********************************************************************
  # Discrimination plot
  #********************************************************************
{
  df.long <- data.frame(trueClass = x, pred.probs) %>% 
    tidyr::gather(key = "class", value = "prob", -1, factor_key = TRUE)
  df.prevalence <- df.long %>% dplyr::group_by_("trueClass") %>% 
    dplyr::summarise_(classCount = ~length(trueClass)) %>% 
    dplyr::mutate_(totalCount = ~sum(classCount), prevalence = ~classCount/totalCount)
  cols <- grDevices::rainbow(dplyr::n_distinct(x))
  p <- ggplot(df.long, aes_(x = ~class, y = ~prob, fill = ~class)) + 
    geom_boxplot(alpha = 0.6) + 
    geom_hline(data = df.prevalence, aes_(yintercept = ~prevalence), colour = "lightgrey") + 
    scale_fill_manual(values = cols) + facet_wrap(~trueClass) + 
    labs(x = "Class", y = "Class Probability") + theme_bw() + 
    theme(plot.title = element_text(face = "bold"), 
          panel.grid = element_blank(), 
          legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle(title)
  return(p)
}

plot_evals <- function(eval.dir, plot.title, algs = c("ridge", "lasso"), threshold = TRUE, print = TRUE, save = TRUE, col.cust = NULL)
  #********************************************************************
  # Plot evaluation measures by class and overall.
  #   eval.dir: path to evaluation list
  #********************************************************************
{
  # import eval list
  evals <- readr::read_rds(eval.dir)
  
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
      col <- c("#878787", "#c6c6c6","magenta", "#f49ae3")
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
    ggtitle(plot.title) + ylim(c(0.1, 1))
  
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
      col <- c("#c6c6c6", "#878787", "#f49ae3", "magenta")
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
    ggtitle(plot.title) + ylim(c(0.25, 0.9))
  
  # save plots
  if(save)
  {
    ggsave(p1, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png"))
  }
  
  # print plots
  if(print) {print(p1); print(p2)}
  
  return(list(
    p1, p2
  ))
}

plot_evals_noCBT <- function(eval.dir1, eval.dir2, plot.title, algs = c("ridge", "lasso", "adaboost", "rf"), 
                             threshold = TRUE, print = TRUE, save = TRUE, col.cust = NULL, 
                             y.lim.class = c(0.1, 1), y.lim.all = c(0.25, 0.9))
  #********************************************************************
  # Plot evaluation measures by class and overall.
  #   eval.dir: path to evaluation list
  #********************************************************************
{
  # import eval list
  evals <- readr::read_rds(eval.dir1)
  evals.2 <- readr::read_rds(eval.dir2)
  
  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~.x$cs %>% data.frame(cs = .) %>% 
                 tibble::rownames_to_column("measure"))
  evals.extract.2 <- evals.2 %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~.x$cs %>% data.frame(cs = .) %>% 
                 tibble::rownames_to_column("measure")) 
  
  # grab the names of the list
  names.list <- names(evals.extract)
  names.list.2 <- names(evals.extract.2)
  
  
  # prepare the eval list for plotting
  evals.prep.1 <- purrr::map2(names.list, evals.extract, 
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
    select(-c(cs, alg)) %>%
    filter(batch_correction == "xpn")
  
  evals.prep.2 <- purrr::map2(names.list.2, evals.extract.2, 
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
    select(-c(cs, alg)) %>%
    filter(batch_correction == "xpn")
  
  evals.prep <- rbind(evals.prep.1, evals.prep.2)
  
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
  overall.1 <- evals %>%
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
    select(-mod)  %>%
    filter(batch_correction == "xpn")
  
  overall.2 <- evals.2 %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~data.frame(accuracy = .x$accuracy, 
                           auc = .x$auc, 
                           macro_f1 = .x$macro_f1)) %>%
    purrr::map2(., names.list.2, ~data.frame(mod = .y, .x)) %>%
    dplyr::bind_rows() %>%
    reshape2::melt(variable.name = "measure", value.name = "value") %>%
    mutate(alg = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(batch_correction = stringr::str_split(mod, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    select(-mod)  %>%
    filter(batch_correction == "xpn")
  
  overall.prep <- rbind(overall.1, overall.2)
  
  
  # reorganize factor levels
  overall.prep$alg <- factor(overall.prep$alg, levels = rev(levels(overall.prep$alg)))
  
  # specify colouring
  col <- c("#878787", "magenta", "#05660e", "#e66b00")
  
  # plot overall evaluation measures
  p2 <- ggplot(overall.prep, aes(x = alg, y = value, 
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
    ggsave(p1, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png"))
  }
  
  # print plots
  if(print) {print(p1); print(p2)}
  
  return(list(
    p1, p2
  ))
}

sup_plots <- function(dir = "./", threshold = TRUE, plot.title, algs = c("mlr_ridge", "mlr_lasso"), print = TRUE, save = TRUE, col.cust = NULL)
  #********************************************************************
  # Plot evaluation measures by class and overall for top algorithms
  # across bootstrap samples (output retrieved from supervised pipeline)
  #********************************************************************
{
  if(!threshold)
  {
    # import IV results
    sup.iv.xpn <- readr::read_rds("data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds")
    sup.iv.cbt <- readr::read_rds("data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds")
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds("data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
    sup.iv.cbt <- readr::read_rds("data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
  }
  
  # create mapping tables for xpn & cbt
  xpn.map <- build_mapping("ov.afc1_xpn") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  cbt.map <- build_mapping("ov.afc1_cbt") %>%
    mutate(class = as.factor(labs)) %>% select(-labs)
  
  # process the data
  iv.xpn.class <- sup.iv.xpn %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(stringr::str_detect(measure, '\\.')) %>%
    mutate(class = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(measure = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    inner_join(., xpn.map, by = "class")
  
  iv.cbt.class <- sup.iv.cbt %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(stringr::str_detect(measure, '\\.')) %>%
    mutate(class = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
    mutate(measure = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
    inner_join(., cbt.map, by = "class")
  
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
  
  # process data for general metrics
  iv.xpn <- sup.iv.xpn %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.cbt <- sup.iv.cbt %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
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
    ggsave(p1, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("output/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png")) 
  }
  
  # print plots
  if(print) {print(p1);print(p2)}
  
  return(list(
    p1, p2
  ))
}


top_algo_plot <- function(dir = "./", threshold = TRUE, plot.title, print = TRUE, save = TRUE, col.cust = NULL)
  #********************************************************************
  # Plot evaluation measures by class and overall for top algorithms
  # across bootstrap samples (output retrieved from supervised pipeline)
  #********************************************************************
{
  if(!threshold)
  {
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir,"data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir,"data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds"))
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds("data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds")
    sup.iv.cbt <- readr::read_rds("data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds")
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
  
  df <- iv.combine %>%
    group_by(batch_correction, mod, measure)
  df$mod  <- with(df, reorder(mod, -percentile_50))
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
  if(save) ggsave(p1, filename = paste0("output/plots/all_algos_ranked.png"))
  
  if(print) print(p1)
  
  return(p1)
}


