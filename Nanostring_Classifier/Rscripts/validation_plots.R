# Dependencies ----


# load dependencies
suppressPackageStartupMessages({
  require(tidyverse)
  source("Rscripts/utils.R")
})


# Functions ----

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

plot_evals_noCBT <- function(eval.dir1, plot.title, algs = c("mlr_ridge", "mlr_lasso", "adaboost", "rf"), 
                             print = TRUE, save = TRUE, col.cust = NULL, 
                             y.lim.class = c(0.1, 1), y.lim.all = c(0.25, 1))
#********************************************************************
# Plot evaluation measures by class and overall.
#   eval.dir: path to evaluation list
#********************************************************************
{
  # import eval list
  evals <- readr::read_rds(eval.dir1)
  
  # do some preprocessing on the eval list
  evals.extract <- evals %>%
    purrr::map(~purrr::flatten(.x)) %>%
    purrr::map(~.x$cs %>% data.frame(cs = .) %>% 
                 tibble::rownames_to_column("measure"))
  
  # grab the names of the list
  names.list <- names(evals.extract)
  
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
  
  evals.prep <- evals.prep.1
  
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
  
  overall.prep <- overall.1
  
  
  # reorganize factor levels
  overall.prep$alg <- factor(overall.prep$alg, levels = rev(levels(overall.prep$alg)))
  
  # specify colouring
  col <- c("#05660e", "magenta", "#878787", "#e66b00")
  
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
    ggsave(p1, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png"))
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
    sup.iv.xpn <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds"))
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds"))
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
    ggsave(p1, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png")) 
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
    sup.iv.xpn <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds"))
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir, "data_pr_ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds"))
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
  if(save) ggsave(p1, filename = paste0("outputs/plots/all_algos_ranked.png"))
  
  if(print) print(p1)
  
  return(p1)
}
  
