build_mapping <- function(train.set)
  #********************************************************************
  # Generate substrain mapping table for specified study
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


top_algo_plot <- function(dir = "data/intermediate/outputs_Feb10_2018/", 
                          threshold = FALSE, plot.title, print = TRUE, 
                          save = TRUE, col.cust = NULL)
  #********************************************************************
  # Plot evaluation measures by class and overall for top algorithms
  # across bootstrap samples (output retrieved from supervised pipeline)
  #********************************************************************
{
  if(!threshold)
  {
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir, "ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir,"ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds"))
  } else {
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir,"ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir,"ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds"))
  }
  
  # create mapping tables for xpn & cbt
  xpn.map <- build_mapping("ov.afc1_xpn") %>%
    mutate(class = as.factor(labs)) %>% dplyr::select(-labs)
  cbt.map <- build_mapping("ov.afc1_cbt") %>%
    mutate(class = as.factor(labs)) %>% dplyr::select(-labs)
  
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
    xlab(label = "Algorithm") +
    ggtitle(plot.title) +
    theme(
      legend.position = 'bottom',
      axis.text.x = element_text(angle = 90, hjust = 1))+
    guides(color=guide_legend(nrow=2,byrow=FALSE))
  
  # save plot
  thresh <- ifelse(threshold==TRUE,"thresh","nothresh")
  if(save) ggsave(p1, filename = paste0("Outputs/plots/all_algos_ranked_",thresh,".png"))
  
  if(print) print(p1)
  
  return(p1)
}

sup_plots <- function(dir = "data/intermediate/outputs_Feb26_2018/", 
                      plot.title, 
                      algs = c("mlr_ridge", "mlr_lasso"), algs_t = c("adaboost", "rf"), 
                      print = TRUE, save = TRUE, col.cust = NULL)
  #********************************************************************
  # Plot evaluation measures by class and overall for top algorithms
  # across bootstrap samples (output retrieved from supervised pipeline)
  #********************************************************************
{
    # import IV results
    sup.iv.xpn <- readr::read_rds(paste0(dir,"ov.afc1_xpn/iv_summary_ov.afc1_xpn.rds"))
    sup.iv.cbt <- readr::read_rds(paste0(dir,"ov.afc1_cbt/iv_summary_ov.afc1_cbt.rds"))
    sup.iv.xpn_t <- readr::read_rds(paste0(dir,"ov.afc1_xpn/iv_summary_ov.afc1_xpn_threshold.rds"))
    sup.iv.cbt_t <- readr::read_rds(paste0(dir,"ov.afc1_cbt/iv_summary_ov.afc1_cbt_threshold.rds"))
    
    # create mapping tables for xpn & cbt
    xpn.map <- build_mapping("ov.afc1_xpn") %>%
    mutate(class = as.factor(labs)) %>% dplyr::select(-labs)
    
    cbt.map <- build_mapping("ov.afc1_cbt") %>%
    mutate(class = as.factor(labs)) %>% dplyr::select(-labs)
  
    # process the data
    iv.xpn.class <- sup.iv.xpn %>%
      dplyr::filter(mod %in% algs) %>%
      filter(normalization == "hc") %>%
      filter(stringr::str_detect(measure, '\\.')) %>%
      mutate(class = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
      mutate(measure = stringr::str_split(measure, "\\.") %>% 
             purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
      inner_join(., xpn.map, by = "class")
    
    iv.xpn.t.class <- sup.iv.xpn_t %>%
      dplyr::filter(mod %in% algs_t) %>%
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
  
    iv.cbt.t.class <- sup.iv.cbt_t %>%
      filter(mod %in% algs_t) %>%
      filter(normalization == "hc") %>%
      filter(stringr::str_detect(measure, '\\.')) %>%
      mutate(class = stringr::str_split(measure, "\\.") %>% 
               purrr::map(., ~.x[2]) %>% unlist %>% as.factor) %>%
      mutate(measure = stringr::str_split(measure, "\\.") %>% 
               purrr::map(., ~.x[1]) %>% unlist %>% as.factor) %>%
      inner_join(., cbt.map, by = "class")
    
    iv.combine.class <- rbind.data.frame(iv.xpn.class, iv.cbt.class,iv.xpn.t.class, iv.cbt.t.class) %>%
    mutate(batch_correction = as.factor(batch_correction))
  
  # plot class-wise metrics
  pd <- position_dodge(width = 0.7)
  
  if(is.null(col.cust))
  { col <- c("#05660e","#7acc81" ,"magenta","#f49ae3","#878787", "#c6c6c6", "#e66b00", "#efa667")
    
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
    theme (legend.position = 'bottom')+
    scale_colour_manual(values = col, name = "Batch and Model") +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Subtype") +
    ggtitle(plot.title)
  
  
  
  # process data for general metrics
  iv.xpn <- sup.iv.xpn %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  iv.xpn.t <- sup.iv.xpn_t %>%
    filter(mod %in% algs_t) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.cbt <- sup.iv.cbt %>%
    filter(mod %in% algs) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.cbt.t <- sup.iv.cbt_t %>%
    filter(mod %in% algs_t) %>%
    filter(normalization == "hc") %>%
    filter(measure %in% c("auc", "accuracy", "macro_f1"))
  
  iv.combine <- rbind.data.frame(iv.xpn,iv.xpn.t, iv.cbt,iv.cbt.t) %>%
    mutate(batch_correction = as.factor(batch_correction))
  
  # plot general metrics
  p2 <- iv.combine %>%
    group_by(batch_correction, mod, measure) %>%
    ggplot(aes(x = mod, y = percentile_50, colour = interaction(batch_correction, mod), group=interaction(batch_correction, mod))) +
    geom_point(position = pd) +
    geom_errorbar(aes(ymin=percentile_5, ymax=percentile_95), width=.4, position = pd) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme_bw() +
    theme (legend.position = 'bottom', axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_wrap(~measure, scales = "free") +
    scale_colour_manual(values = col, name = "Batch and Model") +
    ylab(label = "Evaluation Measure Value") +
    xlab(label = "Evaluation Measure") +
    ggtitle(plot.title)
  p2
  
  # save plots
  if(save)
  {
    ggsave(p1, filename = paste0("Outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_byclass.png"))
    ggsave(p2, filename = paste0("Outputs/plots/", stringr::str_replace_all(string=plot.title, pattern=" ", repl=""), "_overall.png")) 
  }
  
  # print plots
  if(print) {print(p1);print(p2)}
  
  return(list(
    p1, p2
  ))
}


