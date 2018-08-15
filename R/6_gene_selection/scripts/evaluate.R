evaluatePredictions <- function(output_dir, train_dat, train_lab, algs,
                                producePlots = TRUE) {
  cli::cat_rule("Evaluating Predictions")
  pred_filenames <-
    list.files(path = file.path(output_dir, "GeneSelection/output/studyPreds"),
               full.names = TRUE)

  cli::cat_line("Getting training data names")
  site_names <- table(train_dat$site) %>% paste0(names(.), .)
  l <- pred_filenames %>%
    purrr::set_names(site_names) %>%
    purrr::map(readr::read_rds) %>%
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
    plot_dir <- mkdir(file.path(output_dir, "plots"))

    if ("lasso" %in% algs) {
      cli::cat_line("Plotting lasso boxplot")

      pdf(file.path(plot_dir, "lasso_boxplot.pdf"))
      pheatmap::pheatmap(res[["lasso"]], main = "Accuracy By Study - Lasso")
      boxplot(res[["lasso"]], names = seq(4, 94, 5), main = "Lasso")
      dev.off()
    }
    if ("rf" %in% algs) {
      cli::cat_line("Plotting rf boxplot")

      pdf(file.path(plot_dir, "rf_boxplot.pdf"))
      pheatmap::pheatmap(res[["rf"]], main = "Accuracy By Study - Random Forest")
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
