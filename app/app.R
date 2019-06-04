# Load packages, pipe, and relevant constants
library(shiny)
library(randomForest)
library(ggplot2)
`%>%` <- magrittr::`%>%`
alg <- "rf"
seed <- 2018
weights <- purrr::set_names(c(12, 5, 5) / 22, c("Pool1", "Pool2", "Pool3"))

# Load Vancouver CS3 pools (ref 1), training data/labels, and gene frequencies
pools_ref1 <- readRDS("data/van_pools_cs3.rds")
train_dat <- readRDS("data/train_dat.rds")
train_lab <- readRDS("data/train_lab.rds")

# Final gene list
final_glist <- c(
  "FBN1", "TCF7L1", "CCL5", "FN1", "ADAMDEC1", "CTSK", "COL3A1",
  "CD74", "TIMP3", "POSTN", "CXCL9", "SALL2", "NUAK1", "SLAMF7",
  "CYTIP", "TAP1", "FCER1G", "COL5A1", "ABCC3", "THBS2", "COL1A2",
  "CD3e", "OLFML3", "ADAM12", "CRISPLD2", "INHBA", "GFPT2", "SPARC",
  "FAP", "PDGFRB", "LUM", "CD3D", "VCAN", "EPB41L3", "DCN", "LRRC15",
  "SLAMF8", "CD27", "COL5A2", "LOX", "COL11A1", "CD2", "PDZK1IP1",
  "TMEM45A", "CD38", "HMGA2", "CD8A", "AXL", "CXCL11", "CD68",
  "FGF1", "CSF1R", "TAGLN", "TLR4", "ZNF423"
)

# User interface
ui <- fluidPage(
  # Use CSS theme spacelab from http://bootswatch.com/ (version 3)
  theme = shinythemes::shinytheme("spacelab"),
  # Activate shinyjs functions in UI
  shinyjs::useShinyjs(),

  titlePanel("PrOType Web Tool"),
  sidebarLayout(
    sidebarPanel(
      # Uploads section
      h5(strong("Uploads")),

      # Import reference pool and sample RCC files from a single directory
      fileInput(inputId = "rcc",
                label = "Upload RCC files",
                accept = c(".RCC", ".rcc"),
                multiple = TRUE),

      # Analysis section
      h5(strong("Analysis")),

      # Slider to control SNR
      uiOutput(outputId = "sn"),

      # Button to predict NanoString samples
      actionButton(inputId = "predict", label = "Predict NanoString samples"),
      br(), br(),

      # Downloads section
      h5(strong("Downloads")),

      # Buttons to download QC, data, predictions, report
      downloadButton(outputId = "dl_qc", label = "QC"),

      downloadButton(outputId = "dl_data", label = "Data"),

      downloadButton(outputId = "dl_pred", label = "Predictions"),
      br(), br(),

      uiOutput(outputId = "sample_id"),

      downloadButton(outputId = "dl_report", label = "Report"),
      br(), hr(style = "border-color: black;"),

      # App information
      helpText("© Copyright 2018 OVCARE", br(), "Maintained by Derek Chiu"),
      br(), br(),

      # OVCARE logo and link to website
      a(img(src = "ovcare_logo.png", width = "400"),
        href = "http://www.ovcare.ca/",
        target = "_blank")
    ),

    # Main outputs separated into tabs
    mainPanel(
      tabsetPanel(
        id = "tabset",
        selected = "Help",
        navbarMenu("QC",
                   tabPanel("Plots",
                            plotly::plotlyOutput(outputId = "sn_vs_pergd",
                                                 width = "75%"),
                            br(),
                            plotly::plotlyOutput(outputId = "bd_vs_lod",
                                                 width = "75%")),
                   tabPanel("Table",
                            DT::dataTableOutput(outputId = "qc_table"))),
        tabPanel("Data", DT::dataTableOutput(outputId = "Ynorm")),
        tabPanel("Predictions",
                 DT::dataTableOutput(outputId = "preds")),
        tabPanel("Summary",
                 htmlOutput(outputId = "counts"),
                 tableOutput(outputId = "qc_summary"),
                 tableOutput(outputId = "freqs")),
        tabPanel("Help",
                 h3("Welcome to the PrOType Web Tool!"),
                 p("This app allows you to import RCC files from NanoString runs
                   and perform normalization, compute and visualize quality
                   control metrics, and predict samples with selected genes.
                   The following information is intended to help you use the
                   app's various features."),
                 br(),
                 h4("Upload RCC files"),
                 p("Batch effect correction requires two reference pools. The
                   Vancouver CodeSet 3 reference pools are always used as the
                   first reference. The imported reference pools use the
                   proportion of pools 1-3 in Vancouver CodeSet 3 to compute a
                   weighted average before batch effect correction. Note that
                   these reference pool RCCs must have 'Pool' in their file
                   names."),
                 p("Using your operating system's file explorer/finder, import
                   all reference pool", em("and"), "sample RCC files you wish to
                   analyze. In the current implementation, files cannot be
                   selected from multiple directories. For example, if there are
                   6 reference pools and 10 chips with 12 samples each, all 126
                   RCC files must be placed in a single directory first. Files
                   can then be imported using a 'Select All' and 'Open' in your
                   file chooser dialog box."),
                 p("After import, the data is normalized to housekeeping genes",
                   em("and"),
                   "to the reference pools for batch effect correction and
                   displayed in the", strong("Data"), "tab.
                   Some metadata is displayed in the", strong("Summary"), "tab:
                   the number of normalized genes common to both reference
                   pools, the original total number of genes, and the total
                   number of samples imported."),
                 p("The", icon("download"), code("Data"), "button downloads the
                   normalized data to your local machine."),
                 br(),
                 h4("Quality Control Metrics"),
                 p("NanoString QC metrics are automatically computed after
                   sample RCC files are imported. Two visualizations are
                   displayed under the", strong("QC"), ">", strong("Plots"),
                   "tab: signal to noise ratio vs. percentage of genes detected,
                   binding density vs. limit of detection. Both plots colour
                   points by their respective flag. The plots contain
                   interactive components such as zoom, pan, hover, and
                   download. The QC data metrics are displayed under the",
                   strong("QC"), ">", strong("Table"), "tab."),
                 p("The", icon("download"), code("QC"), "button downloads the
                   QC data to your local machine."),
                 br(),
                 h4("Model Prediction"),
                 p("To predict NanoString samples, click the",
                   code("Predict NanoString samples"), "button and the data will
                   be displayed in the", strong("Predictions"), "tab. A random
                   forest model trained on the genes common in both reference
                   pools is used for prediction. The prediction output shows
                   the predicted class, predicted probabilties for each class,
                   and the associated entropy (log 2 scale)."),
                 p("The", icon("download"), code("Predictions"), "button
                   downloads the prediction data to your local machine."),
                 br(),
                 h4("Patient Reports"),
                 p("Patient-specific summary research reports can be generated
                   in the form of word documents. These reports contain some
                   QC metadata information and the primary prediction
                   result. Multiple reports can be generated depending on
                   samples selected in the respective dropdown menu."),
                 p("The", icon("download"), code("Report"), "button downloads
                   the patient reports compressed into a zip file to your local
                   machine."),
                 br(),
                 h4("Output Summary"),
                 p("The", strong("Summary"), "tab shows a summary of the QC
                   flags: how many samples failed and passed. It also shows the
                   distribution of predicted classes in a table.")
                 )

      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Reference 2: imported pools
  pools_ref2 <- reactive({
    req(input$rcc)
    validate(
      need(any(grepl("Pool", input$rcc$name, ignore.case = TRUE)),
           "No RCC pool files selected")
    )
    pools <- input$rcc %>%
      dplyr::filter(grepl("Pool", name, ignore.case = TRUE)) %>%
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) %>%
      tibble::deframe() %>%
      purrr::map(nanostringr::parse_counts) %>%
      purrr::imap(~ `names<-`(.x, c(names(.x)[-4], .y))) %>%
      purrr::reduce(dplyr::inner_join,
                    by = c("Code.Class", "Name", "Accession")) %>%
      dplyr::mutate(Name = ifelse(Name == "CD3E", "CD3e", Name)) %>%
      purrr::set_names(gsub(" ", "", names(.))) %>%
      purrr::set_names(gsub(".*(Pool.*)_.*", "\\1", names(.))) %>%
      tibble::set_tidy_names(quiet = TRUE)

    # Special renaming system if there are pools indexed by letters
    if (any(grepl("Pool[A-Z]", names(pools)))) {
      pools <- pools %>%
        dplyr::rename_at(
          grep("Pool[A-Z]", names(.)),
          ~ gsub("Pool", "", .) %>% paste0("Pool", match(., LETTERS), .)
        )
    }

    # Check all three pools exist
    validate(
      need(any(grepl("Pool1", names(pools), ignore.case = TRUE)),
           "Missing Pool1 RCC files"),
      need(any(grepl("Pool2", names(pools), ignore.case = TRUE)),
           "Missing Pool2 RCC files"),
      need(any(grepl("Pool3", names(pools), ignore.case = TRUE)),
           "Missing Pool3 RCC files")
    )
    nanostringr::HKnorm(as.data.frame(pools))
  })

  # Read in all RCC chip files and combine count data
  dat <- reactive({
    req(input$rcc)
    validate(
      need(any(!grepl("Pool", input$rcc$name, ignore.case = TRUE)),
           "No RCC sample files selected")
    )
    input$rcc %>%
      dplyr::filter(!grepl("Pool", name, ignore.case = TRUE)) %>%
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) %>%
      tibble::deframe() %>%
      purrr::map(nanostringr::parse_counts) %>%
      purrr::imap(~ `names<-`(.x, c(names(.x)[-4], .y))) %>%
      purrr::reduce(dplyr::inner_join,
                    by = c("Code.Class", "Name", "Accession")) %>%
      dplyr::mutate(Name = ifelse(Name == "CD3E", "CD3e", Name)) %>%
      as.data.frame()
  })

  # Read in all RCC chip files and combine attribute data
  exp <- reactive({
    req(input$rcc)
    input$rcc %>%
      dplyr::filter(!grepl("Pool", name, ignore.case = TRUE)) %>%
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) %>%
      tibble::deframe() %>%
      purrr::map(nanostringr::parse_attributes) %>%
      purrr::imap_dfr(~ magrittr::inset(.x, "File.Name", .y)) %>%
      dplyr::rename(sample = File.Name) %>%
      as.data.frame()
  })

  # Print normalized genes common in references, total genes, total samples
  output$counts <- renderText({
    paste(h4(strong("Data Summary")),
          "Normalized Common Top Genes:", ncol(Ynorm()),
          br(),
          "Total Genes:", nrow(dat()),
          br(),
          "Total Samples:", ncol(dat()) - 3,
          br(), br())
  })

  # Slider to control signal to noise ratio
  output$sn <- renderUI({
    req(input$rcc)
    sliderInput(
      inputId = "sn",
      label = "Signal to Noise Ratio",
      min = 0,
      max = 1000,
      value = 170,
      step = 10
    )
  })

  # NanoString quality control variables
  qc <- reactive({
    req(input$sn)
    nanostringr::NanoStringQC(
      raw = dat(),
      exp = exp(),
      detect = 50,
      sn = input$sn
    ) %>%
      dplyr::mutate_at(c("bdFlag", "normFlag"), as.factor)
  })

  # Normalize to HK genes and correct for BE before prediction
  Ynorm <- reactive({
    req(input$rcc)
    withProgress(message = "Normalizing data", {
      # Normalize data to housekeeping genes
      dat_norm <- nanostringr::HKnorm(dat())

      # Calculate mean gene expression for references
      mR1 <-
        tibble::enframe(rowMeans(pools_ref1), name = "Name", value = "expR1")
      mR2 <-
        weights %>%
        purrr::imap_dfc(~ {
          df <- dplyr::select(pools_ref2(), Name, dplyr::matches(.y)) %>%
            tibble::column_to_rownames("Name")
          tibble::enframe(.x * rowSums(df) / ncol(df), name = "Name", value = .y)
        }) %>%
        dplyr::transmute(
          Name,
          expR2 = rowSums(dplyr::select(., dplyr::contains("Pool")))
        )

      # Combine samples with batch effect (difference in means)
      Y <- dplyr::inner_join(mR1, mR2, by = "Name") %>%
        dplyr::transmute(Name, be = expR1 - expR2) %>%
        dplyr::inner_join(dat_norm, by = "Name")

      # Normalize each gene by adding BE values
      Y %>%
        as.data.frame() %>%
        tibble::column_to_rownames("Name") %>%
        dplyr::select(-c(be, Code.Class, Accession)) %>%
        apply(2, `+`, Y[["be"]]) %>%
        t() %>%
        magrittr::extract(, final_glist, drop = FALSE) %>%
        as.data.frame()
    })
  })

  # Train final model and predict NanoString samples
  dat_preds <- reactive({
    req(input$predict)
    withProgress(message = "Predicting samples", {
      final_model <- splendid::classification(
        data = train_dat[, isolate(colnames(Ynorm())), drop = FALSE],
        class = train_lab,
        algorithms = alg,
        seed = seed
      )
      set.seed(1)
      dat_probs <- predict(final_model, isolate(Ynorm()), type = "prob")
      data.frame(
        dat_probs,
        entropy = apply(dat_probs, 1, entropy::entropy, unit = "log2"),
        pred = as.character(predict(final_model, isolate(Ynorm())))
      ) %>%
        tibble::rownames_to_column("sample") %>%
        tibble::as_tibble()
    })
  })

  # Download normalized data to local CSV file
  output$dl_data <- downloadHandler(
    filename = "normalized_data.csv",
    content = function(file) {
      readr::write_csv(tibble::rownames_to_column(Ynorm(), "sample"), file)
    }
  )

  # Download QC data to local CSV file
  output$dl_qc <- downloadHandler(
    filename = "qc_data.csv",
    content = function(file) {
      readr::write_csv(qc(), file)
    }
  )

  # Download predictions to local CSV file
  output$dl_pred <- downloadHandler(
    filename = "ns_predictions.csv",
    content = function(file) {
      readr::write_csv(dat_preds(), file)
    }
  )

  # Select sample for report of prediction summary
  output$sample_id <- renderUI({
    req(input$rcc)
    selectInput(
      inputId = "sample_id",
      label = "Select sample for report",
      choices = dat_preds()[["sample"]],
      multiple = TRUE
    )
  })

  # Download patient-specific report to local word document
  output$dl_report <- downloadHandler(
    filename = "reports.zip",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      files <- purrr::map_chr(input$sample_id, ~ {
        params <- list(
          qc_data = dplyr::filter(qc(), sample == .),
          pred_data = dplyr::filter(dat_preds(), sample == .)
        )
        rmarkdown::render(
          input = temp_report,
          output_file = paste0("report_", ., ".docx"),
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      })
      zip(zipfile = file, files = files, flags = "-jq")
    },
    contentType = "application/zip"
  )

  # Preview of normalized data as DataTable
  output$Ynorm <- DT::renderDataTable({
    Ynorm()[, 1:6] %>%
      tibble::rownames_to_column("sample") %>%
      DT::datatable(rownames = FALSE,
                    selection = "none",
                    caption = "Preview of normalized data") %>%
      DT::formatRound(columns = seq_along(Ynorm()) + 1, digits = 2)
  })

  # NanoString QC data as DataTable
  output$qc_table <- DT::renderDataTable({
    qc() %>%
      DT::datatable(rownames = FALSE,
                    selection = "none",
                    caption = "Quality Control data") %>%
      DT::formatRound(columns = lapply(qc(), class) == "numeric", digits = 2)
  })

  ## NanoString QC Plots
  # sn vs pergd
  output$sn_vs_pergd <- plotly::renderPlotly({
    p <- ggplot(qc(), aes(sn, pergd,
                          text = paste("sample:", sample),
                          color = normFlag)) +
      geom_point() +
      labs(x = "Signal to Noise Ratio",
           y = "Percentage of Genes Detected",
           title = "SNR vs % GD") +
      theme_bw() +
      scale_color_manual(values = c("red", "green"),
                         drop = FALSE)
    plotly::ggplotly(p)
  })

  # bd vs lod
  output$bd_vs_lod <- plotly::renderPlotly({
    p <- ggplot(qc(), aes(binding.density, lod,
                          text = paste("sample:", sample),
                          color = bdFlag)) +
      geom_point() +
      labs(x = "Binding Density",
           y = "Limit of Detection",
           title = "Binding Density vs LOD") +
      theme_bw() +
      scale_color_manual(values = c("red", "green"),
                         drop = FALSE)
    plotly::ggplotly(p)
  })

  # Predicted probabilities and classes as DataTable
  output$preds <- DT::renderDataTable({
    dat_preds() %>%
      DT::datatable(rownames = FALSE,
                    selection = "none",
                    caption = "Sample predictions and probabilities") %>%
      DT::formatRound(columns = lapply(dat_preds(), class) == "numeric",
                      digits = 3)
  })

  # QC Summary of the flags failed and passed
  output$qc_summary <- renderTable({
    qc() %>%
      dplyr::select(dplyr::matches("Flag")) %>%
      purrr::map(table) %>%
      purrr::invoke(rbind, .) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Flag")
  },
  caption = "QC Summary")

  # Class frequencies
  output$freqs <- renderTable({
    table(dat_preds()[["pred"]]) %>%
      tibble::enframe(name = "Class", value = "Freq")
  },
  caption = "Prediction Frequencies")

  # Enable NanoString prediction when files are imported
  observe({
    shinyjs::toggleState(id = "predict", !is.null(input$rcc))
  })

  # Disable prediction after generated for currently imported files
  shinyjs::onclick("predict", shinyjs::disable(id = "predict"))

  # Enable data download when files are imported
  observe({
    shinyjs::toggleState(id = "dl_data", !is.null(input$rcc))
  })

  # Enable QC download when files are imported
  observe({
    shinyjs::toggleState(id = "dl_qc", !is.null(input$rcc))
  })

  # Enable predictions download when files are imported and predictions clicked
  # and matches currently imported data
  observe({
    shinyjs::toggleState(id = "dl_pred",
                         !is.null(input$rcc) && input$predict &&
                           all(rownames(Ynorm()) == dat_preds()[["sample"]]))
  })

  # Enable report download when patient selected and predictions clicked and
  # matches currently imported data
  observe({
    shinyjs::toggleState(id = "dl_report",
                         !is.null(input$sample_id) && input$predict &&
                           all(rownames(Ynorm()) == dat_preds()[["sample"]]))
  })

  # Button label prompts prediction after import
  observeEvent(input$rcc, {
    updateActionButton(session, "predict", label = "Predict NanoString samples")
  })

  # Button label states predictions generated
  observeEvent(input$predict, {
    updateActionButton(session, "predict", label = "Predictions Generated!")
  })

  # Switch to QC Plots tab when raw data has been imported
  observeEvent(input$rcc, {
    updateTabsetPanel(session, "tabset", selected = "Plots")
  })

  # Switch to Predictions tab when predict button is clicked
  observeEvent(input$predict, {
    updateTabsetPanel(session, "tabset", selected = "Predictions")
  })

  # Show source code below app by default
  shinyjs::runjs("toggleCodePosition();")
}

# Run the application
shinyApp(ui = ui, server = server)
