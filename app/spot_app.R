# Load packages, pipe, and relevant constants
library(shiny)
library(randomForest)
library(ggplot2)
library(workflows)
library(ranger)
weights <- purrr::set_names(c(12, 5, 5) / 22, c("Pool1", "Pool2", "Pool3"))
th_pds_adnexal <- c(-0.86697992, -0.37336052, -0.07486426, 0.20987383)
th_pds_omentum <- c(-0.22398146, -0.02006981, 0.21263632, 0.41430278)
th_nact <- c(-0.32177400, -0.08815859, 0.18609397, 0.40834019)
options(shiny.maxRequestSize = 10 * 1024^2)

# Load Vancouver CS3 pools (ref 1), final model, and final gene list
pools_ref1 <- readRDS("data/van_pools_cs3.rds")
final_model <- readRDS("data/final_model.rds")
coefmat <- readRDS("data/coefmat.rds")
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

# Pattern to match control pool files
pool_regexp <- ".*Pool *[0-9]{1,2}_[0-9]{2}\\.RCC"

# User interface
ui <- fluidPage(
  # Use CSS theme spacelab from http://bootswatch.com/ (version 3)
  theme = shinythemes::shinytheme("spacelab"),
  # Activate shinyjs functions in UI
  shinyjs::useShinyjs(),

  titlePanel("SPOT Prediction Web Tool"),
  sidebarLayout(
    sidebarPanel(
      # Uploads section
      h5(strong("Uploads")),

      # Import reference pool and sample RCC files from a single directory
      fileInput(inputId = "rcc",
                label = "Upload RCC files",
                accept = c(".RCC", ".rcc"),
                multiple = TRUE),

      # Import SPOT input
      fileInput(inputId = "spot",
                label = "Upload SPOT input",
                accept = ".csv"),

      # Analysis section
      h5(strong("Analysis")),

      # Slider to control SNR
      uiOutput(outputId = "sn"),

      # Button to predict SPOT
      actionButton(inputId = "predict", label = "Predict SPOT"),
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
      helpText("© Copyright 2025 OVCARE", br(), "Maintained by Derek Chiu"),
      downloadLink(outputId = "dl_code", label = "Source Code"),
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
        navbarMenu(
          "QC",
          tabPanel(
            "Plots",
            plotly::plotlyOutput(outputId = "sn_vs_pergd", width = "75%"),
            br(),
            plotly::plotlyOutput(outputId = "bd_vs_lod", width = "75%")
          ),
          tabPanel("Table", DT::dataTableOutput(outputId = "qc_table"))
        ),
        tabPanel("Data", DT::dataTableOutput(outputId = "Ynorm")),
        tabPanel(
          "Predictions",
          DT::dataTableOutput(outputId = "preds"),
          br(),
          DT::dataTableOutput(outputId = "ov_preds")
        ),
        tabPanel(
          "Summary",
          htmlOutput(outputId = "counts"),
          tableOutput(outputId = "qc_summary"),
          tableOutput(outputId = "spot_freqs"),
          htmlOutput(outputId = "spot_genes"),
          br(),
          htmlOutput(outputId = "spot_unused_genes")
        ),
        tabPanel(
          "Help",
          h3("Welcome to the SPOT Prediction Web Tool!"),
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
          these reference pool RCCs must have 'Pool' in their file names."),
          p("Using your operating system's file explorer/finder, import
          all reference pool", em("and"), "sample RCC files you wish to
          analyze. In the current implementation, files cannot be
          selected from multiple directories. For example, if there are
          6 reference pools and 10 chips with 12 samples each, all 126
          RCC files must be placed in a single directory first. Files
          can then be imported using a 'Select All' and 'Open' in your
          file chooser dialog box."),
          p("After import, the data is normalized to housekeeping genes",
            em("and"), "to the reference pools for batch effect correction and
            displayed in the", strong("Data"), "tab. Some metadata is displayed
            in the", strong("Summary"), "tab: the number of normalized genes
            common to both reference pools, the original total number of genes,
            and the total number of samples imported."),
          p("The", icon("download"), code("Data"), "button downloads the
            normalized data to your local machine."),
          br(),
          h4("SPOT Prediction"),
          p("Additionally, we can add a SPOT prediction to the NanoString samples,
          provided that we input a SPOT design matrix with clinical covariates
          age (in quartiles) and stage (low/high), and other characteristics such as
          cancer site and treatment type. There are coefficients for
          101 genes, 3 contrasts for age, and 2 contrasts for stage. The intersection of
          these genes and covariates with the imported NanoString samples will be used
          to generate a SPOT signature, a continuous score calculated using
          a linear combination of the coefficients and normalized expression.
          A quintile breakdown of the numeric signature is also reported."),
          p("Import a SPOT design matrix input by checking off 'Add SPOT Prediction' to reveal the file upload control icon.
          Only CSV files are allowed. Genes used in the SPOT signature are detailed in the", strong("Summary"), "tab."),
          br(),
          h4("Ovarian Histotype Prediction"),
          p("If we have a separate set of NanoString files that arise from all ovarian histotypes,
          and not just HGSC, we can perform histotype prediction to classify samples into one of the 5
          major histotypes: HGSC, CCOC, ENOC, MUC, and LGSC. The final model used has already been trained
            on an external training set."),
          p("Import a data set with new predictors to predict on by checking off 'Add Ovarian Histotypes Prediction'.
            Only CSV files are allowed. Click the", code("Predict Ovarian Histotype samples"),
            "button and results will be shown on the", strong("Predictions"), "tab."),
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
          p("The", icon("download"), code("Predictions"), "button downloads the
          prediction data to your local machine."),
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
    pools_raw <- input$rcc |>
      dplyr::filter(grepl(pool_regexp, name, ignore.case = TRUE)) |>
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) |>
      tibble::deframe() |>
      purrr::map(nanostringr::parse_counts) |>
      purrr::imap(~ dplyr::rename_with(.x, rlang::quo(.y), 4)) |>
      purrr::reduce(\(x, y) dplyr::inner_join(x, y, by = c("Code.Class", "Name", "Accession"))) |>
      dplyr::mutate(Name = dplyr::case_match(
        Name,
        "CD3E" ~ "CD3e",
        "PD1" ~ "PD-1",
        "PDL1" ~ "PD-L1",
        .default = Name
      ))

    # Special renaming system for pool files with spaces and indexing by letters
    pools_raw <- pools_raw |>
      rlang::set_names(~ gsub(".*(Pool)[[:space:]]*(.+)_.*", "\\1\\2", .,
                              ignore.case = TRUE)) |>
      tibble::set_tidy_names(quiet = TRUE) |>
      dplyr::rename_with(\(x) {
        letter <- gsub("Pool([A-Z])", "\\1", x)
        paste0("Pool", match(letter, LETTERS), letter, recycle0 = TRUE)
      }, matches("Pool[A-Z]"))

    # Check all three pools exist
    validate(
      need(any(grepl("Pool1", names(pools_raw), ignore.case = TRUE)),
           "Missing Pool1 RCC files"),
      need(any(grepl("Pool2", names(pools_raw), ignore.case = TRUE)),
           "Missing Pool2 RCC files"),
      need(any(grepl("Pool3", names(pools_raw), ignore.case = TRUE)),
           "Missing Pool3 RCC files")
    )

    # Pools expression data
    pools_exp <- input$rcc |>
      dplyr::filter(grepl(pool_regexp, name, ignore.case = TRUE)) |>
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) |>
      tibble::deframe() |>
      purrr::map(nanostringr::parse_attributes) |>
      dplyr::bind_rows(.id = "sample") |>
      dplyr::select(-File.Name)

    # Check all pools pass QC
    pools_qc <- nanostringr::NanoStringQC(
      raw = pools_raw,
      exp = pools_exp,
      detect = 50,
      sn = input$sn
    )
    validate(
      need(all(pools_qc[["QCFlag"]] == "Passed"),
           "Some pools failed QC. Normalization failed.")
    )

    nanostringr::HKnorm(pools_raw)
  })

  # Read in all RCC chip files and combine count data
  dat <- reactive({
    req(input$rcc)
    validate(
      need(any(!grepl("Pool", input$rcc$name, ignore.case = TRUE)),
           "No RCC sample files selected")
    )
    input$rcc |>
      dplyr::filter(!grepl(pool_regexp, name, ignore.case = TRUE)) |>
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) |>
      tibble::deframe() |>
      purrr::map(nanostringr::parse_counts) |>
      purrr::imap(~ dplyr::rename_with(.x, rlang::quo(.y), 4)) |>
      purrr::reduce(\(x, y) dplyr::inner_join(x, y, by = c("Code.Class", "Name", "Accession"))) |>
      dplyr::mutate(Name = dplyr::case_match(
        Name,
        "CD3E" ~ "CD3e",
        "PD1" ~ "PD-1",
        "PDL1" ~ "PD-L1",
        .default = Name
      ))
  })

  # Read in all RCC chip files and combine attribute data
  exp <- reactive({
    req(input$rcc)
    input$rcc |>
      dplyr::filter(!grepl(pool_regexp, name, ignore.case = TRUE)) |>
      dplyr::transmute(name = tools::file_path_sans_ext(name), datapath) |>
      tibble::deframe() |>
      purrr::map(nanostringr::parse_attributes) |>
      dplyr::bind_rows(.id = "sample") |>
      dplyr::select(-File.Name)
  })

  # Print normalized genes common in references, total genes, total samples
  output$counts <- renderText({
    if (!is.null(input$rcc)) {
      paste(h4(strong("Data Summary")),
            "Normalized Common Top Genes:", ncol(Ynorm()),
            br(),
            "Total Genes:", nrow(dat()),
            br(),
            "Total Samples:", ncol(dat()) - 3,
            br(), br())
    }
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
    ) |>
      dplyr::mutate_at(c("bdFlag", "normFlag"), as.factor)
  })

  # Normalize to HK genes and correct for BE before prediction
  Ynorm <- reactive({
    req(input$rcc)
    withProgress(message = "Normalizing data", {
      # Normalize data to housekeeping genes
      dat_norm <- nanostringr::HKnorm(dat())

      # Normalize data to pools
      nanostringr::normalize_pools(
        x = dat_norm,
        x_pools = pools_ref2(),
        ref_pools = pools_ref1,
        p = 3
      ) |>
        tibble::column_to_rownames("FileName")
    })
  })

  # SPOT input joined with normalized data
  spot_Ynorm <- reactive({
    req(input$spot)
    if (!is.null(input$spot)) {
      input$spot$datapath |>
        readr::read_csv(col_types = readr::cols()) |>
        dplyr::inner_join(tibble::rownames_to_column(isolate(Ynorm()), "sample"),
                          by = "sample")
    }
  })

  # Predict SPOT
  dat_preds <- reactive({
    req(input$predict)
    withProgress(message = "Predicting samples", {
      # Create SPOT predictions and quintiles
      if (!is.null(input$spot)) {
        req(input$spot)
        spot_df <- spot_Ynorm() |>
          dplyr::mutate(
            age.brks = santoku::chop(
              x = age,
              breaks = c(53, 60, 67),
              labels = c("q1", "q2", "q3", "q4"),
              extend = TRUE
            ),
            age.fq2 = ifelse(age.brks == "q2", 1, 0),
            age.fq3 = ifelse(age.brks == "q3", 1, 0),
            age.fq4 = ifelse(age.brks == "q4", 1, 0),
            stage.f1 = ifelse(stage %in% c(
              "I", "IA", "IB", "IC", "II", "IIA", "IIB", "IIC"
            ), 1, 0),
            stage.f8 = ifelse(is.na(stage), 1, 0),
            .keep = "unused",
            .after = treatment
          ) |>
          tidyr::pivot_longer(
            cols = dplyr::where(is.numeric),
            names_to = "Symbol",
            values_to = "Expression"
          ) |>
          dplyr::left_join(coefmat, by = "Symbol") |>
          dplyr::mutate(SPOT_pred = sum(Expression * Coefficient, na.rm = TRUE),
                        .by = sample) |>
          tidyr::pivot_wider(
            id_cols = c(dplyr::where(is.character), "SPOT_pred"),
            names_from = Symbol,
            values_from = Expression
          ) |>
          dplyr::mutate(
            sample,
            SPOT_quintile = dplyr::case_when(
              treatment == "primary" & site == "adnexal" ~ santoku::chop(
                SPOT_pred,
                breaks = th_pds_adnexal,
                labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                left = FALSE,
                extend = TRUE
              ),
              treatment == "primary" &
                site == "omentum" ~ santoku::chop(
                  SPOT_pred,
                  breaks = th_pds_omentum,
                  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                  left = FALSE,
                  extend = TRUE
                ),
              treatment == "post-NACT" ~ santoku::chop(
                SPOT_pred,
                breaks = th_nact,
                labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                left = FALSE,
                extend = TRUE
              )
            ),
            .keep = "used"
          )
        spot_df
      }
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
      temp_report <- file.path(tempdir(), "spot_report.Rmd")
      file.copy("spot_report.Rmd", temp_report, overwrite = TRUE)
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

  # Download source code
  output$dl_code <- downloadHandler(
    filename = "app.R",
    content = function(file) {
      file.copy("app.R", file)
    },
    contentType = "application/zip"
  )

  # Normalized data as DataTable
  output$Ynorm <- DT::renderDataTable({
    Ynorm() |>
      tibble::rownames_to_column("sample") |>
      DT::datatable(
        rownames = FALSE,
        selection = "none",
        caption = "Normalized data",
        extensions = "FixedColumns",
        options = list(scrollX = TRUE, fixedColumns = TRUE)
      ) |>
      DT::formatRound(columns = seq_along(Ynorm()) + 1, digits = 2) |>
      DT::formatStyle("sample", "white-space" = "nowrap")
  })

  # NanoString QC data as DataTable
  output$qc_table <- DT::renderDataTable({
    qc() |>
      DT::datatable(rownames = FALSE,
                    selection = "none",
                    caption = "Quality Control data") |>
      DT::formatRound(columns = lapply(qc(), class) == "numeric", digits = 2)
  })

  ## NanoString QC Plots
  # sn vs pergd
  output$sn_vs_pergd <- plotly::renderPlotly({
    p <- ggplot(qc(), aes(snr, pergd,
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
    dat_preds() |>
      DT::datatable(
        rownames = FALSE,
        selection = "none",
        caption = "Sample predictions and probabilities",
        extensions = "FixedColumns",
        options = list(scrollX = TRUE, fixedColumns = TRUE)
      ) |>
      DT::formatRound(columns = lapply(dat_preds(), class) == "numeric",
                      digits = 3) |>
      DT::formatStyle("sample", "white-space" = "nowrap")
  })

  # QC Summary of the flags failed and passed
  output$qc_summary <- renderTable({
    if (!is.null(input$rcc)) {
      qc() |>
        dplyr::select(dplyr::matches("Flag")) |>
        purrr::map(table) |>
        do.call(rbind, args = _) |>
        as.data.frame() |>
        tibble::rownames_to_column("Flag")
    }
  },
  caption = "QC Summary")

  # SPOT quintile frequencies
  output$spot_freqs <- renderTable({
    req(input$spot, input$predict)
    dat_preds()[["SPOT_quintile"]] |>
      table() |>
      tibble::enframe(name = "Quintile", value = "Frequency")
  },
  caption = "SPOT Quintile Frequencies")

  # Genes used for SPOT prediction
  output$spot_genes <- renderText({
    req(input$spot, input$predict)
    if (!is.null(input$spot)) {
      intersect(coefmat$Symbol, names(spot_Ynorm())) |>
        grep(pattern = "age|stage", x = _, value = TRUE, invert = TRUE) |>
        paste(collapse = ", ") |>
        paste("Genes <font color=\"#008000\">used</font> for SPOT prediction:", ... = _)
    }
  })

  # Genes not used for SPOT prediction
  output$spot_unused_genes <- renderText({
    req(input$spot, input$predict)
    if (!is.null(input$spot)) {
      setdiff(coefmat$Symbol, names(spot_Ynorm())) |>
        grep(pattern = "age|stage", x = _, value = TRUE, invert = TRUE) |>
        paste(collapse = ", ") |>
        paste("Genes <font color=\"#FF0000\">not used</font> for SPOT prediction:", ... = _)
    }
  })

  # Enable NanoString prediction when files are imported (at least RCC needed)
  observe({
    shinyjs::toggleState(id = "predict", (!is.null(input$rcc) & !is.null(input$spot)))
  })

  # Enable ovarian histotype prediction when CSV file is imported
  observe({
    shinyjs::toggleState(id = "histotype_predict", !is.null(input$histotypes))
  })

  # Disable prediction after generated for currently imported files
  shinyjs::onclick(id = "predict", shinyjs::disable(id = "predict"))

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
                           all(Ynorm()[["sample"]]== dat_preds()[["sample"]]))
  })

  # Enable report download when patient selected and predictions clicked and
  # matches currently imported data
  observe({
    shinyjs::toggleState(id = "dl_report",
                         !is.null(input$sample_id) && input$predict &&
                          all(Ynorm()[["sample"]] == dat_preds()[["sample"]])
                         )
  })

  # Data Import: Prompt prediction and switch to QC Plots tab when raw data has been imported
  observeEvent(input$rcc, {
    updateActionButton(session, "predict", label = "Predict SPOT")
    updateTabsetPanel(session, "tabset", selected = "Plots")
  })

  # SPOT Import: Prompt prediction after SPOT inputted
  observeEvent(input$spot, {
    updateActionButton(session, "predict", label = "Predict SPOT")
  })

  # Subtype Prediction button: update button text and switch to Predictions tab when clicked
  observeEvent(input$predict, {
    updateActionButton(session, "predict", label = "Predictions Generated!")
    updateTabsetPanel(session, "tabset", selected = "Predictions")
  })

  # Ovarian Histotypes Prediction button: update button text and switch to Predictions tab when clicked
  observeEvent(input$histotype_predict, {
    updateActionButton(session, "histotype_predict", label = "Ovarian Histotype Predictions Generated!")
    updateTabsetPanel(session, "tabset", selected = "Predictions")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
