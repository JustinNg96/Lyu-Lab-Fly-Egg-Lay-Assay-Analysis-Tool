library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(multcompView)

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

read_pasted_table <- function(txt) {
  if (nchar(trimws(txt)) == 0) return(NULL)
  con <- textConnection(txt)
  on.exit(close(con), add = TRUE)
  read.table(
    con,
    header = TRUE,
    sep = "\t",
    check.names = TRUE,
    stringsAsFactors = FALSE
  )
}

parse_shade_x <- function(txt) {
  txt <- trimws(txt)
  if (!nzchar(txt)) return(list(type = "none"))
  
  if (grepl("-", txt, fixed = TRUE)) {
    parts <- unlist(strsplit(txt, "[;,]"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    
    ranges <- lapply(parts, function(p) {
      ab <- strsplit(p, "-", fixed = TRUE)[[1]]
      if (length(ab) != 2) return(NULL)
      a <- suppressWarnings(as.numeric(trimws(ab[1])))
      b <- suppressWarnings(as.numeric(trimws(ab[2])))
      if (is.na(a) || is.na(b)) return(NULL)
      c(min(a, b), max(a, b))
    })
    
    ranges <- Filter(Negate(is.null), ranges)
    if (length(ranges) == 0) return(list(type = "none"))
    return(list(type = "ranges", ranges = ranges))
  }
  
  vals <- trimws(strsplit(txt, ",", fixed = TRUE)[[1]])
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(list(type = "none"))
  list(type = "values", values = vals)
}

parse_exclude_ids <- function(txt) {
  txt <- trimws(txt)
  if (!nzchar(txt)) return(integer(0))
  
  parts <- unlist(strsplit(txt, "[,;\\s]+"))
  parts <- parts[nzchar(parts)]
  
  out <- integer(0)
  for (p in parts) {
    if (grepl("-", p, fixed = TRUE)) {
      ab <- strsplit(p, "-", fixed = TRUE)[[1]]
      if (length(ab) == 2) {
        a <- suppressWarnings(as.integer(trimws(ab[1])))
        b <- suppressWarnings(as.integer(trimws(ab[2])))
        if (!is.na(a) && !is.na(b)) {
          out <- c(out, seq.int(min(a, b), max(a, b)))
        }
      }
    } else {
      v <- suppressWarnings(as.integer(p))
      if (!is.na(v)) out <- c(out, v)
    }
  }
  
  unique(out)
}

ui <- fluidPage(
  titlePanel("Plot Builder (Pivot-Style ggplot GUI)"),
  
  tags$style(HTML("\
    body { overflow-y: hidden; }\
    .container-fluid { height: calc(100vh - 80px); }\
    .well { max-height: calc(100vh - 120px); overflow-y: auto; }\
    #plotpane {\
      height: calc(100vh - 140px);\
      overflow-y: auto;\
      padding-right: 12px;\
      border-left: 1px solid #eee;\
    }\
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Load data"),
      fileInput("file", "Upload Excel (.xlsx)", accept = ".xlsx"),
      uiOutput("sheet_ui"),
      textAreaInput("paste", "OR paste table", rows = 5),
      actionButton("load_paste", "Load pasted data"),
      
      tags$hr(),
      h4("2) Map fields"),
      uiOutput("selectors"),
      
      tags$hr(),
      h4("Experiment ID selection"),
      uiOutput("exclude_selectors"),
      textInput("keep_ids", "Keep only these IDs (e.g., 1,2,5-10)", value = ""),
      
      tags$hr(),
      h4("Titles"),
      textInput("title_custom", "Title"),
      textInput("subtitle_custom", "Subtitle"),
      
      tags$hr(),
      h4("Axis labels"),
      textInput("xlabel_custom", "X label (blank = default)", value = "Hours"),
      textInput("ylabel_custom", "Y label (blank = default)", value = "Egg Lays per Fly"),
      
      tags$hr(),
      h4("Fill colors"),
      checkboxInput("custom_fill_colors", "Use manual fill colors", TRUE),
      conditionalPanel(
        "input.custom_fill_colors",
        textInput("fill_colors", "Comma-separated colors", value = "#B3B3B3,#FDD49E,#FC8D59,#B25751")
      ),
      
      tags$hr(),
      h4("Reorder fill levels"),
      uiOutput("fill_level_selector_ui"),
      
      tags$hr(),
      h4("Plot size"),
      sliderInput("plot_width", "Width (in)", 4, 20, 10),
      sliderInput("plot_height", "Height (in)", 3, 15, 6),
      sliderInput("text_scale", "Text size scale", 0.6, 10, 2, step = 0.1),
      
      tags$hr(),
      h4("Split panels"),
      uiOutput("split_selector"),
      checkboxInput("split_free_y", "Free y-scale per panel", TRUE),
      
      h4("Filtering"),
      sliderInput("n_filters", "How many filters?", min = 0, max = 6, value = 0, step = 1),
      uiOutput("dynamic_filters"),
      
      tags$hr(),
      h4("Geoms"),
      checkboxInput("show_boxplot", "Boxplot", TRUE),
      checkboxInput("show_jitter", "Jitter", TRUE),
      sliderInput("jitter_width", "Jitter width", 0, 1, 0.2),
      sliderInput("dodge_width", "Dodge width", 0, 2, 0.75, step = 0.05),
      
      tags$hr(),
      h4("Background shading (X)"),
      textInput("shade_x", "Shade x (levels or ranges)", placeholder = "Examples: 1,2,3  OR  1-3;5-6"),
      sliderInput("shade_alpha", "Shade alpha", 0, 1, 0.2, step = 0.05),
      textInput("shade_label", "Shade label", value = "Before Mating"),
      
      tags$hr(),
      numericInput("ymin", "Y min", 0),
      numericInput("ymax", "Y max", 30),
      
      h4("Stats (Assumptions → ANOVA → Tukey)"),
      checkboxInput("do_stats", "Enable stats", FALSE),
      
      conditionalPanel(
        "input.do_stats",
        h5("Assumption checks (recommended)"),
        checkboxInput("check_assumptions", "Run assumption checks", TRUE),
        checkboxInput("assump_show_groups", "Check variance across groups (Fligner/Bartlett)", TRUE),
        
        tags$hr(),
        h5("ANOVA settings"),
        checkboxInput("do_anova", "Run ANOVA", TRUE),
        sliderInput("anova_n_factors", "How many factors?", min = 1, max = 6, value = 1, step = 1),
        sliderInput("anova_n_blocks", "How many blocks? (optional)", min = 0, max = 6, value = 0, step = 1),
        uiOutput("anova_selectors_dynamic"),
        
        selectInput(
          "anova_interaction_mode",
          "Interactions",
          choices = c("None" = "none", "All 2-way" = "two_way", "Full (A*B*C...)" = "full"),
          selected = "two_way"
        ),
        
        tags$hr(),
        checkboxInput("do_tukey", "Run Tukey HSD", FALSE),
        conditionalPanel(
          "input.do_tukey",
          uiOutput("tukey_term_ui"),
          checkboxInput("show_tukey_letters", "Add Tukey letters to plot (A/B/AB)", TRUE),
          checkboxInput("tukey_sig_only", "Show significant only (p adj < 0.05)", TRUE),
          numericInput("tukey_alpha", "Alpha", value = 0.05, min = 0.0001, max = 0.2, step = 0.01)
        )
      ),
      
      tags$hr(),
      downloadButton("download_plot", "Download PNG")
    ),
    
    mainPanel(
      div(id = "plotpane", uiOutput("plot_ui"))
    )
  )
)

server <- function(input, output, session) {
  default_fill_order <- c("OO", "YO", "OY", "YY")
  
  rv <- reactiveValues(df = NULL, file_path = NULL, sheets = NULL)
  rv_fill_levels <- reactiveVal(NULL)
  rv_anova <- reactiveValues(factors = list(), blocks = list())
  rv_filters <- reactiveValues(cols = list(), modes = list(), vals = list())
  
  observeEvent(input$file, {
    req(input$file)
    rv$file_path <- input$file$datapath
    rv$sheets <- tryCatch(readxl::excel_sheets(rv$file_path), error = function(e) NULL)
  })
  
  observeEvent(list(rv$file_path, input$sheet), {
    req(rv$file_path)
    req(rv$sheets)
    
    sheet_to_use <- input$sheet
    if (is.null(sheet_to_use) || !nzchar(sheet_to_use) || !(sheet_to_use %in% rv$sheets)) {
      sheet_to_use <- rv$sheets[1]
      updateSelectInput(session, "sheet", selected = sheet_to_use)
    }
    
    rv$df <- clean_names(as.data.frame(read_excel(rv$file_path, sheet = sheet_to_use)))
  })
  
  observeEvent(input$load_paste, {
    df <- read_pasted_table(input$paste)
    if (is.null(df)) return()
    rv$df <- clean_names(df)
  })
  
  observeEvent(input$n_filters, {
    nF <- input$n_filters %||% 0
    if (nF <= 0) {
      rv_filters$cols <- list()
      rv_filters$modes <- list()
      rv_filters$vals <- list()
      return()
    }
    
    # Extend lists up to nF (without breaking existing choices)
    while (length(rv_filters$cols) < nF)  rv_filters$cols[[length(rv_filters$cols) + 1]] <- NULL
    while (length(rv_filters$modes) < nF) rv_filters$modes[[length(rv_filters$modes) + 1]] <- "keep"
    while (length(rv_filters$vals) < nF)  rv_filters$vals[[length(rv_filters$vals) + 1]] <- character(0)
    
    # Truncate if user reduces n_filters
    rv_filters$cols  <- rv_filters$cols[seq_len(nF)]
    rv_filters$modes <- rv_filters$modes[seq_len(nF)]
    rv_filters$vals  <- rv_filters$vals[seq_len(nF)]
  }, ignoreInit = TRUE)
  
  output$selectors <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    
    pick <- function(preferred, fallback = 1) {
      if (preferred %in% cols) preferred else cols[min(fallback, length(cols))]
    }
    
    keep_or <- function(current, fallback) {
      if (!is.null(current) && nzchar(current) && current %in% cols) current else fallback
    }
    
    keep_or_blank <- function(current) {
      if (!is.null(current) && current %in% c("", cols)) current else ""
    }
    
    tagList(
      selectInput("xcol", "X", cols, selected = keep_or(input$xcol, pick("day", 1))),
      selectInput("ycol", "Y", cols, selected = keep_or(input$ycol, pick("eggs_laid_per_fly", 2))),
      selectInput("fillcol", "Fill", c("None" = "", cols), selected = {
        cur <- keep_or_blank(input$fillcol)
        if (nzchar(cur)) cur else if ("group" %in% cols) "group" else ""
      }),
      selectInput("shapecol", "Shape", c("None" = "", cols), selected = keep_or_blank(input$shapecol)),
      selectInput("facet_row", "Facet row", c("None" = "", cols), selected = keep_or_blank(input$facet_row)),
      selectInput("facet_col", "Facet col", c("None" = "", cols), selected = keep_or_blank(input$facet_col))
    )
  })
  
  output$sheet_ui <- renderUI({
    req(input$file)
    req(rv$sheets)
    selectInput("sheet", "Excel sheet", choices = rv$sheets, selected = rv$sheets[1])
  })
  
  output$exclude_selectors <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    
    selected_id <- ""
    if (!is.null(input$idcol) && nzchar(input$idcol) && input$idcol %in% cols) {
      selected_id <- input$idcol
    } else if ("exp_id" %in% cols) {
      selected_id <- "exp_id"
    }
    
    selectInput("idcol", "ID column", c("None" = "", cols), selected = selected_id)
  })
  
  output$split_selector <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    
    cur <- if (!is.null(input$split_col) && input$split_col %in% cols) input$split_col else ""
    selectInput("split_col", "Split plots by (optional)", choices = c("None" = "", cols), selected = cur)
  })
  
  output$fill_level_selector_ui <- renderUI({
    req(rv$df)
    
    if (is.null(input$fillcol) || !nzchar(input$fillcol) || !(input$fillcol %in% names(rv$df))) {
      return(helpText("Pick a Fill column first to choose included levels and order."))
    }
    
    levs <- unique(trimws(as.character(df_work()[[input$fillcol]])))
    levs <- levs[!is.na(levs) & nzchar(levs)]
    
    if (length(levs) == 0) {
      return(helpText("No fill levels available after current filters."))
    }
    
    preferred <- if (all(default_fill_order %in% levs)) {
      c(default_fill_order, setdiff(levs, default_fill_order))
    } else {
      levs
    }
    
    cur <- rv_fill_levels()
    selected <- if (!is.null(cur) && length(cur) > 0) {
      kept <- cur[cur %in% preferred]
      if (length(kept) > 0) kept else preferred
    } else {
      preferred
    }
    
    tagList(
      helpText("Select levels to include. Drag selected items to set plotting order."),
      selectizeInput(
        "fill_levels_selected",
        "Included fill levels (ordered)",
        choices = preferred,
        selected = selected,
        multiple = TRUE,
        options = list(plugins = list("drag_drop"), closeAfterSelect = FALSE)
      )
    )
  })
  
  get_anova_factors <- function() {
    nF <- input$anova_n_factors %||% 1
    out <- character(0)
    for (i in seq_len(nF)) {
      v <- input[[paste0("anova_factor_", i)]]
      if (!is.null(v) && nzchar(v)) out <- c(out, v)
    }
    unique(out)
  }
  
  build_anova_formula <- function(df) {
    cols <- names(df)
    req(!is.null(input$anova_y), input$anova_y %in% cols)
    
    y <- input$anova_y
    factors <- get_anova_factors()
    if (length(factors) < 1) return(NULL)
    
    nB <- input$anova_n_blocks %||% 0
    blocks <- character(0)
    if (nB > 0) {
      for (i in seq_len(nB)) {
        b <- input[[paste0("anova_block_", i)]]
        if (!is.null(b) && nzchar(b) && b %in% cols) blocks <- c(blocks, b)
      }
      blocks <- unique(blocks)
    }
    
    interaction_mode <- input$anova_interaction_mode %||% "two_way"
    if (interaction_mode == "none" || length(factors) == 1) {
      rhs <- paste(factors, collapse = " + ")
    } else if (interaction_mode == "full") {
      rhs <- paste(factors, collapse = " * ")
    } else {
      main <- paste(factors, collapse = " + ")
      pair_terms <- combn(factors, 2, FUN = function(x) paste0(x[1], ":", x[2]))
      rhs <- paste(c(main, pair_terms), collapse = " + ")
    }
    
    if (length(blocks) > 0) rhs <- paste(rhs, "+", paste(blocks, collapse = " + "))
    as.formula(paste(y, "~", rhs))
  }
  
  output$anova_selectors_dynamic <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    
    safe_get <- function(x, i) {
      if (is.null(x) || length(x) < i) return(NULL)
      x[[i]]
    }
    
    y_default <- if (!is.null(input$ycol) && input$ycol %in% cols) input$ycol else cols[1]
    
    guess_factor <- function() {
      for (nm in c("condition", "conditions", "group", "exp_group")) {
        if (nm %in% cols) return(nm)
      }
      cols[1]
    }
    
    nF <- input$anova_n_factors %||% 1
    nB <- input$anova_n_blocks %||% 0
    
    factor_pick <- lapply(seq_len(nF), function(i) {
      id <- paste0("anova_factor_", i)
      mem <- safe_get(rv_anova$factors, i)
      default_guess <- if (i == 1) guess_factor() else ""
      default <- if (!is.null(mem) && mem %in% c("", cols)) mem else default_guess
      selectInput(id, paste0("Factor ", LETTERS[i]), choices = c("None" = "", cols), selected = default)
    })
    
    block_pick <- lapply(seq_len(nB), function(i) {
      id <- paste0("anova_block_", i)
      mem <- safe_get(rv_anova$blocks, i)
      default_guess <- if (i == 1 && "exp_id" %in% cols) "exp_id" else ""
      default <- if (!is.null(mem) && mem %in% c("", cols)) mem else default_guess
      selectInput(id, paste0("Block ", i), choices = c("None" = "", cols), selected = default)
    })
    
    tagList(
      selectInput("anova_y", "Response (Y)", choices = cols, selected = y_default),
      tags$hr(),
      factor_pick,
      if (nB > 0) tags$hr() else NULL,
      block_pick
    )
  })
  
  observe({
    req(rv$df)
    nF <- input$anova_n_factors %||% 1
    for (i in seq_len(nF)) {
      v <- input[[paste0("anova_factor_", i)]]
      if (!is.null(v)) rv_anova$factors[[i]] <- v
    }
  })
  
  observe({
    req(rv$df)
    nB <- input$anova_n_blocks %||% 0
    if (nB == 0) return()
    for (i in seq_len(nB)) {
      v <- input[[paste0("anova_block_", i)]]
      if (!is.null(v)) rv_anova$blocks[[i]] <- v
    }
  })
  
  get_tukey_terms_from_fit <- function(fit) {
    out <- tryCatch(names(TukeyHSD(fit)), error = function(e) character(0))
    out[!is.na(out) & nzchar(out)]
  }
  
  tukey_letters_from_raw <- function(tuk0, term, alpha = 0.05) {
    if (is.null(tuk0) || is.null(term) || !nzchar(term) || !(term %in% names(tuk0))) return(NULL)
    
    mat <- as.data.frame(tuk0[[term]])
    if (!("p adj" %in% names(mat))) return(NULL)
    
    p <- mat[["p adj"]]
    names(p) <- rownames(mat)
    letters <- multcompView::multcompLetters(p, threshold = alpha)$Letters
    data.frame(level = names(letters), letter = unname(letters), stringsAsFactors = FALSE)
  }
  
  df_work <- reactive({
    req(rv$df)
    df <- rv$df
    
    nF <- input$n_filters
    if (!is.null(nF) && nF > 0) {
      for (i in seq_len(nF)) {
        col <- input[[paste0("filter_col_", i)]]
        mode <- input[[paste0("filter_mode_", i)]]
        vals <- input[[paste0("filter_val_", i)]]
        
        if (!is.null(col) && col %in% names(df) && !is.null(vals) && length(vals) > 0) {
          if (mode == "keep") df <- df %>% filter(as.character(.data[[col]]) %in% vals)
          if (mode == "remove") df <- df %>% filter(!as.character(.data[[col]]) %in% vals)
        }
      }
    }
    
    if (!is.null(input$idcol) && nzchar(input$idcol) && input$idcol %in% names(df)) {
      keep <- parse_exclude_ids(input$keep_ids)
      if (length(keep) > 0) {
        df[[input$idcol]] <- suppressWarnings(as.integer(as.character(df[[input$idcol]])))
        df <- df %>% filter(.data[[input$idcol]] %in% keep)
      }
    }
    
    df
  })
  
  output$tukey_term_ui <- renderUI({
    req(rv$df)
    
    if (!isTRUE(input$do_stats) || !isTRUE(input$do_anova) || !isTRUE(input$do_tukey)) {
      return(helpText("Turn on: Enable stats → Run ANOVA → Run Tukey HSD."))
    }
    
    df <- df_work()
    fml <- build_anova_formula(df)
    if (is.null(fml)) return(helpText("Pick at least 1 factor to fit ANOVA."))
    
    y <- all.vars(fml)[1]
    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]
    
    factors <- get_anova_factors()
    for (f in factors) if (f %in% names(df)) df[[f]] <- as.factor(df[[f]])
    
    nB <- input$anova_n_blocks %||% 0
    if (nB > 0) {
      for (i in seq_len(nB)) {
        b <- input[[paste0("anova_block_", i)]]
        if (!is.null(b) && nzchar(b) && b %in% names(df)) df[[b]] <- as.factor(df[[b]])
      }
    }
    
    if (nrow(df) < 2) return(helpText("Not enough data to fit ANOVA."))
    
    fit <- tryCatch(aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) return(helpText("ANOVA failed; cannot list Tukey terms."))
    
    terms <- get_tukey_terms_from_fit(fit)
    if (length(terms) == 0) return(helpText("No Tukey terms available for this model."))
    
    sel <- if (!is.null(input$tukey_term) && input$tukey_term %in% terms) input$tukey_term else terms[1]
    selectInput("tukey_term", "Tukey term", choices = terms, selected = sel)
  })
  
  assumption_results_for_df <- function(df) {
    if (!isTRUE(input$do_stats) || !isTRUE(input$check_assumptions)) return("Assumption checks are OFF.")
    
    fml <- build_anova_formula(df)
    if (is.null(fml)) return("Pick at least 1 factor to run assumption checks.")
    
    y <- all.vars(fml)[1]
    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]
    if (nrow(df) < 3) return("Not enough non-NA values (need ≥ 3).")
    
    factors <- get_anova_factors()
    for (f in factors) if (f %in% names(df)) df[[f]] <- as.factor(df[[f]])
    
    nB <- input$anova_n_blocks %||% 0
    if (nB > 0) {
      for (i in seq_len(nB)) {
        b <- input[[paste0("anova_block_", i)]]
        if (!is.null(b) && nzchar(b) && b %in% names(df)) df[[b]] <- as.factor(df[[b]])
      }
    }
    
    fit <- tryCatch(lm(fml, data = df), error = function(e) e)
    if (inherits(fit, "error")) return(paste("Model fit failed:", fit$message))
    
    r <- resid(fit)
    n <- length(r)
    
    shapiro_line <- "Shapiro-Wilk: skipped (n outside 3–5000)"
    if (n >= 3 && n <= 5000) {
      sw <- shapiro.test(r)
      shapiro_line <- sprintf("Shapiro-Wilk on residuals: W=%.3f, p=%.4g", sw$statistic, sw$p.value)
    }
    
    stdr <- tryCatch(rstandard(fit), error = function(e) rep(NA_real_, length(r)))
    out_line <- sprintf("Outliers (|standardized residual| > 3): %d", sum(abs(stdr) > 3, na.rm = TRUE))
    
    homo_lines <- character(0)
    if (isTRUE(input$assump_show_groups)) {
      factors <- get_anova_factors()
      if (length(factors) >= 1) {
        grp <- interaction(df[, factors, drop = FALSE], drop = TRUE, sep = ":")
        fl <- tryCatch(fligner.test(df[[y]] ~ grp), error = function(e) e)
        bt <- tryCatch(bartlett.test(df[[y]] ~ grp), error = function(e) e)
        homo_lines <- c(
          if (inherits(fl, "error")) {
            paste0("Fligner-Killeen failed: ", fl$message)
          } else {
            sprintf("Fligner-Killeen (variance across groups): chi^2=%.3f, p=%.4g", fl$statistic, fl$p.value)
          },
          if (inherits(bt, "error")) {
            paste0("Bartlett failed: ", bt$message)
          } else {
            sprintf("Bartlett (variance across groups): K^2=%.3f, p=%.4g", bt$statistic, bt$p.value)
          }
        )
      } else {
        homo_lines <- "Variance checks: need ≥1 factor."
      }
    }
    
    paste(
      "Model used for checks:",
      paste(deparse(fml), collapse = " "),
      "",
      shapiro_line,
      out_line,
      paste(homo_lines, collapse = "\n"),
      sep = "\n"
    )
  }
  
  output$assump_out <- renderPrint({
    df <- df_work()
    cat(assumption_results_for_df(df), "\n")
  })
  
  anova_results <- reactive({
    req(isTRUE(input$do_anova))
    df <- df_work()
    req(nrow(df) > 1)
    
    cols <- names(df)
    req(!is.null(input$anova_y), input$anova_y %in% cols)
    y <- input$anova_y
    factors <- get_anova_factors()
    
    nB <- input$anova_n_blocks %||% 0
    blocks <- character(0)
    if (nB > 0) {
      for (i in seq_len(nB)) {
        b <- input[[paste0("anova_block_", i)]]
        if (!is.null(b) && nzchar(b) && b %in% cols) blocks <- c(blocks, b)
      }
      blocks <- unique(blocks)
    }
    
    if (length(factors) < 1) return(list(err = "Pick at least 1 factor (Factor A)."))
    
    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]
    if (nrow(df) < 2) return(list(err = "Not enough non-NA Y values."))
    
    for (f in factors) df[[f]] <- as.factor(df[[f]])
    for (b in blocks) df[[b]] <- as.factor(df[[b]])
    
    interaction_mode <- input$anova_interaction_mode %||% "two_way"
    if (interaction_mode == "none" || length(factors) == 1) {
      rhs <- paste(factors, collapse = " + ")
    } else if (interaction_mode == "full") {
      rhs <- paste(factors, collapse = " * ")
    } else {
      main <- paste(factors, collapse = " + ")
      pair_terms <- combn(factors, 2, FUN = function(x) paste0(x[1], ":", x[2]))
      rhs <- paste(c(main, pair_terms), collapse = " + ")
    }
    if (length(blocks) > 0) rhs <- paste(rhs, "+", paste(blocks, collapse = " + "))
    
    fml <- as.formula(paste(y, "~", rhs))
    fit <- tryCatch(aov(fml, data = df), error = function(e) e)
    if (inherits(fit, "error")) return(list(err = paste("ANOVA failed:", fit$message)))
    
    tuk <- NULL
    if (isTRUE(input$do_tukey)) {
      term <- input$tukey_term
      if (is.null(term) || !nzchar(term)) {
        tuk <- list(error = "Pick a Tukey term.")
      } else {
        tuk0 <- tryCatch(TukeyHSD(fit, which = term), error = function(e) e)
        if (inherits(tuk0, "error")) {
          tuk <- list(error = tuk0$message)
        } else if (!(term %in% names(tuk0))) {
          tuk <- list(error = paste0("Tukey term not in model: ", term, ". Available: ", paste(names(tuk0), collapse = ", ")))
        } else {
          alpha <- input$tukey_alpha
          mat <- as.data.frame(tuk0[[term]])
          mat$Comparison <- rownames(mat)
          if (isTRUE(input$tukey_sig_only)) mat <- mat[mat$`p adj` < alpha, , drop = FALSE]
          tuk <- list(filtered = mat, which = term, raw = tuk0)
        }
      }
    }
    
    list(formula = fml, summary = summary(fit), tukey = tuk)
  })
  
  output$dynamic_filters <- renderUI({
    req(rv$df)
    nF <- input$n_filters
    if (is.null(nF) || nF == 0) return(NULL)
    
    cols <- names(rv$df)
    filter_list <- lapply(seq_len(nF), function(i) {
      col_id <- paste0("filter_col_", i)
      mode_id <- paste0("filter_mode_", i)
      
      selected_col <- safe_get(rv_filters$cols, i)
      if (is.null(selected_col) || !(selected_col %in% cols)) selected_col <- cols[1]
      
      selected_mode <- safe_get(rv_filters$modes, i)
      if (is.null(selected_mode) || !(selected_mode %in% c("keep", "remove"))) selected_mode <- "keep"
      
      tagList(
        tags$hr(),
        h5(paste("Filter", i)),
        selectInput(col_id, "Column", choices = cols, selected = selected_col),
        selectInput(mode_id, "Mode", choices = c("Keep selected" = "keep", "Remove selected" = "remove"), selected = selected_mode),
        uiOutput(paste0("filter_val_ui_", i))
      )
    })
    tagList(filter_list)
  })
  
  output$anova_out <- renderPrint({
    if (!isTRUE(input$do_anova)) {
      cat("ANOVA is off (toggle 'Run ANOVA' in the sidebar).\n")
      return(invisible(NULL))
    }
    
    res <- anova_results()
    if (!is.null(res$err)) {
      cat(res$err, "\n")
      return(invisible(NULL))
    }
    
    cat("Model formula:\n")
    print(res$formula)
    cat("\nANOVA summary:\n")
    print(res$summary)
    
    if (isTRUE(input$do_tukey)) {
      cat("\nTukey HSD:\n")
      if (is.null(res$tukey)) {
        cat("No Tukey results.\n")
      } else if (!is.null(res$tukey$error)) {
        cat("Tukey error:", res$tukey$error, "\n")
      } else {
        cat("Term:", res$tukey$which, "\n")
        print(res$tukey$filtered)
      }
    }
  })
  
  observeEvent(list(input$fillcol, input$fill_levels_selected), {
    req(rv$df)
    
    if (is.null(input$fillcol) || !nzchar(input$fillcol) || !(input$fillcol %in% names(rv$df))) {
      rv_fill_levels(NULL)
      return()
    }
    
    levs <- unique(trimws(as.character(df_work()[[input$fillcol]])))
    levs <- levs[!is.na(levs) & nzchar(levs)]
    
    if (length(levs) == 0) {
      rv_fill_levels(NULL)
      return()
    }
    
    picked <- input$fill_levels_selected
    if (!is.null(picked)) {
      picked <- picked[picked %in% levs]
      rv_fill_levels(if (length(picked) > 0) picked else levs)
      return()
    }
    
    if (all(default_fill_order %in% levs)) {
      rv_fill_levels(c(default_fill_order, setdiff(levs, default_fill_order)))
    } else {
      rv_fill_levels(levs)
    }
  }, ignoreInit = FALSE)
  
  make_one_plot <- function(df) {
    req(input$xcol, input$ycol)
    
    df[[input$ycol]] <- suppressWarnings(as.numeric(df[[input$ycol]]))
    
    x_raw <- as.character(df[[input$xcol]])
    x_levels <- unique(x_raw)
    x_levels_num <- suppressWarnings(as.numeric(x_levels))
    if (all(!is.na(x_levels_num))) x_levels <- x_levels[order(x_levels_num)]
    
    df$.__xlab__ <- factor(x_raw, levels = x_levels)
    
    if (nzchar(input$fillcol)) {
      if (!is.null(rv_fill_levels())) {
        df[[input$fillcol]] <- factor(df[[input$fillcol]], levels = rv_fill_levels())
      } else {
        df[[input$fillcol]] <- as.factor(df[[input$fillcol]])
      }
    }
    
    if (nzchar(input$shapecol)) df[[input$shapecol]] <- as.factor(df[[input$shapecol]])
    if (nzchar(input$facet_row)) df[[input$facet_row]] <- as.factor(df[[input$facet_row]])
    if (nzchar(input$facet_col)) df[[input$facet_col]] <- as.factor(df[[input$facet_col]])
    
    rect_df <- NULL
    label_df <- NULL
    
    tryCatch({
      shade_spec <- parse_shade_x(input$shade_x)
      if (shade_spec$type != "none") {
        pos_map <- setNames(seq_along(x_levels), x_levels)
        
        if (shade_spec$type == "values") {
          wanted <- intersect(shade_spec$values, x_levels)
          if (length(wanted) > 0) {
            xs <- unname(pos_map[wanted])
            rect_df <- data.frame(xmin = xs - 0.5, xmax = xs + 0.5, ymin = -Inf, ymax = Inf)
          }
        }
        
        if (shade_spec$type == "ranges") {
          x_levels_num2 <- suppressWarnings(as.numeric(x_levels))
          if (!all(!is.na(x_levels_num2))) stop("Range shading requires numeric x labels.")
          
          rect_list <- lapply(shade_spec$ranges, function(rg) {
            keep <- which(x_levels_num2 >= rg[1] & x_levels_num2 <= rg[2])
            if (length(keep) == 0) return(NULL)
            data.frame(xmin = min(keep) - 0.5, xmax = max(keep) + 0.5, ymin = -Inf, ymax = Inf)
          })
          rect_list <- Filter(Negate(is.null), rect_list)
          if (length(rect_list) > 0) rect_df <- do.call(rbind, rect_list)
        }
        
        if (!is.null(rect_df) && nrow(rect_df) > 0) {
          y_top <- input$ymax %||% max(df[[input$ycol]], na.rm = TRUE)
          yrange <- input$ymax - input$ymin
          if (is.null(yrange) || is.na(yrange) || yrange == 0) yrange <- 1
          y_label <- y_top - 0.03 * yrange
          
          label_text <- input$shade_label %||% "Before Mating"
          if (!nzchar(label_text)) label_text <- "Before Mating"
          
          label_df <- data.frame(
            x = max(rect_df$xmax) - 0.05,
            y = y_label,
            label = label_text,
            hjust = 1
          )
        }
      }
    }, error = function(e) {
      rect_df <<- NULL
      label_df <<- NULL
      showNotification(paste0("Shading ignored: ", conditionMessage(e)), type = "warning", duration = 5)
    })
    
    yvar <- input$ycol
    aes_map <- aes(x = .data$.__xlab__, y = .data[[yvar]])
    
    if (nzchar(input$fillcol)) {
      fvar <- input$fillcol
      aes_map <- modifyList(aes_map, aes(fill = .data[[fvar]], group = interaction(.data$.__xlab__, .data[[fvar]], drop = TRUE)))
    } else {
      aes_map <- modifyList(aes_map, aes(group = .data$.__xlab__))
    }
    
    if (nzchar(input$shapecol)) {
      svar <- input$shapecol
      aes_map <- modifyList(aes_map, aes(shape = .data[[svar]]))
    }
    
    p <- ggplot(df, aes_map)
    
    if (!is.null(rect_df) && nrow(rect_df) > 0) {
      p <- p + geom_rect(
        data = rect_df,
        inherit.aes = FALSE,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "grey60",
        alpha = input$shade_alpha
      )
    }
    
    if (!is.null(label_df) && nrow(label_df) > 0) {
      p <- p + geom_text(
        data = label_df,
        inherit.aes = FALSE,
        aes(x = x, y = y, label = label),
        hjust = 1,
        vjust = 1,
        fontface = "bold"
      )
    }
    
    if (isTRUE(input$show_boxplot)) {
      p <- p + geom_boxplot(outlier.shape = NA, position = position_dodge(width = input$dodge_width))
    }
    
    if (isTRUE(input$show_jitter)) {
      p <- p + geom_jitter(
        alpha = 0.7,
        size = 2,
        position = position_jitterdodge(jitter.width = input$jitter_width, dodge.width = input$dodge_width)
      )
    }
    
    if (isTRUE(input$do_stats) && isTRUE(input$do_anova) && isTRUE(input$do_tukey) && isTRUE(input$show_tukey_letters)) {
      res <- tryCatch(anova_results(), error = function(e) NULL)
      if (!is.null(res) && is.null(res$err) && !is.null(res$tukey) && !is.null(res$tukey$raw)) {
        term <- res$tukey$which
        term_vars <- strsplit(term, ":", fixed = TRUE)[[1]]
        
        if (input$xcol %in% term_vars) {
          letters_df <- tukey_letters_from_raw(res$tukey$raw, term, alpha = input$tukey_alpha)
          
          if (!is.null(letters_df) && nrow(letters_df) > 0) {
            df$.__tukey_key__ <- interaction(df[, term_vars, drop = FALSE], drop = TRUE, sep = ":")
            
            if (nzchar(input$fillcol)) {
              pos_df <- df %>%
                group_by(.__xlab__, .data[[input$fillcol]], .__tukey_key__) %>%
                summarise(ypos = max(.data[[input$ycol]], na.rm = TRUE), .groups = "drop")
            } else {
              pos_df <- df %>%
                group_by(.__xlab__, .__tukey_key__) %>%
                summarise(ypos = max(.data[[input$ycol]], na.rm = TRUE), .groups = "drop")
            }
            
            pos_df <- pos_df %>% left_join(letters_df, by = c(".__tukey_key__" = "level"))
            yr <- input$ymax - input$ymin
            if (is.null(yr) || is.na(yr) || yr == 0) yr <- 1
            pos_df$ypos <- pos_df$ypos + 0.05 * yr
            
            if (nzchar(input$fillcol)) {
              p <- p + geom_text(
                data = pos_df,
                aes(x = .__xlab__, y = ypos, label = letter, group = .data[[input$fillcol]]),
                inherit.aes = FALSE,
                position = position_dodge(width = input$dodge_width),
                vjust = 0,
                fontface = "bold"
              )
            } else {
              p <- p + geom_text(data = pos_df, aes(x = .__xlab__, y = ypos, label = letter), inherit.aes = FALSE, vjust = 0, fontface = "bold")
            }
          }
        }
      }
    }
    
    fr <- if (nzchar(input$facet_row)) input$facet_row else "."
    fc <- if (nzchar(input$facet_col)) input$facet_col else "."
    if (fr != "." || fc != ".") {
      p <- p + facet_grid(as.formula(paste(fr, "~", fc)), scales = if (isTRUE(input$split_free_y)) "free_y" else "fixed")
    }
    
    base_size <- 11 * input$text_scale
    xlab <- if (nzchar(input$xlabel_custom)) input$xlabel_custom else input$xcol
    ylab <- if (nzchar(input$ylabel_custom)) input$ylabel_custom else input$ycol
    
    p <- p +
      scale_x_discrete(drop = FALSE) +
      coord_cartesian(ylim = c(input$ymin, input$ymax)) +
      theme_bw(base_size = base_size) +
      theme(
        legend.position = if (nzchar(input$fillcol) || nzchar(input$shapecol)) "bottom" else "none",
        plot.title = element_text(size = base_size * 1.2, face = "bold"),
        plot.subtitle = element_text(size = base_size),
        axis.text = element_text(size = base_size * 0.9),
        legend.text = element_text(size = base_size * 0.9),
        strip.text = element_text(size = base_size)
      ) +
      labs(
        title = input$title_custom %||% NULL,
        subtitle = input$subtitle_custom %||% NULL,
        x = xlab,
        y = ylab,
        fill = if (input$fillcol == "group") "Group" else if (nzchar(input$fillcol)) input$fillcol else NULL,
        shape = if (nzchar(input$shapecol)) input$shapecol else NULL
      )
    
    if (nzchar(input$fillcol) && isTRUE(input$custom_fill_colors) && nzchar(input$fill_colors)) {
      cols <- strsplit(input$fill_colors, ",", fixed = TRUE)[[1]] |> trimws()
      cols <- cols[nzchar(cols)]
      if (length(cols) > 0) {
        n_levels <- nlevels(df[[input$fillcol]])
        if (length(cols) < n_levels) cols <- rep(cols, length.out = n_levels)
        p <- p + scale_fill_manual(values = cols[seq_len(n_levels)])
      }
    }
    
    p
  }
  
  observe({
    req(rv$df)
    nF <- input$n_filters
    if (is.null(nF) || nF == 0) return()
    
    for (i in seq_len(nF)) {
      local({
        ii <- i
        output[[paste0("filter_val_ui_", ii)]] <- renderUI({
          col <- input[[paste0("filter_col_", ii)]]
          req(col)
          
          vals <- unique(as.character(rv$df[[col]]))
          vals <- vals[!is.na(vals)]
          
          remembered_vals <- if (length(rv_filters$vals) >= ii) rv_filters$vals[[ii]] else NULL
          if (is.null(remembered_vals)) remembered_vals <- character(0)
          remembered_vals <- remembered_vals[remembered_vals %in% vals]
          
          selectInput(
            paste0("filter_val_", ii),
            "Values",
            choices = vals,
            selected = remembered_vals,
            multiple = TRUE
          )
        })
      })
    }
  })
  
  observe({
    nF <- input$n_filters
    if (is.null(nF) || nF == 0) return()
    
    for (i in seq_len(nF)) {
      col <- input[[paste0("filter_col_", i)]]
      mode <- input[[paste0("filter_mode_", i)]]
      vals <- input[[paste0("filter_val_", i)]]
      
      if (!is.null(col)) rv_filters$cols[[i]] <- col
      if (!is.null(mode)) rv_filters$modes[[i]] <- mode
      if (!is.null(vals)) rv_filters$vals[[i]] <- vals
    }
    
    if (length(rv_filters$cols) > nF) rv_filters$cols <- rv_filters$cols[seq_len(nF)]
    if (length(rv_filters$modes) > nF) rv_filters$modes <- rv_filters$modes[seq_len(nF)]
    if (length(rv_filters$vals) > nF) rv_filters$vals <- rv_filters$vals[seq_len(nF)]
  })
  
  output$plot_ui <- renderUI({
    req(input$plot_height)
    
    if (is.null(input$split_col) || !nzchar(input$split_col)) {
      return(tagList(
        plotOutput("plot", height = paste0(input$plot_height * 72, "px")),
        tags$hr(),
        h4("Assumption checks"),
        verbatimTextOutput("assump_out"),
        tags$small(
          "How to interpret:",
          tags$ul(
            tags$li("Equal Variance (Fligner/Bartlett): p ≥ 0.05 → variances equal (ANOVA appropriate). p < 0.05 → variances unequal (consider Welch ANOVA)."),
            tags$li("Normality (Shapiro-Wilk): p ≥ 0.05 → normality OK. p < 0.05 → residuals deviate from normal."),
            tags$li("Outliers: |standardized residual| > 3 suggests influential observations.")
          ),
          "Most important: independence > equal variance > normality."
        ),
        tags$hr(),
        h4("ANOVA / Tukey"),
        verbatimTextOutput("anova_out")
      ))
    }
    
    df <- df_work()
    if (!(input$split_col %in% names(df))) {
      return(tagList(
        plotOutput("plot", height = paste0(input$plot_height * 72, "px")),
        tags$hr(),
        h4("Assumption checks"),
        verbatimTextOutput("assump_out"),
        tags$hr(),
        h4("ANOVA / Tukey"),
        verbatimTextOutput("anova_out")
      ))
    }
    
    lvls <- unique(as.character(df[[input$split_col]]))
    lvls <- lvls[!is.na(lvls) & nzchar(lvls)]
    
    tagList(lapply(seq_along(lvls), function(i) {
      tagList(
        plotOutput(outputId = paste0("plot_", i), height = paste0(input$plot_height * 72, "px")),
        tags$hr(),
        h4(paste0("Stats: ", input$split_col, " = ", lvls[i])),
        h5("Assumption checks"),
        verbatimTextOutput(paste0("assump_out_", i)),
        h5("ANOVA / Tukey"),
        verbatimTextOutput(paste0("anova_out_", i)),
        tags$hr()
      )
    }))
  })
  
  output$plot <- renderPlot({
    df <- df_work()
    if (!is.null(input$split_col) && nzchar(input$split_col)) return(NULL)
    make_one_plot(df)
  })
  
  observe({
    req(rv$df)
    req(input$plot_height)
    
    if (is.null(input$split_col) || !nzchar(input$split_col)) return()
    
    df <- df_work()
    req(input$split_col %in% names(df))
    
    lvls <- unique(as.character(df[[input$split_col]]))
    lvls <- lvls[!is.na(lvls) & nzchar(lvls)]
    
    for (i in seq_along(lvls)) {
      local({
        ii <- i
        lvl <- lvls[ii]
        out_id <- paste0("plot_", ii)
        
        output[[out_id]] <- renderPlot({
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          sub <- input$subtitle_custom %||% ""
          new_sub <- if (nzchar(sub)) paste0(sub, " | ", input$split_col, ": ", lvl) else paste0(input$split_col, ": ", lvl)
          
          p <- make_one_plot(dfi) + labs(subtitle = new_sub)
          if (isTRUE(input$split_free_y)) p <- p + coord_cartesian(ylim = range(dfi[[input$ycol]], na.rm = TRUE))
          p
        })
        
        output[[paste0("assump_out_", ii)]] <- renderPrint({
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          cat(assumption_results_for_df(dfi), "\n")
        })
        
        output[[paste0("anova_out_", ii)]] <- renderPrint({
          if (!isTRUE(input$do_anova)) {
            cat("ANOVA is off (toggle 'Run ANOVA' in the sidebar).\n")
            return(invisible(NULL))
          }
          
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          fml <- build_anova_formula(dfi)
          if (is.null(fml)) {
            cat("Pick at least 1 factor.\n")
            return(invisible(NULL))
          }
          
          y <- all.vars(fml)[1]
          dfi[[y]] <- suppressWarnings(as.numeric(dfi[[y]]))
          dfi <- dfi[!is.na(dfi[[y]]), , drop = FALSE]
          
          if (nrow(dfi) < 2) {
            cat("Not enough non-NA Y values for this panel.\n")
            return(invisible(NULL))
          }
          
          fit <- tryCatch(aov(fml, data = dfi), error = function(e) e)
          if (inherits(fit, "error")) {
            cat("ANOVA failed:", fit$message, "\n")
            return(invisible(NULL))
          }
          
          cat("Model formula:\n")
          print(fml)
          cat("\nANOVA summary:\n")
          print(summary(fit))
          
          if (isTRUE(input$do_tukey)) {
            term <- input$tukey_term
            cat("\nTukey HSD:\n")
            
            tuk0 <- tryCatch(TukeyHSD(fit, which = term), error = function(e) e)
            if (inherits(tuk0, "error")) {
              cat("Tukey error:", tuk0$message, "\n")
              return(invisible(NULL))
            }
            
            if (!(term %in% names(tuk0))) {
              cat("Tukey term not found in model:", term, "\n")
              cat("Available terms:", paste(names(tuk0), collapse = ", "), "\n")
              return(invisible(NULL))
            }
            
            mat <- as.data.frame(tuk0[[term]])
            mat$Comparison <- rownames(mat)
            
            alpha <- input$tukey_alpha
            if (isTRUE(input$tukey_sig_only)) mat <- mat[mat$`p adj` < alpha, , drop = FALSE]
            
            cat("Term:", term, "\n")
            print(mat)
          }
        })
      })
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".png"),
    content = function(file) {
      df <- df_work()
      p <- make_one_plot(df)
      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = 300)
    }
  )
}

shinyApp(ui, server)
