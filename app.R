library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(multcompView)
library(DT)

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

parse_relabel_map <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(setNames(character(0), character(0)))
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  pairs <- lapply(lines, function(ln) {
    parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(NULL)
    key <- trimws(parts[[1]])
    val <- trimws(paste(parts[-1], collapse = "="))
    if (!nzchar(key)) return(NULL)
    c(key, val)
  })
  pairs <- pairs[!vapply(pairs, is.null, logical(1))]
  if (length(pairs) == 0) return(setNames(character(0), character(0)))
  keys <- vapply(pairs, `[[`, character(1), 1)
  vals <- vapply(pairs, `[[`, character(1), 2)
  setNames(vals, keys)
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

normalize_term_token <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("`", "", x, fixed = TRUE)
  x <- gsub("^as\\.factor\\((.*)\\)$", "\\1", x)
  x <- gsub("^factor\\((.*)\\)$", "\\1", x)
  x <- gsub("^I\\((.*)\\)$", "\\1", x)
  x
}

term_includes_var <- function(term, var_name) {
  if (is.null(term) || !nzchar(term) || is.null(var_name) || !nzchar(var_name)) return(FALSE)
  tokens <- strsplit(as.character(term), ":", fixed = TRUE)[[1]]
  tokens <- vapply(tokens, normalize_term_token, character(1))
  target <- normalize_term_token(var_name)
  any(tokens == target)
}

choose_preferred_tukey_term <- function(terms, current = NULL, xcol = NULL, fillcol = NULL) {
  terms <- unique(terms[!is.na(terms) & nzchar(terms)])
  if (length(terms) == 0) return(NULL)

  if (!is.null(current) && nzchar(current) && current %in% terms) return(current)

  if (!is.null(fillcol) && nzchar(fillcol)) {
    fill_hits <- terms[vapply(terms, term_includes_var, logical(1), var_name = fillcol)]
    if (length(fill_hits) > 0) return(fill_hits[1])
  }

  if (!is.null(xcol) && nzchar(xcol)) {
    x_hits <- terms[vapply(terms, term_includes_var, logical(1), var_name = xcol)]
    if (length(x_hits) > 0) return(x_hits[1])
  }

  main_effects <- terms[!grepl(":", terms, fixed = TRUE)]
  if (length(main_effects) > 0) return(main_effects[1])

  terms[1]
}

ui <- fluidPage(
  titlePanel("Plot Builder (Pivot-Style ggplot GUI)"),

  tags$style(HTML("\
    body { overflow-y: hidden; }\
    .container-fluid { height: calc(100vh - 80px); }\
    .well { max-height: calc(100vh - 120px); overflow-y: auto; }\
    #plotpane {\
      height: calc(100vh - 140px);\
      overflow: auto;\
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
      h4("Filtering"),
      sliderInput("n_filters", "How many filters?", min = 0, max = 6, value = 0, step = 1),
      uiOutput("dynamic_filters"),

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

      tags$hr(),
      h4("Plot labels"),
      textInput("legend_title_fill", "Fill legend title (blank = column name)", value = ""),
      textInput("legend_title_shape", "Shape legend title (blank = column name)", value = ""),
      textAreaInput(
        "level_relabel",
        "Rename levels (one per line: old=new)",
        value = "",
        placeholder = "OO=Old/Old\nYY=Young/Young",
        rows = 4
      ),
      helpText("Applies to X axis ticks, fill/shape legend levels, and facet strip labels."),

      h4("Stats (Assumptions → ANOVA → Tukey)"),
      checkboxInput("do_stats", "Enable stats", FALSE),

      conditionalPanel(
        "input.do_stats",
        h5("Assumption checks (recommended)"),
        checkboxInput("check_assumptions", "Run assumption checks", TRUE),
        checkboxInput("assump_show_groups", "Check variance across groups (Fligner/Bartlett)", TRUE),

        tags$hr(),
        h5("Two-group t-test"),
        checkboxInput("do_ttest", "Run t-test", FALSE),
        conditionalPanel(
          "input.do_ttest",
          uiOutput("ttest_y_ui"),
          uiOutput("ttest_group_col_ui"),
          uiOutput("ttest_group_levels_ui"),
          selectInput(
            "ttest_variance",
            "Variance",
            choices = c("Welch (unequal)" = "welch", "Pooled (equal)" = "equal"),
            selected = "welch"
          ),
          checkboxInput("ttest_paired", "Paired (uses ID column)", FALSE),
          selectInput(
            "ttest_alternative",
            "Alternative",
            choices = c("two-sided" = "two.sided", "A < B (less)" = "less", "A > B (greater)" = "greater"),
            selected = "two.sided"
          ),
          numericInput("ttest_alpha", "Alpha", value = 0.05, min = 0.0001, max = 0.2, step = 0.01)
        ),

        tags$hr(),
        h5("ANOVA settings"),
        checkboxInput("do_anova", "Run ANOVA", TRUE),
        selectInput(
          "anova_method",
          "ANOVA type",
          choices = c("Regular multi-way ANOVA" = "regular", "Multi-way Welch ANOVA" = "welch"),
          selected = "regular"
        ),
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

  coalesce_chr <- function(current, fallback = character(0)) {
    if (!is.null(current)) as.character(current) else as.character(fallback)
  }

  safe_list_get <- function(x, i) {
    if (length(x) >= i) x[[i]] else NULL
  }

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
      selectInput("xcol", "X", cols, selected = keep_or(isolate(input$xcol), pick("day", 1))),
      selectInput("ycol", "Y", cols, selected = keep_or(isolate(input$ycol), pick("eggs_laid_per_fly", 2))),
      selectInput("fillcol", "Fill", c("None" = "", cols), selected = {
        cur <- keep_or_blank(isolate(input$fillcol))
        if (nzchar(cur)) cur else if ("group" %in% cols) "group" else ""
      }),
      selectInput("shapecol", "Shape", c("None" = "", cols), selected = keep_or_blank(isolate(input$shapecol))),
      selectInput("facet_row", "Facet row", c("None" = "", cols), selected = keep_or_blank(isolate(input$facet_row))),
      selectInput("facet_col", "Facet col", c("None" = "", cols), selected = keep_or_blank(isolate(input$facet_col)))
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

    cur_id <- isolate(input$idcol)
    selected_id <- ""
    if (!is.null(cur_id) && nzchar(cur_id) && cur_id %in% cols) {
      selected_id <- cur_id
    } else if ("exp_id" %in% cols) {
      selected_id <- "exp_id"
    }

    selectInput("idcol", "ID column", c("None" = "", cols), selected = selected_id)
  })

  output$split_selector <- renderUI({
    req(rv$df)
    cols <- names(rv$df)

    cur_split <- isolate(input$split_col)
    cur <- if (!is.null(cur_split) && cur_split %in% cols) cur_split else ""
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

    cur <- coalesce_chr(isolate(input$fill_levels_selected), isolate(rv_fill_levels()))
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

  output$ttest_y_ui <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    cur <- isolate(input$ttest_y)
    fallback_y <- isolate(input$ycol)
    default <- if (!is.null(cur) && cur %in% cols) cur
               else if (!is.null(fallback_y) && nzchar(fallback_y) && fallback_y %in% cols) fallback_y
               else cols[1]
    selectInput("ttest_y", "Response (Y)", choices = cols, selected = default)
  })

  output$ttest_group_col_ui <- renderUI({
    req(rv$df)
    cols <- names(rv$df)
    cur <- isolate(input$ttest_group_col)
    fallback_fill <- isolate(input$fillcol)
    fallback_x <- isolate(input$xcol)
    default <- if (!is.null(cur) && cur %in% cols) cur
               else if (!is.null(fallback_fill) && nzchar(fallback_fill) && fallback_fill %in% cols) fallback_fill
               else if (!is.null(fallback_x) && nzchar(fallback_x) && fallback_x %in% cols) fallback_x
               else cols[1]
    selectInput("ttest_group_col", "Group column", choices = cols, selected = default)
  })

  output$ttest_group_levels_ui <- renderUI({
    req(rv$df)
    g <- input$ttest_group_col
    if (is.null(g) || !nzchar(g) || !(g %in% names(rv$df))) {
      return(helpText("Pick a group column first."))
    }
    levs <- unique(trimws(as.character(df_work()[[g]])))
    levs <- sort(levs[!is.na(levs) & nzchar(levs)])
    if (length(levs) < 2) return(helpText("Need at least 2 levels in the group column after current filters."))

    cur_a <- isolate(input$ttest_group_a)
    cur_b <- isolate(input$ttest_group_b)
    sel_a <- if (!is.null(cur_a) && cur_a %in% levs) cur_a else levs[1]
    sel_b <- if (!is.null(cur_b) && cur_b %in% levs && cur_b != sel_a) cur_b else {
      alt <- setdiff(levs, sel_a)
      if (length(alt) > 0) alt[1] else levs[min(2, length(levs))]
    }

    tagList(
      selectInput("ttest_group_a", "Group A", choices = levs, selected = sel_a),
      selectInput("ttest_group_b", "Group B", choices = levs, selected = sel_b)
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
      mem <- isolate(safe_get(rv_anova$factors, i))
      default_guess <- if (i == 1) guess_factor() else ""
      default <- if (!is.null(mem) && mem %in% c("", cols)) mem else default_guess
      selectInput(id, paste0("Factor ", LETTERS[i]), choices = c("None" = "", cols), selected = default)
    })

    block_pick <- lapply(seq_len(nB), function(i) {
      id <- paste0("anova_block_", i)
      mem <- isolate(safe_get(rv_anova$blocks, i))
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
    if (length(rv_anova$factors) > nF) rv_anova$factors <- rv_anova$factors[seq_len(nF)]
  })

  observe({
    req(rv$df)
    nB <- input$anova_n_blocks %||% 0
    if (nB > 0) {
      for (i in seq_len(nB)) {
        v <- input[[paste0("anova_block_", i)]]
        if (!is.null(v)) rv_anova$blocks[[i]] <- v
      }
    }
    if (length(rv_anova$blocks) > nB) rv_anova$blocks <- rv_anova$blocks[seq_len(nB)]
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

    nF <- input$n_filters %||% 0
    if (nF > 0) {
      active_filters <- lapply(seq_len(nF), function(i) {
        col <- input[[paste0("filter_col_", i)]]
        mode <- input[[paste0("filter_mode_", i)]]
        vals <- input[[paste0("filter_val_", i)]]

        if (is.null(col) || !(col %in% names(df)) || is.null(vals) || length(vals) == 0) return(NULL)
        if (!(mode %in% c("keep", "remove"))) return(NULL)

        list(col = col, mode = mode, vals = as.character(vals))
      })

      active_filters <- Filter(Negate(is.null), active_filters)
      if (length(active_filters) > 0) {
        for (flt in active_filters) {
          col_vals <- as.character(df[[flt$col]])
          keep_idx <- col_vals %in% flt$vals
          if (flt$mode == "remove") keep_idx <- !keep_idx
          df <- df[keep_idx, , drop = FALSE]
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

    if (identical(input$anova_method %||% "regular", "welch")) {
      return(helpText("Tukey HSD is only available for regular ANOVA."))
    }

    df <- df_work()
    fml <- build_anova_formula(df)
    if (is.null(fml)) return(helpText("Pick at least 1 factor to fit ANOVA."))

    df <- prep_anova_data(df, fml)$df
    if (nrow(df) < 2) return(helpText("Not enough data to fit ANOVA."))

    fit <- tryCatch(aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) return(helpText("ANOVA failed; cannot list Tukey terms."))

    terms <- get_tukey_terms_from_fit(fit)
    if (length(terms) == 0) return(helpText("No Tukey terms available for this model."))

    preferred_term <- if (!is.null(input$xcol) && nzchar(input$xcol)) {
      hits <- terms[grepl(paste0("(^|:)", input$xcol, "(:|$)"), terms)]
      if (length(hits) > 0) hits[1] else terms[1]
    } else {
      terms[1]
    }

    cur_tukey <- isolate(input$tukey_term)
    sel <- if (!is.null(cur_tukey) && cur_tukey %in% terms) cur_tukey else preferred_term
    selectInput("tukey_term", "Tukey term", choices = terms, selected = sel)
  })

  get_anova_blocks <- function(df_names) {
    nB <- input$anova_n_blocks %||% 0
    if (nB == 0) return(character(0))
    out <- character(0)
    for (i in seq_len(nB)) {
      b <- input[[paste0("anova_block_", i)]]
      if (!is.null(b) && nzchar(b) && b %in% df_names) out <- c(out, b)
    }
    unique(out)
  }

  prep_anova_data <- function(df, fml) {
    y <- all.vars(fml)[1]
    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]

    factors <- get_anova_factors()
    for (f in factors) if (f %in% names(df)) df[[f]] <- as.factor(df[[f]])

    for (b in get_anova_blocks(names(df))) df[[b]] <- as.factor(df[[b]])

    list(df = df, y = y, factors = factors)
  }

  assumption_results_for_df <- function(df) {
    if (!isTRUE(input$do_stats) || !isTRUE(input$check_assumptions)) return("Assumption checks are OFF.")

    fml <- build_anova_formula(df)
    if (is.null(fml)) return("Pick at least 1 factor to run assumption checks.")

    prep <- prep_anova_data(df, fml)
    df <- prep$df; y <- prep$y; factors <- prep$factors
    if (nrow(df) < 3) return("Not enough non-NA values (need ≥ 3).")

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

    assumption_lines <- c(
      "Model used for checks:",
      paste(deparse(fml), collapse = " "),
      "",
      homo_lines,
      shapiro_line,
      out_line
    )

    paste(assumption_lines[nzchar(assumption_lines)], collapse = "\n")
  }

  output$assump_out <- renderPrint({
    df <- df_work()
    cat(assumption_results_for_df(df), "\n")
  })

  compute_descriptive_stats <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (is.null(input$ycol) || !nzchar(input$ycol) || !(input$ycol %in% names(df))) return(NULL)

    y <- input$ycol
    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]
    if (nrow(df) == 0) return(NULL)

    group_cols <- character(0)
    for (k in c("xcol", "fillcol", "facet_row", "facet_col")) {
      v <- input[[k]]
      if (!is.null(v) && nzchar(v) && v %in% names(df)) group_cols <- c(group_cols, v)
    }
    group_cols <- unique(group_cols)

    if (length(group_cols) == 0) {
      out <- df %>% summarise(
        n      = sum(!is.na(.data[[y]])),
        mean   = mean(.data[[y]], na.rm = TRUE),
        sd     = sd(.data[[y]], na.rm = TRUE),
        se     = sd(.data[[y]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[y]]))),
        median = median(.data[[y]], na.rm = TRUE),
        min    = min(.data[[y]], na.rm = TRUE),
        Q1     = quantile(.data[[y]], 0.25, na.rm = TRUE, names = FALSE),
        Q3     = quantile(.data[[y]], 0.75, na.rm = TRUE, names = FALSE),
        max    = max(.data[[y]], na.rm = TRUE)
      )
    } else {
      out <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          n      = sum(!is.na(.data[[y]])),
          mean   = mean(.data[[y]], na.rm = TRUE),
          sd     = sd(.data[[y]], na.rm = TRUE),
          se     = sd(.data[[y]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[y]]))),
          median = median(.data[[y]], na.rm = TRUE),
          min    = min(.data[[y]], na.rm = TRUE),
          Q1     = quantile(.data[[y]], 0.25, na.rm = TRUE, names = FALSE),
          Q3     = quantile(.data[[y]], 0.75, na.rm = TRUE, names = FALSE),
          max    = max(.data[[y]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    as.data.frame(out)
  }

  output$desc_table <- renderTable({
    df <- df_work()
    compute_descriptive_stats(df)
  }, digits = 3, striped = TRUE, hover = TRUE, na = "—")

  mapped_columns <- function(df) {
    keep <- character(0)
    for (k in c("idcol", "xcol", "ycol", "fillcol", "shapecol", "facet_row", "facet_col", "split_col")) {
      v <- input[[k]]
      if (!is.null(v) && nzchar(v) && v %in% names(df)) keep <- c(keep, v)
    }
    unique(keep)
  }

  output$data_table <- DT::renderDT({
    df <- df_work()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    cols <- mapped_columns(df)
    if (length(cols) == 0) df else df[, cols, drop = FALSE]
  }, options = list(pageLength = 25, scrollX = TRUE, lengthMenu = c(10, 25, 50, 100, 250)),
     rownames = FALSE, filter = "top")

  run_ttest_for_df <- function(df) {
    cols <- names(df)
    y <- input$ttest_y
    g <- input$ttest_group_col
    a_lab <- input$ttest_group_a
    b_lab <- input$ttest_group_b

    if (is.null(y) || !nzchar(y) || !(y %in% cols)) return(list(err = "Pick a response (Y) for the t-test."))
    if (is.null(g) || !nzchar(g) || !(g %in% cols)) return(list(err = "Pick a group column for the t-test."))
    if (is.null(a_lab) || !nzchar(a_lab) || is.null(b_lab) || !nzchar(b_lab)) {
      return(list(err = "Pick Group A and Group B."))
    }
    if (identical(a_lab, b_lab)) return(list(err = "Group A and Group B must be different."))

    df[[y]] <- suppressWarnings(as.numeric(df[[y]]))
    df <- df[!is.na(df[[y]]), , drop = FALSE]
    df <- df[as.character(df[[g]]) %in% c(a_lab, b_lab), , drop = FALSE]
    if (nrow(df) < 2) return(list(err = "Not enough non-NA values across the two groups (after filters)."))

    paired <- isTRUE(input$ttest_paired)
    var_equal <- identical(input$ttest_variance %||% "welch", "equal")
    alternative <- input$ttest_alternative %||% "two.sided"
    alpha <- input$ttest_alpha %||% 0.05
    conf_level <- 1 - alpha

    if (paired) {
      id_col <- input$idcol
      if (is.null(id_col) || !nzchar(id_col) || !(id_col %in% names(df))) {
        return(list(err = "Paired t-test needs an ID column (set 'ID column' in the Exclude section)."))
      }
      mask_a <- as.character(df[[g]]) == a_lab
      mask_b <- as.character(df[[g]]) == b_lab
      df_a <- data.frame(.id = df[[id_col]][mask_a], .y = df[[y]][mask_a], stringsAsFactors = FALSE)
      df_b <- data.frame(.id = df[[id_col]][mask_b], .y = df[[y]][mask_b], stringsAsFactors = FALSE)
      df_a <- aggregate(.y ~ .id, data = df_a, FUN = mean, na.rm = TRUE)
      df_b <- aggregate(.y ~ .id, data = df_b, FUN = mean, na.rm = TRUE)
      names(df_a)[2] <- ".y_a"
      names(df_b)[2] <- ".y_b"
      merged <- merge(df_a, df_b, by = ".id")
      if (nrow(merged) < 2) return(list(err = "Paired t-test needs at least 2 IDs present in both groups."))
      fit <- tryCatch(
        t.test(merged$.y_a, merged$.y_b, paired = TRUE, alternative = alternative, conf.level = conf_level),
        error = function(e) e
      )
      if (inherits(fit, "error")) return(list(err = paste("t-test failed:", fit$message)))
      return(list(
        fit = fit, n_a = nrow(merged), n_b = nrow(merged),
        paired = TRUE, method = "Paired t-test",
        a_lab = a_lab, b_lab = b_lab, y = y, g = g, alpha = alpha
      ))
    }

    a_vals <- df[[y]][as.character(df[[g]]) == a_lab]
    b_vals <- df[[y]][as.character(df[[g]]) == b_lab]
    if (length(a_vals) < 2 || length(b_vals) < 2) {
      return(list(err = "Each group needs at least 2 non-NA values."))
    }
    fit <- tryCatch(
      t.test(a_vals, b_vals, var.equal = var_equal, alternative = alternative, conf.level = conf_level),
      error = function(e) e
    )
    if (inherits(fit, "error")) return(list(err = paste("t-test failed:", fit$message)))

    list(
      fit = fit, n_a = length(a_vals), n_b = length(b_vals),
      paired = FALSE,
      method = if (var_equal) "Two-sample t-test (pooled variance)" else "Welch two-sample t-test",
      a_lab = a_lab, b_lab = b_lab, y = y, g = g, alpha = alpha
    )
  }

  print_ttest_res <- function(res) {
    if (!is.null(res$err)) { cat(res$err, "\n"); return(invisible(NULL)) }
    cat("Method:    ", res$method, "\n", sep = "")
    cat("Response:  ", res$y, "  |  Group column: ", res$g, "\n", sep = "")
    cat("Group A:   ", res$a_lab, " (n = ", res$n_a, ")\n", sep = "")
    cat("Group B:   ", res$b_lab, " (n = ", res$n_b, ")\n\n", sep = "")
    print(res$fit)
    sig <- isTRUE(res$fit$p.value < res$alpha)
    cat("\nSignificant at alpha = ", res$alpha, ": ", if (sig) "YES" else "NO", "\n", sep = "")
  }

  output$ttest_out <- renderPrint({
    if (!isTRUE(input$do_stats) || !isTRUE(input$do_ttest)) {
      cat("t-test is off (toggle 'Enable stats' and 'Run t-test' in the sidebar).\n")
      return(invisible(NULL))
    }
    df <- df_work()
    if (is.null(df) || nrow(df) < 2) {
      cat("Not enough data after filters.\n")
      return(invisible(NULL))
    }
    print_ttest_res(run_ttest_for_df(df))
  })

  anova_results <- reactive({
    req(isTRUE(input$do_anova))
    df <- df_work()
    req(nrow(df) > 1)

    if (length(get_anova_factors()) < 1) return(list(err = "Pick at least 1 factor (Factor A)."))
    fml <- build_anova_formula(df)
    if (is.null(fml)) return(list(err = "Could not build ANOVA formula."))

    prep <- prep_anova_data(df, fml)
    df <- prep$df
    factors <- prep$factors
    blocks <- get_anova_blocks(names(df))
    if (nrow(df) < 2) return(list(err = "Not enough non-NA Y values."))

    anova_method <- input$anova_method %||% "regular"
    if (identical(anova_method, "regular")) {
      fit <- tryCatch(aov(fml, data = df), error = function(e) e)
      if (inherits(fit, "error")) return(list(err = paste("ANOVA failed:", fit$message)))

      tuk <- NULL
      if (isTRUE(input$do_tukey)) {
        tuk_all <- tryCatch(TukeyHSD(fit), error = function(e) e)
        if (inherits(tuk_all, "error")) {
          tuk <- list(error = tuk_all$message)
        } else {
          terms <- names(tuk_all)
          term <- choose_preferred_tukey_term(
            terms = terms,
            current = input$tukey_term,
            xcol = input$xcol,
            fillcol = input$fillcol
          )

          if (is.null(term) || !(term %in% terms)) {
            tuk <- list(error = paste0("No valid Tukey term available. Terms: ", paste(terms, collapse = ", ")))
          } else {
            alpha <- input$tukey_alpha
            mat <- as.data.frame(tuk_all[[term]])
            mat$Comparison <- rownames(mat)
            if (isTRUE(input$tukey_sig_only)) mat <- mat[mat$`p adj` < alpha, , drop = FALSE]
            tuk <- list(filtered = mat, which = term, raw = tuk_all)
          }
        }
      }

      return(list(formula = fml, summary = summary(fit), tukey = tuk, method = "regular"))
    }

    if (length(blocks) > 0) {
      return(list(err = "Multi-way Welch ANOVA does not support block terms in this app. Set blocks to 0.", method = "welch"))
    }

    if (length(factors) == 1) {
      welch_fit <- tryCatch(oneway.test(fml, data = df, var.equal = FALSE), error = function(e) e)
      if (inherits(welch_fit, "error")) return(list(err = paste("Welch ANOVA failed:", welch_fit$message), method = "welch"))
      return(list(formula = fml, summary = welch_fit, tukey = NULL, method = "welch"))
    }

    if (!requireNamespace("welchADF", quietly = TRUE)) {
      return(list(
        err = "Multi-way Welch ANOVA requires the 'welchADF' package. Install it to run Welch with 2+ factors.",
        method = "welch"
      ))
    }

    welch_fit <- tryCatch(welchADF::welchADF.test(fml, data = df), error = function(e) e)
    if (inherits(welch_fit, "error")) {
      return(list(err = paste("Multi-way Welch ANOVA failed:", welch_fit$message), method = "welch"))
    }

    list(formula = fml, summary = welch_fit, tukey = NULL, method = "welch")
  })

  output$dynamic_filters <- renderUI({
    req(rv$df)
    nF <- input$n_filters
    if (is.null(nF) || nF == 0) return(NULL)

    cols <- names(rv$df)
    req(length(cols) > 0)

    filter_list <- lapply(seq_len(nF), function(i) {
      col_id <- paste0("filter_col_", i)
      mode_id <- paste0("filter_mode_", i)

      selected_col <- isolate(safe_list_get(rv_filters$cols, i))
      if (is.null(selected_col) || !(selected_col %in% cols)) selected_col <- cols[1]

      selected_mode <- isolate(safe_list_get(rv_filters$modes, i))
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

  render_anova_for_df <- function(dfi) {
    if (!isTRUE(input$do_anova)) {
      cat("ANOVA is off (toggle 'Run ANOVA' in the sidebar).\n")
      return(invisible(NULL))
    }
    fml <- build_anova_formula(dfi)
    if (is.null(fml)) { cat("Pick at least 1 factor.\n"); return(invisible(NULL)) }

    prep <- prep_anova_data(dfi, fml)
    dfi <- prep$df
    if (nrow(dfi) < 2) { cat("Not enough non-NA Y values for this panel.\n"); return(invisible(NULL)) }

    fit <- tryCatch(aov(fml, data = dfi), error = function(e) e)
    if (inherits(fit, "error")) { cat("ANOVA failed:", fit$message, "\n"); return(invisible(NULL)) }

    cat("Model formula:\n"); print(fml)
    cat("\nANOVA summary:\n"); print(summary(fit))

    if (isTRUE(input$do_tukey)) {
      term <- input$tukey_term
      cat("\nTukey HSD:\n")
      tuk0 <- tryCatch(TukeyHSD(fit, which = term), error = function(e) e)
      if (inherits(tuk0, "error")) { cat("Tukey error:", tuk0$message, "\n"); return(invisible(NULL)) }
      if (!(term %in% names(tuk0))) {
        cat("Tukey term not found in model:", term, "\nAvailable terms:", paste(names(tuk0), collapse = ", "), "\n")
        return(invisible(NULL))
      }
      mat <- as.data.frame(tuk0[[term]])
      mat$Comparison <- rownames(mat)
      alpha <- input$tukey_alpha %||% 0.05
      if (isTRUE(input$tukey_sig_only)) mat <- mat[mat$`p adj` < alpha, , drop = FALSE]
      cat("Term:", term, "\n")
      print(mat)
    }
  }

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
    method_label <- if (identical(res$method, "welch")) "Welch ANOVA summary" else "ANOVA summary"
    cat("\n", method_label, ":\n", sep = "")
    print(res$summary)

    if (isTRUE(input$do_tukey) && !identical(res$method, "welch")) {
      cat("\nTukey HSD:\n")
      if (is.null(res$tukey)) {
        cat("No Tukey results.\n")
      } else if (!is.null(res$tukey$error)) {
        cat("Tukey error:", res$tukey$error, "\n")
      } else {
        cat("Term:", res$tukey$which, "\n")
        print(res$tukey$filtered)
      }
    } else if (isTRUE(input$do_tukey) && identical(res$method, "welch")) {
      cat("\nTukey HSD skipped: not available for Welch ANOVA in this app.\n")
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

    ymin_v <- input$ymin %||% 0
    ymax_v <- input$ymax %||% 30
    yrange <- ymax_v - ymin_v
    if (is.na(yrange) || yrange == 0) yrange <- 1

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
          y_top <- ymax_v
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
      jitter_pos <- if (nzchar(input$fillcol) || nzchar(input$shapecol)) {
        position_jitterdodge(jitter.width = input$jitter_width, dodge.width = input$dodge_width)
      } else {
        position_jitter(width = input$jitter_width)
      }

      p <- p + geom_jitter(
        alpha = 0.7,
        size = 2,
        position = jitter_pos
      )
    }

    if (isTRUE(input$do_stats) && isTRUE(input$do_anova) && isTRUE(input$do_tukey) && isTRUE(input$show_tukey_letters)) {
      res <- tryCatch(anova_results(), error = function(e) NULL)
      if (!is.null(res) && is.null(res$err) && !is.null(res$tukey) && !is.null(res$tukey$raw)) {
        term <- res$tukey$which
        if (term_includes_var(term, input$xcol)) {
          term_vars <- strsplit(term, ":", fixed = TRUE)[[1]]
          term_vars <- vapply(term_vars, normalize_term_token, character(1))

          missing_vars <- setdiff(term_vars, names(df))
          if (length(missing_vars) > 0) {
            showNotification(
              paste0("Cannot draw Tukey letters for term '", term, "' because these term columns are not in the plot data: ", paste(missing_vars, collapse = ", "), "."),
              type = "warning",
              duration = 6
            )
          } else {
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
              pos_df$ypos <- pos_df$ypos + 0.05 * yrange

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
        } else {
          showNotification(
            paste0("Tukey letters are only drawn when the selected Tukey term includes X column ('", input$xcol, "')."),
            type = "message",
            duration = 4
          )
        }
      }
    }

    relabel_map <- parse_relabel_map(input$level_relabel)
    relabel <- function(x) {
      x <- as.character(x)
      hits <- x %in% names(relabel_map)
      x[hits] <- unname(relabel_map[x[hits]])
      x
    }

    fr <- if (nzchar(input$facet_row)) input$facet_row else "."
    fc <- if (nzchar(input$facet_col)) input$facet_col else "."
    if (fr != "." || fc != ".") {
      p <- p + facet_grid(
        as.formula(paste(fr, "~", fc)),
        scales = if (isTRUE(input$split_free_y)) "free_y" else "fixed",
        labeller = labeller(.default = relabel)
      )
    }

    base_size <- 11 * input$text_scale
    xlab <- if (nzchar(input$xlabel_custom)) input$xlabel_custom else input$xcol
    ylab <- if (nzchar(input$ylabel_custom)) input$ylabel_custom else input$ycol

    fill_title <- if (nzchar(input$legend_title_fill %||% "")) input$legend_title_fill
                  else if (nzchar(input$fillcol) && input$fillcol == "group") "Group"
                  else if (nzchar(input$fillcol)) input$fillcol
                  else NULL
    shape_title <- if (nzchar(input$legend_title_shape %||% "")) input$legend_title_shape
                   else if (nzchar(input$shapecol)) input$shapecol
                   else NULL

    p <- p +
      scale_x_discrete(drop = FALSE, labels = relabel) +
      coord_cartesian(ylim = c(ymin_v, ymax_v)) +
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
        fill = fill_title,
        shape = shape_title
      )

    if (nzchar(input$shapecol)) p <- p + scale_shape_discrete(labels = relabel)

    if (nzchar(input$fillcol) && isTRUE(input$custom_fill_colors) && nzchar(input$fill_colors)) {
      cols <- strsplit(input$fill_colors, ",", fixed = TRUE)[[1]] |> trimws()
      cols <- cols[nzchar(cols)]
      if (length(cols) > 0) {
        n_levels <- nlevels(df[[input$fillcol]])
        if (length(cols) < n_levels) cols <- rep(cols, length.out = n_levels)
        p <- p + scale_fill_manual(values = cols[seq_len(n_levels)], labels = relabel)
      }
    } else if (nzchar(input$fillcol)) {
      p <- p + scale_fill_discrete(labels = relabel)
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
          req(col, rv$df)
          req(col %in% names(rv$df))

          vals <- unique(as.character(rv$df[[col]]))
          vals <- sort(vals[!is.na(vals)])

          selected_vals <- coalesce_chr(
            isolate(input[[paste0("filter_val_", ii)]]),
            isolate(safe_list_get(rv_filters$vals, ii))
          )
          selected_vals <- selected_vals[selected_vals %in% vals]

          selectInput(
            paste0("filter_val_", ii),
            "Values",
            choices = vals,
            selected = selected_vals,
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
    req(input$plot_height, input$plot_width)
    plot_w <- paste0(input$plot_width * 72, "px")
    plot_h <- paste0(input$plot_height * 72, "px")

    split_active <- !is.null(input$split_col) && nzchar(input$split_col)
    df_split <- if (split_active) df_work() else NULL
    has_split <- split_active && !is.null(df_split) && input$split_col %in% names(df_split)

    stats_help <- tags$small(
      "How to interpret:",
      tags$ul(
        tags$li("Equal Variance (Fligner/Bartlett): p ≥ 0.05 → variances equal (ANOVA appropriate). p < 0.05 → variances unequal (consider Welch ANOVA)."),
        tags$li("Normality (Shapiro-Wilk): p ≥ 0.05 → normality OK. p < 0.05 → residuals deviate from normal."),
        tags$li("Outliers: |standardized residual| > 3 suggests influential observations.")
      ),
      "Most important: independence > equal variance > normality."
    )

    if (!has_split) {
      plot_tab <- tagList(
        plotOutput("plot", width = plot_w, height = plot_h),
        tags$hr(),
        h4("Assumption checks"),
        verbatimTextOutput("assump_out"),
        stats_help,
        tags$hr(),
        h4("t-test"),
        verbatimTextOutput("ttest_out"),
        tags$hr(),
        h4("ANOVA / Tukey"),
        verbatimTextOutput("anova_out")
      )
      desc_tab <- tagList(
        helpText("Summary of Y grouped by X (and Fill / facets if set), after current filters."),
        tableOutput("desc_table")
      )
    } else {
      lvls <- unique(as.character(df_split[[input$split_col]]))
      lvls <- lvls[!is.na(lvls) & nzchar(lvls)]

      plot_tab <- tagList(
        stats_help,
        tags$hr(),
        lapply(seq_along(lvls), function(i) {
          tagList(
            h4(paste0(input$split_col, " = ", lvls[i])),
            plotOutput(outputId = paste0("plot_", i), width = plot_w, height = plot_h),
            h5("Assumption checks"),
            verbatimTextOutput(paste0("assump_out_", i)),
            h5("t-test"),
            verbatimTextOutput(paste0("ttest_out_", i)),
            h5("ANOVA / Tukey"),
            verbatimTextOutput(paste0("anova_out_", i)),
            tags$hr()
          )
        })
      )

      desc_tab <- tagList(
        helpText("Summary of Y grouped by X (and Fill / facets if set), per split panel."),
        lapply(seq_along(lvls), function(i) {
          tagList(
            h4(paste0(input$split_col, " = ", lvls[i])),
            tableOutput(paste0("desc_table_", i)),
            tags$hr()
          )
        })
      )
    }

    data_tab <- tagList(
      helpText("Filtered dataset feeding the plot — only the columns selected in Map fields / Experiment ID / Split, after current filters and ID keep list."),
      DT::DTOutput("data_table")
    )

    tabsetPanel(
      id = "main_tabs",
      tabPanel("Plot & stats", plot_tab),
      tabPanel("Descriptive", desc_tab),
      tabPanel("Data", data_tab)
    )
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

        output[[paste0("ttest_out_", ii)]] <- renderPrint({
          if (!isTRUE(input$do_stats) || !isTRUE(input$do_ttest)) {
            cat("t-test is off.\n")
            return(invisible(NULL))
          }
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          if (is.null(dfi) || nrow(dfi) < 2) {
            cat("Not enough data in this panel.\n")
            return(invisible(NULL))
          }
          print_ttest_res(run_ttest_for_df(dfi))
        })

        output[[paste0("anova_out_", ii)]] <- renderPrint({
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          render_anova_for_df(dfi)
        })

        output[[paste0("desc_table_", ii)]] <- renderTable({
          dfi <- df %>% filter(as.character(.data[[input$split_col]]) == lvl)
          compute_descriptive_stats(dfi)
        }, digits = 3, striped = TRUE, hover = TRUE, na = "—")
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
