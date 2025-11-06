#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import shiny
#' @import bslib
#' @importFrom irr kripp.alpha kappam.fleiss
#' @importFrom stats na.omit
#' @export

# Declare global variables for dplyr NSE
utils::globalVariables(c("unit_id", "coder_id", "code"))
# -------------------------------
# Helpers
# -------------------------------
na_to_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}
`%||%` <- function(x, y) if (length(x) == 0) y else x

read_data_file <- function(path, name) {
  if (grepl("\\.rds$", name, ignore.case = TRUE)) {
    readRDS(path)
  } else if (grepl("\\.csv$", name, ignore.case = TRUE)) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported file type. Please select a .rds or .csv file.")
  }
}

preview_head <- function(df, n = 10) utils::head(df, n)

make_long_icr <- function(df, unit_id_col, coder_cols) {
  stopifnot(unit_id_col %in% names(df), all(coder_cols %in% names(df)))
  df %>%
    mutate(unit_id = as.character(.data[[unit_id_col]])) %>%
    select(unit_id, all_of(coder_cols)) %>%
    pivot_longer(cols = all_of(coder_cols), names_to = "coder_id", values_to = "code") %>%
    mutate(coder_id = as.character(coder_id), code = as.character(code)) %>%
    group_by(unit_id, coder_id) %>%
    summarise(code = dplyr::first(code[!is.na(code)] %||% NA_character_), .groups = "drop") %>%
    tidyr::complete(unit_id, coder_id, fill = list(code = NA_character_))
}

filter_units_by_coders <- function(long_df, min_coders = 2L) {
  long_df %>% group_by(unit_id) %>% filter(sum(!is.na(code)) >= min_coders) %>% ungroup()
}

compute_icr_summary <- function(long_df) {
  wide <- long_df %>% pivot_wider(names_from = coder_id, values_from = code) %>% arrange(unit_id)
  if (!"unit_id" %in% names(wide)) {
    return(data.frame(metric = character(), value = character(), stringsAsFactors = FALSE))
  }
  ratings_raw <- wide %>% select(-unit_id)
  n_units <- nrow(ratings_raw); n_coders <- ncol(ratings_raw)
  if (n_units == 0L || n_coders < 2L) {
    return(data.frame(
      metric = c("units_included", "coders", "kripp_alpha_nominal", "fleiss_kappa"),
      value = c(n_units, n_coders, NA, NA),
      stringsAsFactors = FALSE
    ))
  }
  all_levels <- sort(unique(na.omit(unlist(ratings_raw))))
  ratings_fac <- as.data.frame(lapply(ratings_raw, function(col) factor(col, levels = all_levels)))
  ratings_int <- as.data.frame(lapply(ratings_fac, function(col) as.integer(col)))
  alpha_val <- tryCatch({
    rmat <- t(as.matrix(ratings_int))
    irr::kripp.alpha(rmat, method = "nominal")$value
  }, error = function(e) NA_real_)
  fleiss_val <- tryCatch({
    comp <- ratings_int[stats::complete.cases(ratings_int), , drop = FALSE]
    if (nrow(comp) >= 2L && length(all_levels) >= 2L) irr::kappam.fleiss(comp)$value else NA_real_
  }, error = function(e) NA_real_)
  data.frame(
    metric = c("units_included", "coders", "kripp_alpha_nominal", "fleiss_kappa"),
    value = c(n_units, n_coders, round(alpha_val, 4), round(fleiss_val, 4)),
    stringsAsFactors = FALSE
  )
}

# -------------------------------
# Human check module (UI + server)
# -------------------------------
humancheck_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        :root { --max-panel-height: 900px; --bluish: #5A6F8F; }
        #sidebar-box, #main-text-box { overflow-y: auto; max-height: var(--max-panel-height); padding-right: 10px; }
        .btn-primary { background-color: var(--bluish) !important; border-color: var(--bluish) !important; }
        .btn-secondary { background-color: #6c757d !important; border-color: #6c757d !important; color: #fff !important; }
        #main-text { font-size: 0.9rem; background: #f8f9fa; border-radius: 8px; padding: 10px; }
        .small-box { max-height: 220px; overflow-y: auto; background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 6px; margin-bottom: 6px; }
        .progress-bar { height: 5px; background: var(--bluish); border-radius: 3px; margin-bottom: 6px; }
      ")),
      tags$script(HTML(sprintf("
        document.addEventListener('keydown', function(e) {
          if (e.key === 'ArrowRight') Shiny.setInputValue('%s', Math.random());
          if (e.key === 'ArrowLeft') Shiny.setInputValue('%s', Math.random());
        });
      ", ns("next_text"), ns("prev_text"))))
    ),
    fluidRow(
      column(
        width = 4,
        div(
          id = "sidebar-box",
          uiOutput(ns("meta_ui")),
          hr(),
          uiOutput(ns("progress_ui")),
          fluidRow(
            column(4, actionButton(ns("prev_text"), "<- Previous", class = "btn btn-light w-100")),
            column(4, actionButton(ns("next_text"), "Next ->", class = "btn btn-light w-100")),
            column(4, actionButton(ns("manual_save"), "Save Now", class = "btn btn-secondary w-100"))
          ),
          hr(),
          uiOutput(ns("llm_side")),     # LLM output / justification (LLM mode)
          uiOutput(ns("mode_fields")),  # Score (manual) or Status (LLM)
          textAreaInput(ns("comments"), "Comments", "", rows = 3, placeholder = "Type your notes..."),
          textAreaInput(ns("examples"), "Examples (optional)", "", rows = 3, placeholder = "Paste or type examples..."),
          verbatimTextOutput(ns("status_msg"))
        )
      ),
      column(
        width = 8,
        div(
          id = "main-text-box",
          uiOutput(ns("text_ui"))
        )
      )
    )
  )
}

humancheck_server <- function(
    id,
    data,
    text_col,
    blind = reactive(TRUE),
    llm_output_col = reactive(NULL),
    llm_evidence_col = reactive(NULL), # internally named evidence; UI shows justification
    original_file_name = reactive("data.csv"),
    meta_cols = reactive(character())
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(df = NULL, n = 0L, text_vec = NULL)
    current_index <- reactiveVal(1L)

    assessed_path <- reactive({
      base_path <- req(original_file_name())
      out_dir <- dirname(base_path) # always the agreement folder (uploads saved there)
      file.path(out_dir, paste0(tools::file_path_sans_ext(basename(base_path)), "_assessed.rds"))
    })

    # Atomic save of full dataframe + last_index
    save_now <- function() {
      if (is.null(rv$df)) return(invisible(NULL))
      sp <- tryCatch(assessed_path(), error = function(e) NULL)
      if (is.null(sp) || !nzchar(sp)) return(invisible(NULL))
      dir.create(dirname(sp), recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile("assessed_", tmpdir = dirname(sp), fileext = ".rds")
      payload <- list(
        version = 4L,
        last_index = current_index(),
        df = rv$df
      )
      ok <- tryCatch({
        saveRDS(payload, tmp, compress = "xz")
        if (!file.rename(tmp, sp)) {
          if (file.copy(tmp, sp, overwrite = TRUE)) unlink(tmp) else stop("copy failed")
        }
        TRUE
      }, error = function(e) FALSE)
      if (!ok && file.exists(tmp)) try(unlink(tmp), silent = TRUE)
      invisible(NULL)
    }

    # Load/merge progress when inputs change
    observeEvent(list(data(), text_col(), blind(), llm_output_col(), llm_evidence_col(), original_file_name(), meta_cols()), {
      df <- req(data())
      txt <- req(text_col())
      if (!is.data.frame(df)) return()
      if (!txt %in% names(df)) {
        showNotification(sprintf("Text column '%s' not found in data.", txt), type = "error", duration = 5)
        return()
      }
      n <- nrow(df)

      # Ensure IDs and columns exist
      if (!".row_id" %in% names(df)) df$.row_id <- sprintf("row_%s", seq_len(n))

      # Split comments/examples by mode; keep legacy columns if present
      if (!"comments_manual" %in% names(df)) df$comments_manual <- ""
      if (!"examples_manual" %in% names(df)) df$examples_manual <- ""
      if (!"comments_llm" %in% names(df)) df$comments_llm <- ""
      if (!"examples_llm" %in% names(df)) df$examples_llm <- ""
      df$comments_manual <- na_to_empty(df$comments_manual)
      df$examples_manual <- na_to_empty(df$examples_manual)
      df$comments_llm    <- na_to_empty(df$comments_llm)
      df$examples_llm    <- na_to_empty(df$examples_llm)

      # Legacy migration (if generic columns exist)
      if ("comments" %in% names(df)) {
        df$comments_manual <- ifelse(nzchar(df$comments_manual), df$comments_manual, na_to_empty(df$comments))
        df$comments_llm    <- ifelse(nzchar(df$comments_llm),    df$comments_llm,    na_to_empty(df$comments))
      }
      if ("examples" %in% names(df)) {
        df$examples_manual <- ifelse(nzchar(df$examples_manual), df$examples_manual, na_to_empty(df$examples))
        df$examples_llm    <- ifelse(nzchar(df$examples_llm),    df$examples_llm,    na_to_empty(df$examples))
      }

      if (!"status" %in% names(df)) df$status <- rep("Unmarked", n) else df$status <- na_to_empty(df$status)
      if (!"score"  %in% names(df)) df$score  <- rep(NA_real_, n)

      # Merge saved progress
      saved_last <- NA_integer_
      sp <- tryCatch(assessed_path(), error = function(e) NULL)
      if (!is.null(sp) && file.exists(sp)) {
        saved <- tryCatch(readRDS(sp), error = function(e) NULL)
        saved_df <- NULL
        if (is.list(saved) && "df" %in% names(saved)) saved_df <- saved$df
        if (is.data.frame(saved)) saved_df <- saved
        if (!is.null(saved_df) && ".row_id" %in% names(saved_df)) {
          keep <- intersect(c(
            ".row_id",
            "comments_manual","examples_manual",
            "comments_llm","examples_llm",
            "comments","examples",  # legacy fallback
            "status","score"
          ), names(saved_df))
          m <- dplyr::left_join(df, saved_df[, keep, drop = FALSE], by = ".row_id", suffix = c("", ".saved"))
          df$comments_manual <- na_to_empty(dplyr::coalesce(m$comments_manual.saved, df$comments_manual, m$comments.saved))
          df$examples_manual <- na_to_empty(dplyr::coalesce(m$examples_manual.saved, df$examples_manual, m$examples.saved))
          df$comments_llm    <- na_to_empty(dplyr::coalesce(m$comments_llm.saved,    df$comments_llm,    m$comments.saved))
          df$examples_llm    <- na_to_empty(dplyr::coalesce(m$examples_llm.saved,    df$examples_llm,    m$examples.saved))
          df$status          <- na_to_empty(dplyr::coalesce(m$status.saved, df$status))
          if ("score.saved" %in% names(m)) df$score <- dplyr::coalesce(m$score.saved, df$score)
        }
        if (is.list(saved) && is.numeric(saved$last_index)) saved_last <- as.integer(saved$last_index)
      }

      rv$df <- df
      rv$n <- n
      rv$text_vec <- as.character(df[[txt]])

      # Start index (mode-aware)
      if (isTRUE(blind())) {
        coded <- nzchar(df$comments_manual) | nzchar(df$examples_manual) | !is.na(df$score)
      } else {
        coded <- nzchar(df$comments_llm) | nzchar(df$examples_llm) | !(df$status %in% c("", "Unmarked"))
      }
      start_idx <- if (is.finite(saved_last) && !is.na(saved_last)) max(1L, min(saved_last, n)) else if (any(coded)) max(which(coded)) else 1L
      move_and_refresh(start_idx)
    }, ignoreInit = FALSE, priority = 10)

    # LLM side panel in sidebar
    output$llm_side <- renderUI({
      req(rv$df)
      if (isTRUE(blind())) return(NULL)
      out_col <- llm_output_col()
      just_col  <- llm_evidence_col()
      tagList(
        h5("LLM output / justification"),
        if (!is.null(out_col) && nzchar(out_col) && out_col %in% names(rv$df)) {
          div(class = "small-box", strong("LLM output:"), verbatimTextOutput(ns("llm_output")))
        } else {
          div(class = "small-box", strong("LLM output:"), span("Select a valid LLM output column.", style = "color:#dc3545"))
        },
        if (!is.null(just_col) && nzchar(just_col) && !identical(just_col, "None") && just_col %in% names(rv$df)) {
          div(class = "small-box", strong("LLM justification:"), verbatimTextOutput(ns("llm_justification")))
        } else NULL
      )
    })
    output$llm_output <- renderText({
      if (isTRUE(blind())) return("")
      col <- llm_output_col()
      if (is.null(col) || !nzchar(col) || !(col %in% names(rv$df))) return("")
      i <- current_index(); na_to_empty(rv$df[[col]][i])
    })
    output$llm_justification <- renderText({
      if (isTRUE(blind())) return("")
      col <- llm_evidence_col()
      if (is.null(col) || !nzchar(col) || identical(col, "None") || !(col %in% names(rv$df))) return("")
      i <- current_index(); na_to_empty(rv$df[[col]][i])
    })

    # Mode-specific fields (score vs status)
    output$mode_fields <- renderUI({
      if (isTRUE(blind())) {
        numericInput(ns("score"), "Score", value = NA, step = 1)
      } else {
        radioButtons(ns("status_sel"), "Status", choices = c("Valid","Invalid","Unmarked"), selected = "Unmarked", inline = TRUE)
      }
    })

    # Meta table
    output$meta_ui <- renderUI({
      req(rv$df)
      cols <- meta_cols()
      if (length(cols) == 0) return(NULL)
      i <- current_index()
      df <- rv$df
      tags$div(
        h5("Metadata"),
        tags$table(
          class = "table table-sm table-bordered",
          do.call(tags$tbody, lapply(cols, function(cn) {
            tags$tr(tags$th(cn), tags$td(na_to_empty(df[[cn]][i])))
          }))
        )
      )
    })

    # Text + progress
    output$text_ui <- renderUI({
      req(rv$text_vec, rv$n)
      tagList(
        tags$div(class = "progress-bar", style = sprintf("width:%s%%;", round(100 * current_index() / max(1, rv$n), 1))),
        tags$div(id = "main-text", rv$text_vec[current_index()])
      )
    })

    # Sync UI to row (mode-aware comments/examples)
    move_and_refresh <- function(to) {
      if (is.null(rv$n) || is.na(to)) return()
      to <- max(1L, min(to, rv$n))
      current_index(to)
      i <- to
      if (isTRUE(blind())) {
        updateTextAreaInput(session, "comments", value = na_to_empty(rv$df$comments_manual[i]))
        updateTextAreaInput(session, "examples", value = na_to_empty(rv$df$examples_manual[i]))
        updateNumericInput(session, "score", value = suppressWarnings(as.numeric(rv$df$score[i])))
      } else {
        updateTextAreaInput(session, "comments", value = na_to_empty(rv$df$comments_llm[i]))
        updateTextAreaInput(session, "examples", value = na_to_empty(rv$df$examples_llm[i]))
        updateRadioButtons(session, "status_sel", selected = na_to_empty(rv$df$status[i] %||% "Unmarked"))
      }
      output$progress_ui <- renderUI({
        p(sprintf("Progress: %d / %d (%.1f%%)", current_index(), rv$n, 100 * current_index() / max(1, rv$n)))
      })
    }

    # Persist edits (mode-aware)
    observeEvent(input$comments, {
      if (is.null(rv$df)) return()
      i <- current_index()
      if (isTRUE(blind())) {
        rv$df$comments_manual[i] <- na_to_empty(input$comments %||% "")
      } else {
        rv$df$comments_llm[i] <- na_to_empty(input$comments %||% "")
      }
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$examples, {
      if (is.null(rv$df)) return()
      i <- current_index()
      if (isTRUE(blind())) {
        rv$df$examples_manual[i] <- na_to_empty(input$examples %||% "")
      } else {
        rv$df$examples_llm[i] <- na_to_empty(input$examples %||% "")
      }
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$score, {
      if (!isTRUE(blind()) || is.null(rv$df)) return()
      i <- current_index()
      rv$df$score[i] <- suppressWarnings(as.numeric(input$score))
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$status_sel, {
      if (isTRUE(blind()) || is.null(rv$df)) return()
      i <- current_index()
      rv$df$status[i] <- na_to_empty(input$status_sel %||% "Unmarked")
      save_now()
    }, ignoreInit = TRUE)

    # Navigation
    observeEvent(input$next_text, { if (!is.null(rv$n) && current_index() < rv$n) { save_now(); move_and_refresh(current_index() + 1L) } })
    observeEvent(input$prev_text, { if (!is.null(rv$n) && current_index() > 1L)  { save_now(); move_and_refresh(current_index() - 1L) } })

    observeEvent(input$manual_save, ignoreInit = TRUE, handlerExpr = save_now)

    list(current_index = reactive(current_index()))
  })
}

# -------------------------------
# Main App
# -------------------------------
agreement_app <- function() {
  ui <- fluidPage(
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      base_font = font_google("Inter", local = TRUE),
      heading_font = font_google("Inter", local = TRUE),
      code_font = font_collection("Fira Mono", "Consolas"),
      "font-size-base" = ".85rem",
      "primary" = "#5A6F8F"
    ),
    tags$head(tags$style(HTML("
      .app-title { font-size: 1.35rem; font-weight: 700; margin: 6px 0 12px 0; }
    "))),
    div(class = "app-title", strong("Agreement App")),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("1. Select a data file"),
        fileInput("file", "Choose a data file (.rds or .csv):", accept = c(".rds", ".csv")),
        div(style="margin-top:-6px; color:#6c757d;", textOutput("loaded_file_name")),
        actionButton("reset_btn", "Save and reset", class = "btn btn-secondary w-100"),
        hr(),
        h4("2. Choose mode"),
        radioButtons("mode", "Mode:",
                     choices = c(
                       "Manual coding (blind)" = "blind",
                       "Checking LLM outputs" = "llm",
                       "Calculate agreement scores" = "agreement"
                     ),
                     selected = "blind"
        ),
        hr(),
        uiOutput("column_selectors"),
        br(),
        helpText("Progress is saved to *_assessed.rds in a newly created folder called agreement")
      ),
      mainPanel(uiOutput("main_content"))
    )
  )

  server <- function(input, output, session) {
    dataset <- reactiveVal(NULL)
    last_file <- reactiveVal(NULL)

    state_path <- ".app_state.rds"
    hc <- NULL
    get_hc_index <- function() {
      if (is.null(hc)) return(NA_integer_)
      idx <- tryCatch(isolate(hc$current_index()), error = function(e) NA_integer_)
      if (is.null(idx)) NA_integer_ else as.integer(idx)
    }

    save_state <- function() {
      st <- list(
        last_file = tryCatch(isolate(last_file()), error = function(e) NULL),
        mode = input$mode,
        text_col = isolate(input$text_col),
        meta_cols = isolate(input$meta_cols),
        llm_output_col = isolate(input$llm_output_col),
        llm_evidence_col = isolate(input$llm_evidence_col),
        unit_id_col = isolate(input$unit_id_col),
        coder_cols = isolate(input$coder_cols),
        hc_last_index = get_hc_index()
      )
      try(saveRDS(st, state_path), silent = TRUE)
    }

    # Restore last file and mode
    observe({
      st <- if (file.exists(state_path)) tryCatch(readRDS(state_path), error = function(e) NULL) else NULL
      if (!is.null(st)) {
        if (!is.null(st$last_file) && file.exists(st$last_file) && is.null(dataset())) {
          obj <- tryCatch(read_data_file(st$last_file, basename(st$last_file)),
                          error = function(e) { showNotification(e$message, type = "error"); NULL })
          if (!is.null(obj) && is.data.frame(obj)) { dataset(obj); last_file(st$last_file) }
          if (!is.null(obj) && !is.data.frame(obj)) {
            showNotification("Wrong format: please upload a .rds or .csv that contains a data frame.", type = "error")
          }
        }
        if (!is.null(st$mode)) updateRadioButtons(session, "mode", selected = st$mode)
      } else if (is.null(dataset()) && file.exists(".last_file.txt")) {
        lf <- readLines(".last_file.txt", warn = FALSE)
        if (length(lf) == 1 && nzchar(lf) && file.exists(lf)) {
          obj <- tryCatch(read_data_file(lf, basename(lf)), error = function(e) NULL)
          if (!is.null(obj) && is.data.frame(obj)) { dataset(obj); last_file(lf) }
        }
      }
    })

    output$loaded_file_name <- renderText({
      p <- last_file(); if (is.null(p)) "No file loaded" else paste("Loaded:", basename(p))
    })

    # Persist upload locally to agreement folder and validate
    observeEvent(input$file, {
      req(input$file)
      dir.create("agreement", showWarnings = FALSE, recursive = TRUE)
      dest <- normalizePath(file.path("agreement", input$file$name), mustWork = FALSE)
      if (!isTRUE(file.copy(input$file$datapath, dest, overwrite = TRUE))) {
        showNotification("Could not persist uploaded file.", type = "error")
        return()
      }
      obj <- tryCatch(read_data_file(dest, basename(dest)),
                      error = function(e) { showNotification(e$message, type = "error"); NULL })
      if (is.null(obj)) return()
      if (!is.data.frame(obj)) {
        showNotification("Wrong format: please upload a .rds or .csv that contains a data frame.", type = "error")
        return()
      }
      dataset(obj)
      last_file(dest)
      writeLines(dest, ".last_file.txt")
      save_state()
    }, ignoreInit = TRUE)

    # Save and reset
    observeEvent(input$reset_btn, {
      if (file.exists(state_path)) try(unlink(state_path), silent = TRUE)
      if (file.exists(".last_file.txt")) try(unlink(".last_file.txt"), silent = TRUE)
      dataset(NULL); last_file(NULL)
      showNotification("App reset. You can load a new file now.", type = "message")
    })

    # Column selectors UI
    output$column_selectors <- renderUI({
      req(dataset())
      if (!is.data.frame(dataset())) return(NULL)
      cols <- names(dataset())
      st <- if (file.exists(state_path)) tryCatch(readRDS(state_path), error = function(e) NULL) else NULL
      mode <- input$mode
      tagList(
        h4("3. Select columns"),
        selectInput("text_col", "Text column:", choices = cols,
                    selected = {
                      sel <- if (!is.null(st) && !is.null(st$text_col)) st$text_col else NULL
                      if (!is.null(sel) && sel %in% cols) sel else cols[[1]]
                    }),
        if (mode == "blind") {
          selectInput("meta_cols", "Metadata columns (optional):", choices = cols, multiple = TRUE,
                      selected = if (!is.null(st) && !is.null(st$meta_cols)) intersect(st$meta_cols, cols) else NULL)
        } else if (mode == "llm") {
          tagList(
            selectInput("llm_output_col", "LLM output column (required):", choices = cols,
                        selected = {
                          sel <- if (!is.null(st) && !is.null(st$llm_output_col)) st$llm_output_col else NULL
                          if (!is.null(sel) && sel %in% cols) sel else cols[[1]]
                        }),
            selectInput("llm_evidence_col", "LLM justification column (optional):", choices = c("None", cols),
                        selected = {
                          sel <- if (!is.null(st) && !is.null(st$llm_evidence_col)) st$llm_evidence_col else "None"
                          if (!is.null(sel) && sel %in% c("None", cols)) sel else "None"
                        })
          )
        } else if (mode == "agreement") {
          tagList(
            selectInput("unit_id_col", "Unit ID column:", choices = cols,
                        selected = if (!is.null(st) && !is.null(st$unit_id_col) && st$unit_id_col %in% cols) st$unit_id_col else NULL),
            selectInput("coder_cols", "Coder columns (multiple):", choices = cols, multiple = TRUE,
                        selected = if (!is.null(st) && !is.null(st$coder_cols)) intersect(st$coder_cols, cols) else NULL)
          )
        }
      )
    })

    # Data preview
    output$data_preview <- renderTable({
      req(dataset()); if (!is.data.frame(dataset())) return(NULL); preview_head(dataset())
    })

    # Human check server
    observe({
      req(dataset(), is.data.frame(dataset()))
      mode <- input$mode
      txt <- input$text_col
      if (is.null(txt) || !(txt %in% names(dataset()))) return()
      if (mode == "llm") req(input$llm_output_col)
      hc <<- humancheck_server(
        id = "hc",
        data = reactive(dataset()),
        text_col = reactive(txt),
        blind = reactive(mode == "blind"),
        llm_output_col = reactive(if (mode == "llm") req(input$llm_output_col) else NULL),
        llm_evidence_col = reactive({
          if (mode == "llm") {
            col <- input$llm_evidence_col
            if (is.null(col) || identical(col, "None")) NULL else col
          } else NULL
        }),
        original_file_name = reactive({
          lf <- last_file()
          if (is.null(lf) || !nzchar(lf)) "agreement/unknown.csv" else lf
        }),
        meta_cols = reactive(if (mode == "blind") input$meta_cols else character())
      )
    })

    # Persist state on changes
    observeEvent(input$mode, save_state, ignoreInit = FALSE)
    observeEvent(input$text_col, save_state, ignoreInit = TRUE)
    observeEvent(input$meta_cols, save_state, ignoreInit = TRUE)
    observeEvent(input$llm_output_col, save_state, ignoreInit = TRUE)
    observeEvent(input$llm_evidence_col, save_state, ignoreInit = TRUE)
    observeEvent(input$unit_id_col, save_state, ignoreInit = TRUE)
    observeEvent(input$coder_cols, save_state, ignoreInit = TRUE)
    observeEvent({ if (!is.null(hc)) hc$current_index() }, save_state, ignoreInit = TRUE)

    # Main content UI
    output$main_content <- renderUI({
      if (is.null(dataset())) {
        tagList(
          h3("Welcome to the Agreement App"),
          p("Step 1: Choose a file"),
          p("Step 2: Select mode"),
          p("Step 3: Select appropriate columns."),
          hr(),
          h4("File Preview:"),
          tableOutput("data_preview")
        )
      } else if (!is.data.frame(dataset())) {
        tagList(
          h4("Loaded object is not a data frame."),
          p("Please upload a .rds or .csv containing a data frame.")
        )
      } else if (input$mode %in% c("blind", "llm")) {
        humancheck_ui("hc")
      } else if (input$mode == "agreement") {
        tagList(
          h4("Agreement scores:"),
          tableOutput("icr_summary"),
          downloadButton("export_icr", "Export agreement scores")
        )
      }
    })

    # Agreement mode
    icr_result <- reactive({
      req(dataset(), is.data.frame(dataset()), input$mode == "agreement")
      df <- dataset()
      unit_id <- input$unit_id_col
      coder_cols <- input$coder_cols
      if (is.null(unit_id) || !unit_id %in% names(df) ||
          is.null(coder_cols) || length(coder_cols) < 2 || !all(coder_cols %in% names(df))) {
        return(NULL)
      }
      long_data <- make_long_icr(df, unit_id_col = unit_id, coder_cols = coder_cols)
      long_filtered <- filter_units_by_coders(long_data, min_coders = 2L)
      if (nrow(long_filtered) == 0) {
        data.frame(metric = "message", value = "No units have at least 2 coders. Cannot compute agreement.",
                   stringsAsFactors = FALSE)
      } else {
        compute_icr_summary(long_filtered)
      }
    })

    output$icr_summary <- renderTable({
      req(input$mode == "agreement")
      res <- icr_result()
      if (is.null(res)) {
        data.frame(metric = "message",
                   value = "Select a unit ID and at least two coder columns.",
                   stringsAsFactors = FALSE)
      } else res
    }, striped = TRUE, bordered = TRUE)

    output$export_icr <- downloadHandler(
      filename = function() "icr_results.csv",
      content = function(file) {
        res <- icr_result()
        if (is.null(res)) {
          utils::write.csv(data.frame(metric="message", value="No valid selection.", stringsAsFactors=FALSE),
                           file, row.names = FALSE)
        } else {
          utils::write.csv(res, file, row.names = FALSE)
        }
      }
    )
  }

  shinyApp(ui, server)
}

# Run the app if executed directly
if (identical(environment(), globalenv()) && !length(commandArgs(trailingOnly = TRUE))) {
  agreement_app()
}
shinApp(ui=ui, server=server)
