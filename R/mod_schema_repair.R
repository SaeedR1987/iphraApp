#' Variable schema repair ("Repair Mappings") for the Data Standardization step.
#'
#' Shows an editable working copy of the selected Data object's
#' `..variable_schema` in a modal. `variable` and `value` are canonical and
#' read-only; the column aliases, allowed values and schema labels can be
#' edited, validated, saved back to the object, loaded from and exported to
#' CSV / Excel. A reference panel searches the imported tool (survey and
#' choices) to help decide on mappings.
#' @noRd

iphra_schema_langs <- c("en", "fr", "es", "ar")
iphra_schema_canonical <- c("variable", "value")
iphra_schema_editable <- c(
  "col_names", "allowed",
  paste0(rep(c("variable_label_", "value_label_"), times = length(iphra_schema_langs)),
         rep(iphra_schema_langs, each = 2))
)
iphra_schema_display <- c(iphra_schema_canonical, iphra_schema_editable)

#' Working copy of a schema: all columns kept as character, missing display
#' columns added so the label columns for every language exist.
#' @noRd
iphra_schema_working <- function(schema) {
  if (!is.data.frame(schema) || !nrow(schema)) return(NULL)
  w <- as.data.frame(schema, stringsAsFactors = FALSE)
  w[] <- lapply(w, function(x) {
    x <- as.character(x)
    x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
    x
  })
  for (cl in setdiff(iphra_schema_display, names(w))) w[[cl]] <- NA_character_
  w
}

iphra_schema_key <- function(df) {
  paste(df$variable, ifelse(is.na(df$value), "", df$value), sep = "\r")
}

#' Structural / formatting checks. Returns a character vector of issues.
#' @noRd
iphra_schema_issues <- function(w, canonical = NULL) {
  issues <- character(0)
  if (!is.data.frame(w) || !nrow(w)) return("The variable schema is empty.")
  miss <- setdiff(c("variable", "value", "col_names", "allowed"), names(w))
  if (length(miss)) {
    return(paste0("Missing required column(s): ", paste(miss, collapse = ", "), "."))
  }
  if (!is.null(canonical)) {
    if (nrow(w) != nrow(canonical) || !identical(iphra_schema_key(w), iphra_schema_key(canonical))) {
      issues <- c(issues, "The canonical variable/value columns were changed.")
    }
  }
  if (anyNA(w$variable)) {
    issues <- c(issues, sprintf("%d row(s) have an empty variable name.", sum(is.na(w$variable))))
  }
  dup <- duplicated(iphra_schema_key(w))
  if (any(dup)) {
    issues <- c(issues, sprintf("Duplicated variable/value rows: %s.",
                                paste(unique(w$variable[dup]), collapse = ", ")))
  }

  split_cell <- function(x) trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  cells <- w$col_names
  idx <- which(!is.na(cells))
  bad <- idx[vapply(idx, function(i) any(!nzchar(split_cell(cells[i]))) ||
                      grepl(",\\s*$|^\\s*,", cells[i]), logical(1))]
  if (length(bad)) {
    issues <- c(issues, sprintf(
      "'col_names' has empty entries (stray commas) for: %s.",
      paste(unique(w$variable[bad]), collapse = ", ")
    ))
  }
  issues
}

#' Apply the editable columns of `incoming` onto `base`, matched on
#' variable/value. Returns the updated copy and the number of unmatched rows.
#' @noRd
iphra_schema_apply <- function(base, incoming) {
  inc <- iphra_schema_working(incoming)
  if (is.null(inc) || !all(iphra_schema_canonical %in% names(inc))) {
    stop("The file must contain 'variable' and 'value' columns.")
  }
  pos <- match(iphra_schema_key(inc), iphra_schema_key(base))
  ok <- !is.na(pos)
  cols <- intersect(iphra_schema_editable, names(incoming))
  for (cl in cols) base[[cl]][pos[ok]] <- inc[[cl]][ok]
  list(data = base, unmatched = sum(!ok), updated = sum(ok))
}

iphra_schema_read_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    as.data.frame(readxl::read_excel(path, col_types = "text"))
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                    na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  }
}

#' Stages ("raw", "standardized", "clean") for which the Data object holds a
#' non-empty data frame.
#' @noRd
iphra_available_stages <- function(obj) {
  if (is.null(obj)) return(character(0))
  fields <- c(raw = "..raw_data", standardized = "..standardized_data", clean = "..clean_data")
  keep <- vapply(fields, function(f) {
    x <- tryCatch(obj$get(field = f), error = function(e) NULL)
    is.data.frame(x) && nrow(x) > 0
  }, logical(1))
  names(fields)[keep]
}

#' Column / unique-value summary of a data frame, optionally filtered by a
#' case-insensitive substring on the column name or any unique value.
#' @noRd
iphra_data_reference <- function(df, q = "", max_values = 50L) {
  q <- tolower(trimws(q %||% ""))
  rows <- lapply(names(df), function(cl) {
    u <- unique(as.character(df[[cl]]))
    u <- sort(u[!is.na(u) & nzchar(u)])
    name_hit <- !nzchar(q) || grepl(q, tolower(cl), fixed = TRUE)
    val_hit <- grepl(q, tolower(u), fixed = TRUE)
    if (!name_hit && !any(val_hit)) return(NULL)
    shown <- if (name_hit) u else c(u[val_hit], u[!val_hit])
    extra <- length(shown) - max_values
    txt <- paste(utils::head(shown, max_values), collapse = " | ")
    if (extra > 0) txt <- paste0(txt, " | (+", extra, " more)")
    data.frame(column = cl, n_unique = length(u), unique_values = txt,
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

iphra_schema_modal <- function(ns, tx, choices, selected) {
  shiny::modalDialog(
    title = tx("Repair Mappings"), size = "l", easyClose = FALSE,
    shiny::tags$style(".modal-dialog.modal-lg{width:96%;max-width:1700px;}"),
    shiny::tags$script(shiny::HTML("$(document).one('shown.bs.modal', function(){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 250); });")),
    phr_toolbar(
      shiny::div(
        style = "display:inline-block; min-width:300px;",
        shiny::selectInput(ns("rm_dataset"), label = NULL, choices = choices,
                           selected = selected, width = "100%")
      ),
      shiny::actionButton(ns("rm_validate"), tx("Validate schema"),
                          icon = shiny::icon("check"), class = "btn-default"),
      shiny::actionButton(ns("rm_save"), tx("Save schema"),
                          icon = shiny::icon("save"), class = "btn-primary"),
      shiny::actionButton(ns("rm_load"), tx("Load schema"),
                          icon = shiny::icon("upload"), class = "btn-default",
                          onclick = sprintf("document.getElementById('%s').click(); return false;", ns("rm_file"))),
      shiny::div(
        style = "display:none;",
        shiny::fileInput(ns("rm_file"), label = NULL, accept = c(".csv", ".xlsx", ".xls", ".xlsm"))
      ),
      shiny::div(
        style = "display:inline-block; width:90px;",
        shiny::selectInput(ns("rm_format"), label = NULL,
                           choices = c("Excel" = "xlsx", "CSV" = "csv"), width = "100%")
      ),
      shiny::downloadButton(ns("rm_export"), tx("Export schema"), class = "btn-default")
    ),
    shiny::uiOutput(ns("rm_status")),
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::uiOutput(ns("rm_empty")),
        shiny::textInput(ns("rm_filter"), label = NULL, width = "100%",
                         placeholder = tx("Filter by variable or value...")),
        rhandsontable::rHandsontableOutput(ns("rm_table"), width = "100%", height = "auto")
      ),
      shiny::column(
        4,
        shiny::tabsetPanel(
          shiny::tabPanel(
            tx("Tool reference"),
            shiny::br(),
            shiny::uiOutput(ns("rm_ref_ui")),
            rhandsontable::rHandsontableOutput(ns("rm_ref_table"), width = "100%", height = "auto")
          ),
          shiny::tabPanel(
            tx("Data reference"),
            shiny::br(),
            shiny::uiOutput(ns("rm_data_ui")),
            rhandsontable::rHandsontableOutput(ns("rm_data_table"), width = "100%", height = "auto")
          )
        )
      )
    ),
    footer = shiny::modalButton(tx("Close"))
  )
}

iphra_schema_repair_server <- function(input, output, session, iphra_session, datasets, tx) {
  ns <- session$ns
  ws <- shiny::reactiveVal(NULL)
  canonical <- shiny::reactiveVal(NULL)
  version <- shiny::reactiveVal(0L)
  view_idx <- shiny::reactiveVal(integer(0))
  status <- shiny::reactiveVal(NULL)
  tool_r <- iphra_session$get_module_reactive("imported_tool")

  set_status <- function(type, msgs) status(list(type = type, msgs = msgs))

  shiny::observeEvent(input$repair, {
    ds <- datasets()
    shiny::req(length(ds))
    choices <- stats::setNames(names(ds), vapply(names(ds), function(n) {
      paste0(n, " (", class(ds[[n]])[1], ")")
    }, character(1)))
    status(NULL)
    shiny::showModal(iphra_schema_modal(
      ns, tx, choices,
      if (!is.null(input$dataset) && input$dataset %in% names(ds)) input$dataset else names(ds)[1]
    ))
  })

  shiny::observeEvent(input$rm_dataset, {
    obj <- datasets()[[input$rm_dataset]]
    shiny::req(obj)
    sch <- tryCatch(obj$get(field = "..variable_schema"), error = function(e) NULL)
    w <- iphra_schema_working(sch)
    ws(w)
    canonical(if (is.null(w)) NULL else w[, iphra_schema_canonical, drop = FALSE])
    status(NULL)
    version(version() + 1L)
  })

  output$rm_status <- shiny::renderUI({
    s <- status()
    if (is.null(s)) return(NULL)
    cls <- switch(s$type, error = "alert-danger", success = "alert-success", "alert-warning")
    shiny::div(class = paste("alert", cls), style = "margin:8px 0; padding:8px 12px;",
               shiny::tags$ul(style = "margin:0; padding-left:18px;",
                              lapply(s$msgs, shiny::tags$li)))
  })

  output$rm_empty <- shiny::renderUI({
    version()
    if (!is.null(shiny::isolate(ws()))) return(NULL)
    phr_info_panel(tx("This dataset has no variable schema attached."),
                   title = tx("No variable schema"))
  })

  output$rm_table <- rhandsontable::renderRHandsontable({
    version()
    w <- shiny::isolate(ws())
    shiny::req(w)
    q <- trimws(input$rm_filter %||% "")
    idx <- seq_len(nrow(w))
    if (nzchar(q)) {
      hit <- grepl(tolower(q), tolower(w$variable), fixed = TRUE) |
        grepl(tolower(q), tolower(w$value), fixed = TRUE)
      idx <- which(hit)
    }
    view_idx(idx)
    shiny::req(length(idx) > 0)
    d <- w[idx, iphra_schema_display, drop = FALSE]
    d[is.na(d)] <- ""
    rhandsontable::rhandsontable(
      d, rowHeaders = NULL, height = 520, stretchH = "all", width = NULL,
      colWidths = c(190, 150, 260, 200, rep(170, length(iphra_schema_editable) - 2L))
    ) |>
      rhandsontable::hot_table(highlightRow = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_col(iphra_schema_canonical, readOnly = TRUE) |>
      rhandsontable::hot_cols(manualColumnResize = TRUE)
  })

  shiny::observeEvent(input$rm_table, {
    d <- rhandsontable::hot_to_r(input$rm_table)
    w <- ws()
    idx <- view_idx()
    if (is.null(w) || !is.data.frame(d) || nrow(d) != length(idx)) return()
    for (cl in iphra_schema_editable) {
      x <- as.character(d[[cl]])
      x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
      w[[cl]][idx] <- x
    }
    ws(w)
  })

  shiny::observeEvent(input$rm_validate, {
    shiny::req(ws())
    issues <- iphra_schema_issues(ws(), canonical())
    if (length(issues)) set_status("error", issues)
    else set_status("success", tx("The variable schema is valid."))
  })

  shiny::observeEvent(input$rm_save, {
    shiny::req(ws(), input$rm_dataset)
    issues <- iphra_schema_issues(ws(), canonical())
    if (length(issues)) {
      set_status("error", c(tx("The schema was not saved:"), issues))
      return()
    }
    obj <- datasets()[[input$rm_dataset]]
    res <- tryCatch({
      obj$set(field = "..variable_schema", value = ws())
      NULL
    }, error = function(e) e)
    if (inherits(res, "error")) {
      set_status("error", conditionMessage(res))
    } else {
      iphra_session$touch_module("datasets")
      set_status("success", tx("The variable schema was saved to the dataset."))
    }
  })

  shiny::observeEvent(input$rm_file, {
    shiny::req(ws())
    f <- input$rm_file
    res <- tryCatch({
      inc <- iphra_schema_read_file(f$datapath, f$name)
      iphra_schema_apply(ws(), inc)
    }, error = function(e) e)
    if (inherits(res, "error")) {
      set_status("error", paste(tx("Could not load the schema:"), conditionMessage(res)))
      return()
    }
    ws(res$data)
    version(version() + 1L)
    msgs <- sprintf(tx("Loaded %d row(s) from %s."), res$updated, f$name)
    if (res$unmatched > 0L) {
      msgs <- c(msgs, sprintf(tx("%d row(s) in the file did not match a variable/value row and were ignored."),
                              res$unmatched))
    }
    set_status(if (res$unmatched > 0L) "warning" else "success", msgs)
  })

  output$rm_export <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("[^A-Za-z0-9_-]+", "_", input$rm_dataset %||% "dataset"),
             "_variable_schema.", input$rm_format %||% "xlsx")
    },
    content = function(file) {
      w <- ws()
      shiny::req(w)
      if (identical(input$rm_format, "csv")) {
        utils::write.csv(w, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      } else {
        writexl::write_xlsx(w, file)
      }
    }
  )

  # ---- data reference -------------------------------------------------------
  rm_stages <- shiny::reactive({
    ds <- datasets()
    nm <- input$rm_dataset
    if (is.null(nm) || !nm %in% names(ds)) return(character(0))
    iphra_available_stages(ds[[nm]])
  })

  output$rm_data_ui <- shiny::renderUI({
    st <- rm_stages()
    if (!length(st)) {
      return(phr_info_panel(tx("This dataset has no data available."), title = tx("No data")))
    }
    sel <- shiny::isolate(input$rm_data_stage)
    if (is.null(sel) || !sel %in% st) sel <- st[1]
    shiny::tagList(
      shiny::radioButtons(ns("rm_data_stage"), label = NULL, inline = TRUE,
                          choices = stats::setNames(st, vapply(st, tx, character(1))),
                          selected = sel),
      shiny::textInput(ns("rm_data_search"), label = NULL, width = "100%",
                       placeholder = tx("Filter by column or value..."))
    )
  })

  output$rm_data_table <- rhandsontable::renderRHandsontable({
    ds <- datasets()
    nm <- input$rm_dataset
    st <- input$rm_data_stage
    shiny::req(nm, st, nm %in% names(ds), st %in% rm_stages())
    df <- tryCatch(ds[[nm]]$get_data(st), error = function(e) NULL)
    shiny::req(is.data.frame(df))
    ref <- iphra_data_reference(df, input$rm_data_search)
    shiny::req(!is.null(ref))
    rhandsontable::rhandsontable(
      ref, readOnly = TRUE, rowHeaders = NULL, height = 480, stretchH = "all", width = NULL,
      colWidths = c(140, 70, 400)
    ) |>
      rhandsontable::hot_cols(manualColumnResize = TRUE, columnSorting = TRUE)
  })

  # ---- tool reference -------------------------------------------------------
  tool <- shiny::reactive({
    t <- tool_r()
    if (is.null(t) || !is.data.frame(t$survey) || !is.data.frame(t$choices)) NULL else t
  })

  output$rm_ref_ui <- shiny::renderUI({
    if (is.null(tool())) {
      return(phr_info_panel(
        tx("Import a survey and choices sheet in the Data Import step to search the tool here."),
        title = tx("No tool loaded")
      ))
    }
    shiny::tagList(
      shiny::radioButtons(ns("rm_ref_type"), label = NULL, inline = TRUE,
                          choices = stats::setNames(c("survey", "choices"),
                                                    c(tx("Survey (name)"), tx("Choices (list_name)")))),
      shiny::textInput(ns("rm_ref_search"), label = NULL, placeholder = tx("Search..."), width = "100%")
    )
  })

  output$rm_ref_table <- rhandsontable::renderRHandsontable({
    t <- tool()
    shiny::req(t, input$rm_ref_type)
    is_survey <- identical(input$rm_ref_type, "survey")
    df <- if (is_survey) t$survey else t$choices
    key <- if (is_survey) "name" else "list_name"
    shiny::req(key %in% names(df))
    q <- trimws(input$rm_ref_search %||% "")
    if (nzchar(q)) {
      df <- df[grepl(tolower(q), tolower(as.character(df[[key]])), fixed = TRUE), , drop = FALSE]
    }
    lab <- grep("^label", names(df), value = TRUE)
    cols <- if (is_survey) c("name", "type", lab) else c("list_name", "name", lab)
    df <- utils::head(df[, intersect(cols, names(df)), drop = FALSE], 500L)
    shiny::req(nrow(df) > 0)
    df[] <- lapply(df, as.character)
    rhandsontable::rhandsontable(
      df, readOnly = TRUE, rowHeaders = NULL, height = 480, stretchH = "all", width = NULL
    ) |>
      rhandsontable::hot_cols(manualColumnResize = TRUE)
  })
}
