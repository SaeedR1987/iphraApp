#' Data cleaning module
#'
#' Select a stored Data object, generate its cleaning log (one or all), view the
#' cleaning and deletion logs on the left and the raw / standardized / clean
#' data on the right.
#'
#' @param id Module id.
#' @param lang_r Reactive returning the active UI language.
#' @noRd
mod_data_cleaning_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("toolbar")),
    shiny::uiOutput(ns("empty")),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::tabsetPanel(
          id = ns("log_tabs"),
          shiny::tabPanel(shiny::textOutput(ns("lbl_cleaning"), inline = TRUE), value = "cleaning",
                          shiny::div(style = "margin-top:10px;",
                                     shiny::uiOutput(ns("cleaning_entry_btns"), inline = TRUE),
                                     rhandsontable::rHandsontableOutput(ns("cleaning_log"), height = "auto"))),
          shiny::tabPanel(shiny::textOutput(ns("lbl_deletion"), inline = TRUE), value = "deletion",
                          shiny::div(style = "margin-top:10px;",
                                     shiny::uiOutput(ns("deletion_entry_btns"), inline = TRUE),
                                     rhandsontable::rHandsontableOutput(ns("deletion_log"), height = "auto")))
        )
      ),
      shiny::column(
        6,
        shiny::h4(shiny::textOutput(ns("lbl_viewer"), inline = TRUE), style = "margin:6px 0 4px;"),
        shiny::div(style = "margin-top:2px;",
                   shiny::uiOutput(ns("stage_select"))),
        shiny::uiOutput(ns("view_filters")),
        rhandsontable::rHandsontableOutput(ns("data_view"), height = "auto")
      )
    )
  )
}

mod_data_cleaning_server <- function(id, lang_r = function() "en") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iphra_session <- get_session(session)
    tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))
    max_rows <- 5000L

    output$lbl_cleaning <- shiny::renderText(tx("Cleaning Log"))
    output$lbl_viewer <- shiny::renderText(tx("Dataset Viewer"))

    stage_radio <- function(id, choices, selected) {
      shiny::div(
        style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
        shiny::span(class = "text-muted small", style = "font-weight:600;", tx("Dataset Selection")),
        shiny::div(class = "phr-stage-radio",
                   shiny::radioButtons(ns(id), NULL, inline = TRUE, choices = choices, selected = selected))
      )
    }
    output$lbl_deletion <- shiny::renderText(tx("Deletion Log"))
    shiny::outputOptions(output, "lbl_cleaning", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "lbl_deletion", suspendWhenHidden = FALSE)

    datasets_r <- iphra_session$get_module_reactive("datasets")
    datasets <- shiny::reactive(datasets_r() %||% list())
    refresh <- shiny::reactiveVal(0L)

    current <- shiny::reactive({
      ds <- datasets()
      nm <- input$dataset
      if (is.null(nm) || !nm %in% names(ds)) return(NULL)
      ds[[nm]]
    })

    output$toolbar <- shiny::renderUI({
      ds <- datasets()
      if (!length(ds)) return(NULL)
      choices <- stats::setNames(names(ds), vapply(names(ds), function(n) {
        paste0(n, " (", class(ds[[n]])[1], ")")
      }, character(1)))
      btn <- function(x, color) shiny::tagAppendAttributes(x, class = paste("phr-act", paste0("phr-act--", color)))
      phr_toolbar(
        shiny::tags$style(shiny::HTML(
          ".phr-cl-left .form-group{margin-bottom:4px;} .phr-cl-left .radio-inline{font-size:13px;}
           .phr-stage-radio .form-group{margin-bottom:0;}
           .phr-hide-col .selectize-input, .phr-hide-col .selectize-input input,
           .phr-hide-col .selectize-dropdown, .phr-row-no input{font-size:12px;}
           .phr-act{width:100%; height:100%; min-height:34px; font-size:11px; padding:4px 5px;
             line-height:1.2; white-space:normal; color:#333; border:1px solid rgba(0,0,0,.15);}
           .phr-act--blue{background:#d6e8fa;} .phr-act--teal{background:#d3f0ec;}
           .phr-act--purple{background:#e6dcf5;} .phr-act--yellow{background:#fbf0c4;}
           .phr-act--orange{background:#fde0c8;} .phr-act--green{background:#d4efd6;}
           .phr-act--red{background:#f8d0d0;} .phr-act--grey{background:#e4e7ea;}
           .phr-act:hover,.phr-act:focus{filter:brightness(.95); color:#222;}"
        )),
        shiny::div(
          style = "display:flex; gap:16px; align-items:flex-start; flex-wrap:wrap;",
          shiny::div(
            class = "phr-cl-left", style = "min-width:300px;",
            shiny::selectInput(ns("dataset"), label = NULL, choices = choices,
                               selected = shiny::isolate(input$dataset), width = "100%"),
            shiny::uiOutput(ns("gen_stage_ui"))
          ),
          shiny::div(
            style = "display:grid; grid-template-columns:repeat(7, minmax(100px, 1fr)); gap:6px; flex:1;",
            btn(shiny::actionButton(ns("gen_one"), tx("Generate Log"),
                                    icon = shiny::icon("list-alt"), class = "iphra-heavy"), "blue"),
            btn(shiny::actionButton(ns("save_one"), tx("Save Log"),
                                    icon = shiny::icon("save")), "teal"),
            btn(shiny::actionButton(ns("import_one"), tx("Import Log"),
                                    icon = shiny::icon("file-import")), "purple"),
            btn(shiny::actionButton(ns("validate_one"), tx("Validate Log"),
                                    icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::downloadButton(ns("export_one"), tx("Export Log")), "orange"),
            shiny::uiOutput(ns("complete_one_ui"), style = "display:contents;"),
            btn(shiny::actionButton(ns("clean_one"), tx("Clean"),
                                    icon = shiny::icon("broom")), "grey"),
            btn(shiny::actionButton(ns("gen_all"), tx("Generate All Logs"),
                                    icon = shiny::icon("list"), class = "iphra-heavy"), "blue"),
            btn(shiny::actionButton(ns("save_all"), tx("Save All Logs"),
                                    icon = shiny::icon("floppy-disk")), "teal"),
            btn(shiny::actionButton(ns("import_all"), tx("Import All Logs"),
                                    icon = shiny::icon("file-import")), "purple"),
            btn(shiny::actionButton(ns("validate_all"), tx("Validate All Logs"),
                                    icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::downloadButton(ns("export_all"), tx("Export All Logs")), "orange"),
            shiny::uiOutput(ns("complete_all_ui"), style = "display:contents;"),
            btn(shiny::actionButton(ns("clean_all"), tx("Clean All"),
                                    icon = shiny::icon("broom")), "grey")
          )
        )
      )
    })

    complete_btn <- function(id, all) {
      refresh()
      ds <- datasets()
      objs <- if (all) ds else if (!is.null(current())) stats::setNames(list(current()), input$dataset) else list()
      st <- unlist(lapply(objs, function(o) c(iphra_log_status(o, "cleaning"), iphra_log_status(o, "deletion"))))
      done <- length(st) > 0 && all(st == "green")
      lab <- if (all) {
        if (done) tx("Mark All Logs Incomplete") else tx("Mark All Logs Complete")
      } else {
        if (done) tx("Mark Log Incomplete") else tx("Mark Log Complete")
      }
      shiny::actionButton(ns(id), lab, icon = shiny::icon(if (done) "rotate-left" else "flag-checkered"),
                          class = paste("phr-act", if (done) "phr-act--red" else "phr-act--green"))
    }
    output$complete_one_ui <- shiny::renderUI(complete_btn("complete_one", FALSE))
    output$complete_all_ui <- shiny::renderUI(complete_btn("complete_all", TRUE))

    output$empty <- shiny::renderUI({
      if (length(datasets())) return(NULL)
      phr_info_panel(
        tx("Import a dataset in the Data Import step first."),
        title = tx("No datasets available")
      )
    })

    output$gen_stage_ui <- shiny::renderUI({
      obj <- current(); shiny::req(obj)
      avail <- intersect(c("raw", "standardized"), iphra_available_stages(obj))
      if (!length(avail)) avail <- c("raw", "standardized")
      labels <- c(raw = tx("Raw"), standardized = tx("Standardized"))
      sel <- shiny::isolate(input$gen_stage)
      if (is.null(sel) || !sel %in% avail) {
        sel <- if ("standardized" %in% avail) "standardized" else avail[1]
      }
      stage_radio("gen_stage", stats::setNames(avail, labels[avail]), sel)
    })

    gen_stage <- function() {
      st <- input$gen_stage
      if (is.null(st) || !st %in% c("raw", "standardized")) "standardized" else st
    }

    generate <- function(obj, label) {
      stage <- gen_stage()
      clear_work(label)
      avail <- intersect(c("raw", "standardized"), iphra_available_stages(obj))
      if (length(avail) && !stage %in% avail) stage <- avail[1]
      res <- tryCatch(obj$generate_cleaning_log(stage = stage), error = function(e) e)
      failed <- inherits(res, "error") ||
        isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE))
      if (failed) {
        msg <- if (inherits(res, "error")) conditionMessage(res) else tx("the step reported a failure, see the log.")
        shiny::showNotification(paste0(label, ": ", msg), type = "error", duration = 15)
      }
      !failed
    }

    shiny::observeEvent(input$gen_one, {
      obj <- current(); shiny::req(obj)
      ok <- shiny::withProgress(
        message = tx("Generating cleaning log"), detail = input$dataset, value = 0.5,
        generate(obj, input$dataset)
      )
      if (ok) {
        n_of <- function(field) {
          lg <- tryCatch(obj$get(field = field), error = function(e) NULL)
          df <- if (is.null(lg)) NULL else tryCatch(lg$get("..log_df"), error = function(e) NULL)
          if (is.data.frame(df)) nrow(df) else 0L
        }
        n_clean <- n_of("..cleaning_log"); n_del <- n_of("..deletion_log")
        shiny::showNotification(
          paste0(tx("Cleaning log"), ": ", n_clean, " / ", tx("Deletion Log"), ": ", n_del),
          type = if (n_clean + n_del > 0) "message" else "warning"
        )
      }
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
    })

    shiny::observeEvent(input$gen_all, {
      ds <- datasets()
      if (!length(ds)) return()
      ok <- 0L
      shiny::withProgress(message = tx("Generating cleaning logs"), value = 0, {
        for (nm in names(ds)) {
          shiny::incProgress(1 / length(ds), detail = nm)
          if (generate(ds[[nm]], nm)) ok <- ok + 1L
        }
      })
      shiny::showNotification(
        paste0(tx("Cleaning logs generated"), ": ", ok, "/", length(ds)),
        type = if (ok == length(ds)) "message" else "warning"
      )
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
    })

    log_fields <- c(cleaning = "..cleaning_log", deletion = "..deletion_log")
    # Working (possibly edited) copies of the logs, keyed by dataset and log kind.
    work <- new.env(parent = emptyenv())
    rest_rows <- new.env(parent = emptyenv())
    work_ver <- shiny::reactiveVal(0L)
    work_key <- function(nm, kind) paste0(nm, "::", kind)
    clear_work <- function(nm) {
      for (k in names(log_fields)) {
        key <- work_key(nm, k)
        for (e in list(work, rest_rows)) {
          if (exists(key, envir = e, inherits = FALSE)) rm(list = key, envir = e)
        }
      }
    }

    log_df_of <- function(obj, field) {
      lg <- tryCatch(obj$get(field = field), error = function(e) NULL)
      if (is.null(lg)) return(NULL)
      df <- tryCatch(lg$get("..log_df"), error = function(e) NULL)
      if (is.data.frame(df)) df else NULL
    }

    log_table <- function(kind) {
      refresh(); work_ver()
      obj <- current(); shiny::req(obj)
      full <- ensure_work(obj, input$dataset, kind)
      shiny::req(is.data.frame(full), nrow(full) > 0)
      hide <- setdiff(input[[paste0("hide_", kind)]], c("row_no", "uuid"))
      df <- full[, setdiff(names(full), hide), drop = FALSE]
      rhandsontable::rhandsontable(
        df, height = 520, rowHeaders = NULL, stretchH = "none"
      ) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_col("row_no", readOnly = TRUE) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE,
                                fixedColumnsLeft = fixed_cols(df))
    }
    output$cleaning_log <- rhandsontable::renderRHandsontable(log_table("cleaning"))
    output$deletion_log <- rhandsontable::renderRHandsontable(log_table("deletion"))

    hide_select <- function(kind) {
      refresh()
      obj <- current(); shiny::req(obj)
      w <- ensure_work(obj, input$dataset, kind)
      shiny::req(is.data.frame(w))
      cols <- setdiff(names(w), c("row_no", "uuid"))
      id <- paste0("hide_", kind)
      shiny::selectizeInput(
        ns(id), label = NULL, choices = cols, multiple = TRUE, width = "100%",
        selected = intersect(shiny::isolate(input[[id]]), cols),
        options = list(placeholder = tx("Hide Columns"), plugins = list("remove_button"))
      )
    }
    output$hide_cleaning_ui <- shiny::renderUI(hide_select("cleaning"))
    output$hide_deletion_ui <- shiny::renderUI(hide_select("deletion"))

    row_no_input <- function(kind) {
      x <- shiny::numericInput(ns(paste0("row_n_", kind)), label = NULL, value = NA,
                               min = 0, step = 1, width = "100%")
      htmltools::tagQuery(x)$find("input")$addAttrs(placeholder = tx("Row No."))$allTags()
    }

    entry_btns <- function(kind) {
      shiny::div(
        style = "margin-bottom:6px; display:flex; gap:6px; align-items:center; flex-wrap:wrap;",
        shiny::uiOutput(ns(paste0("valid_", kind, "_ui")), inline = TRUE),
        shiny::div(class = "phr-row-no", style = "width:90px;",
                   row_no_input(kind)),
        shiny::div(
          style = "display:flex; flex-direction:column; gap:3px;",
          shiny::actionButton(ns(paste0("add_", kind)), tx("Add Entry After Row"),
                              icon = shiny::icon("plus"), class = "btn-default btn-xs"),
          shiny::actionButton(ns(paste0("rm_", kind)), tx("Remove Row"),
                              icon = shiny::icon("minus"), class = "btn-default btn-xs")
        ),
        shiny::div(class = "phr-hide-col", style = "min-width:200px; flex:1;",
                   shiny::uiOutput(ns(paste0("hide_", kind, "_ui"))))
      )
    }
    output$cleaning_entry_btns <- shiny::renderUI(entry_btns("cleaning"))
    output$deletion_entry_btns <- shiny::renderUI(entry_btns("deletion"))

    fixed_cols <- function(df) sum(tolower(names(df)) %in% c("row_no", "uuid"))

    # Working-copy-only row number, always the first column.
    renumber <- function(w) {
      w$row_no <- seq_len(nrow(w))
      w[, c("row_no", setdiff(names(w), "row_no")), drop = FALSE]
    }

    # Head (displayed rows, row number then uuid first) of the working log; created
    # from the stored log on first use. Rows beyond max_rows are held aside in `rest`.
    set_work <- function(key, orig) {
      orig <- as.data.frame(orig)
      cols <- names(orig)
      cols <- c(cols[tolower(cols) == "uuid"], cols[tolower(cols) != "uuid"])
      orig <- orig[, cols, drop = FALSE]
      assign(key, renumber(iphra_display_frame(utils::head(orig, max_rows))), envir = work)
      rest <- if (nrow(orig) > max_rows) orig[-seq_len(max_rows), , drop = FALSE] else NULL
      assign(key, rest, envir = rest_rows)
    }

    ensure_work <- function(obj, nm, kind) {
      key <- work_key(nm, kind)
      if (!exists(key, envir = work, inherits = FALSE)) {
        orig <- log_df_of(obj, log_fields[[kind]])
        if (is.null(orig)) return(NULL)
        set_work(key, orig)
      }
      get(key, envir = work, inherits = FALSE)
    }

    # ---- import logs from a file into the working copies ---------------------
    imp <- shiny::reactiveVal(NULL)

    open_import <- function(mode) {
      ds <- datasets()
      if (!length(ds)) return()
      target <- if (mode == "one") input$dataset else NULL
      if (mode == "one" && is.null(target)) return()
      imp(list(mode = mode, target = target, sheets = NULL))
      shiny::showModal(shiny::modalDialog(
        title = if (mode == "one") paste(tx("Import Log"), "-", target) else tx("Import All Logs"),
        size = "l", easyClose = FALSE,
        shiny::p(class = "text-muted",
                 tx("Choose an Excel or CSV file, then match each sheet to a cleaning or deletion log of a stored Data object. Imported logs replace the working copy; use Save Log to store them in the Data object.")),
        shiny::fileInput(ns("log_file"), label = NULL, accept = c(".xlsx", ".xls", ".xlsm", ".csv", ".txt")),
        shiny::uiOutput(ns("log_map_ui")),
        footer = shiny::tagList(
          shiny::modalButton(tx("Cancel")),
          shiny::actionButton(ns("confirm_log_import"), tx("Import"), class = "btn-primary")
        )
      ))
    }
    shiny::observeEvent(input$import_one, open_import("one"))
    shiny::observeEvent(input$import_all, open_import("all"))

    shiny::observeEvent(input$log_file, {
      f <- input$log_file
      p <- imp(); shiny::req(p)
      parsed <- tryCatch(iphra_read_import_file(f$datapath, f$name), error = function(e) e)
      if (inherits(parsed, "error") || !length(parsed)) {
        shiny::showNotification(
          paste(tx("Could not read the file:"), f$name,
                if (inherits(parsed, "error")) conditionMessage(parsed) else tx("No data sheets found.")),
          type = "error", duration = 10)
        return()
      }
      p$sheets <- parsed
      imp(p)
    })

    guess_log_kind <- function(sheet) {
      s <- tolower(sheet)
      if (grepl("delet", s)) "deletion" else if (grepl("clean", s)) "cleaning" else "skip"
    }

    guess_log_dataset <- function(sheet, nms) {
      base <- trimws(sub("\\s*(cleaning|deletion)\\d*$", "", sheet, ignore.case = TRUE))
      if (!nzchar(base)) return("__skip__")
      safe <- tolower(gsub("[\\[\\]:*?/\\\\]", "_", nms, perl = TRUE))
      hit <- which(safe == tolower(base))
      if (!length(hit)) hit <- which(startsWith(safe, tolower(base)))
      if (length(hit)) nms[hit[1]] else "__skip__"
    }

    output$log_map_ui <- shiny::renderUI({
      p <- imp()
      if (is.null(p) || is.null(p$sheets)) return(NULL)
      nms <- names(datasets())
      kinds <- stats::setNames(c("skip", "cleaning", "deletion"),
                               c(tx("Do not import"), tx("Cleaning log"), tx("Deletion log")))
      ds_choices <- c(stats::setNames("__skip__", tx("(do not import)")), nms)
      guessed <- vapply(names(p$sheets), guess_log_kind, character(1))
      for (k in c("cleaning", "deletion")) {
        dup <- which(guessed == k)
        if (p$mode == "one" && length(dup) > 1L) guessed[dup[-1L]] <- "skip"
      }
      rows <- lapply(seq_along(p$sheets), function(i) {
        df <- p$sheets[[i]]
        shiny::tags$tr(
          shiny::tags$td(
            shiny::strong(names(p$sheets)[i]), shiny::tags$br(),
            shiny::span(class = "text-muted",
                        paste0(format(nrow(df), big.mark = ","), " ", tx("rows"), " \u00d7 ",
                               ncol(df), " ", tx("columns")))
          ),
          shiny::tags$td(shiny::selectInput(ns(paste0("logtype_", i)), NULL, kinds,
                                            selected = guessed[[i]], width = "100%")),
          if (p$mode == "all") {
            shiny::tags$td(shiny::selectInput(
              ns(paste0("logds_", i)), NULL, ds_choices, width = "100%",
              selected = guess_log_dataset(names(p$sheets)[i], nms)))
          }
        )
      })
      shiny::tags$table(
        class = "table table-condensed phr-import-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(tx("Sheet")), shiny::tags$th(tx("Import as")),
          if (p$mode == "all") shiny::tags$th(tx("Data object"))
        )),
        shiny::tags$tbody(rows)
      )
    })

    # Returns NULL on success or a message describing why the sheet was skipped.
    apply_log_import <- function(nm, kind, df) {
      obj <- datasets()[[nm]]
      if (is.null(obj)) return(tx("Data object not found."))
      orig <- log_df_of(obj, log_fields[[kind]])
      if (is.null(orig)) return(tx("No log exists yet; generate one first."))
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      df$row_no <- NULL
      if (!"uuid" %in% tolower(names(df))) return(tx("No uuid column found."))
      for (m in setdiff(names(orig), names(df))) df[[m]] <- NA
      df <- df[, names(orig), drop = FALSE]
      for (cl in names(orig)) {
        cls <- class(orig[[cl]])[1]
        if (!identical(class(df[[cl]])[1], cls)) {
          df[[cl]] <- tryCatch(methods::as(df[[cl]], cls), error = function(e) df[[cl]])
        }
      }
      set_work(work_key(nm, kind), df)
      NULL
    }

    shiny::observeEvent(input$confirm_log_import, {
      p <- imp()
      if (is.null(p) || is.null(p$sheets)) {
        shiny::showNotification(tx("Select a file first."), type = "warning")
        return()
      }
      done <- 0L
      for (i in seq_along(p$sheets)) {
        kind <- input[[paste0("logtype_", i)]]
        if (is.null(kind) || kind == "skip") next
        nm <- if (p$mode == "one") p$target else input[[paste0("logds_", i)]]
        if (is.null(nm) || nm == "__skip__") next
        why <- apply_log_import(nm, kind, p$sheets[[i]])
        if (is.null(why)) {
          done <- done + 1L
        } else {
          shiny::showNotification(paste0(names(p$sheets)[i], " -> ", nm, ": ", why),
                                  type = "warning", duration = 10)
        }
      }
      shiny::removeModal()
      work_ver(work_ver() + 1L)
      shiny::showNotification(
        paste0(tx("Logs imported"), ": ", done),
        type = if (done > 0) "message" else "warning")
    })

    shiny::observeEvent(input$add_cleaning, add_entry("cleaning"))
    shiny::observeEvent(input$add_deletion, add_entry("deletion"))
    shiny::observeEvent(input$rm_cleaning, remove_entry("cleaning"))
    shiny::observeEvent(input$rm_deletion, remove_entry("deletion"))

    add_entry <- function(kind) {
      obj <- current(); shiny::req(obj)
      w <- ensure_work(obj, input$dataset, kind)
      if (is.null(w)) return()
      n <- input[[paste0("row_n_", kind)]]
      pos <- if (is.null(n) || is.na(n)) nrow(w) else if (n == 0) 0L else match(n, w$row_no)
      if (is.na(pos)) {
        shiny::showNotification(tx("Row number not found."), type = "warning")
        return()
      }
      blank <- as.data.frame(lapply(w, function(x) x[NA_integer_]), stringsAsFactors = FALSE)
      after <- if (pos < nrow(w)) w[(pos + 1L):nrow(w), , drop = FALSE] else w[0, , drop = FALSE]
      w <- rbind(w[seq_len(pos), , drop = FALSE], blank, after)
      assign(work_key(input$dataset, kind), renumber(w), envir = work)
      work_ver(work_ver() + 1L)
    }

    remove_entry <- function(kind) {
      obj <- current(); shiny::req(obj)
      w <- ensure_work(obj, input$dataset, kind)
      if (is.null(w) || nrow(w) == 0) return()
      n <- input[[paste0("row_n_", kind)]]
      pos <- if (is.null(n) || is.na(n)) nrow(w) else match(n, w$row_no)
      if (is.na(pos)) {
        shiny::showNotification(tx("Row number not found."), type = "warning")
        return()
      }
      assign(work_key(input$dataset, kind), renumber(w[-pos, , drop = FALSE]), envir = work)
      work_ver(work_ver() + 1L)
    }

    # The table may show only some columns and in a sorted order, so merge the
    # visible columns back into the full working frame by row number.
    store_work <- function(kind, hot) {
      nm <- input$dataset
      if (is.null(nm) || is.null(hot)) return()
      df <- tryCatch(rhandsontable::hot_to_r(hot), error = function(e) NULL)
      if (!is.data.frame(df)) return()
      key <- work_key(nm, kind)
      full <- get0(key, envir = work, inherits = FALSE)
      if (is.data.frame(full) && !identical(names(full), names(df))) {
        idx <- if (nrow(full) == nrow(df) && !anyNA(df$row_no) && !anyDuplicated(df$row_no)) {
          match(df$row_no, full$row_no)
        } else NA
        if (anyNA(idx)) return()
        full <- full[idx, , drop = FALSE]
        full[names(df)] <- df
        df <- full
      }
      assign(key, df, envir = work)
    }
    shiny::observeEvent(input$cleaning_log, store_work("cleaning", input$cleaning_log))
    shiny::observeEvent(input$deletion_log, store_work("deletion", input$deletion_log))

    # Working log merged with the rows held aside, with the original column
    # order and classes restored where possible.
    working_log <- function(obj, nm, kind) {
      key <- work_key(nm, kind)
      if (!exists(key, envir = work, inherits = FALSE)) return(NULL)
      w <- get(key, envir = work, inherits = FALSE)
      orig <- log_df_of(obj, log_fields[[kind]])
      if (is.null(orig) || !all(names(orig) %in% names(w))) return(NULL)
      w <- w[, names(orig), drop = FALSE]
      for (cl in names(orig)) {
        cls <- class(orig[[cl]])[1]
        if (!identical(class(w[[cl]])[1], cls)) {
          w[[cl]] <- tryCatch(methods::as(w[[cl]], cls), error = function(e) w[[cl]])
        }
      }
      rest <- get0(key, envir = rest_rows, inherits = FALSE)
      if (!is.null(rest)) w <- rbind(w, rest[, names(orig), drop = FALSE])
      tibble::as_tibble(w)
    }

    save_logs <- function(obj, nm) {
      saved <- 0L
      for (kind in names(log_fields)) {
        df <- working_log(obj, nm, kind)
        if (is.null(df)) next
        field <- log_fields[[kind]]
        res <- tryCatch(
          obj$set(field = field, member = "..log_df", value = df),
          error = function(e) {
            tryCatch(obj$get(field = field)$set("..log_df", df), error = function(e2) e2)
          }
        )
        if (inherits(res, "error")) {
          shiny::showNotification(paste0(nm, " (", kind, "): ", conditionMessage(res)),
                                  type = "error", duration = 15)
        } else {
          saved <- saved + 1L
        }
      }
      saved
    }

    shiny::observeEvent(input$save_one, {
      obj <- current(); shiny::req(obj)
      n <- save_logs(obj, input$dataset)
      shiny::showNotification(
        paste0(tx("Logs saved"), ": ", n),
        type = if (n > 0) "message" else "warning"
      )
      iphra_session$touch_module("datasets")
    })

    shiny::observeEvent(input$save_all, {
      ds <- datasets()
      if (!length(ds)) return()
      n <- 0L
      for (nm in names(ds)) n <- n + save_logs(ds[[nm]], nm)
      shiny::showNotification(
        paste0(tx("Logs saved"), ": ", n),
        type = if (n > 0) "message" else "warning"
      )
      iphra_session$touch_module("datasets")
    })

    # ---- validation ----------------------------------------------------------
    log_validated <- function(obj, kind) {
      lg <- tryCatch(obj$get(field = log_fields[[kind]]), error = function(e) NULL)
      if (is.null(lg)) return(NA)
      for (f in c("..validated")) {
        v <- tryCatch(lg$get(f), error = function(e) NULL)
        if (is.logical(v) && length(v) == 1L) return(v)
      }
      NA
    }

    valid_icon <- function(kind) {
      refresh()
      obj <- current(); shiny::req(obj)
      v <- log_validated(obj, kind)
      spec <- if (isTRUE(v)) list("circle-check", "#2e7d32", tx("Log validated"))
      else if (isFALSE(v)) list("circle-xmark", "#c62828", tx("Log not validated"))
      else list("circle-question", "#9e9e9e", tx("Validation status unknown"))
      shiny::span(title = spec[[3]], style = paste0("color:", spec[[2]], "; font-size:16px;"),
                  shiny::icon(spec[[1]]))
    }
    output$valid_cleaning_ui <- shiny::renderUI(valid_icon("cleaning"))
    output$valid_deletion_ui <- shiny::renderUI(valid_icon("deletion"))

    # Saves the working copy first so the stored log is what gets validated.
    validate_obj <- function(obj, nm) {
      save_logs(obj, nm)
      stage <- gen_stage()
      ok <- 0L
      for (kind in names(log_fields)) {
        lg <- tryCatch(obj$get(field = log_fields[[kind]]), error = function(e) NULL)
        if (is.null(lg)) next
        res <- tryCatch(
          {
            lg$validate()
            if (kind == "cleaning") lg$validate_cl_against_dataset(obj, stage = stage)
            else lg$validate_dl_against_dataset(obj, stage = stage)
          },
          error = function(e) e
        )
        if (inherits(res, "error")) {
          shiny::showNotification(paste0(nm, " (", kind, "): ", conditionMessage(res)),
                                  type = "error", duration = 15)
        } else ok <- ok + 1L
      }
      ok
    }

    report_validation <- function(objs) {
      v <- unlist(lapply(names(objs), function(nm) {
        c(log_validated(objs[[nm]], "cleaning"), log_validated(objs[[nm]], "deletion"))
      }))
      bad <- sum(!v, na.rm = TRUE)
      shiny::showNotification(
        paste0(tx("Logs validated"), ": ", sum(v, na.rm = TRUE), "/", length(v),
               if (bad > 0) paste0(" (", bad, " ", tx("with issues"), ")") else ""),
        type = if (bad > 0) "warning" else "message")
    }

    shiny::observeEvent(input$validate_one, {
      obj <- current(); shiny::req(obj)
      validate_obj(obj, input$dataset)
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
      report_validation(stats::setNames(list(obj), input$dataset))
    })

    shiny::observeEvent(input$validate_all, {
      ds <- datasets()
      if (!length(ds)) return()
      for (nm in names(ds)) validate_obj(ds[[nm]], nm)
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
      report_validation(ds)
    })

    # Completion lives in each log's metadata so it is saved with the Data object.
    complete_obj <- function(obj, nm, value = TRUE) {
      if (value) save_logs(obj, nm)
      n <- 0L
      for (kind in names(log_fields)) {
        lg <- tryCatch(obj$get(field = log_fields[[kind]]), error = function(e) NULL)
        if (is.null(lg)) next
        md <- iphra_log_get(lg, "metadata")
        if (!is.list(md)) md <- list()
        md$complete <- value
        md$completed_datetime <- if (value) Sys.time() else NULL
        err <- iphra_log_set(lg, "metadata", md)
        if (is.null(err)) n <- n + 1L else {
          shiny::showNotification(paste0(nm, " (", kind, "): ", conditionMessage(err)),
                                  type = "error", duration = 15)
        }
      }
      n
    }

    all_complete <- function(objs) {
      st <- unlist(lapply(objs, function(o) c(iphra_log_status(o, "cleaning"), iphra_log_status(o, "deletion"))))
      length(st) > 0 && all(st == "green")
    }

    toggle_msg <- function(n, value) {
      shiny::showNotification(
        paste0(if (value) tx("Logs marked complete") else tx("Logs marked incomplete"), ": ", n),
        type = if (n > 0) "message" else "warning")
    }

    shiny::observeEvent(input$complete_one, {
      obj <- current(); shiny::req(obj)
      value <- !all_complete(list(obj))
      n <- complete_obj(obj, input$dataset, value)
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
      toggle_msg(n, value)
    })

    shiny::observeEvent(input$complete_all, {
      ds <- datasets()
      if (!length(ds)) return()
      value <- !all_complete(ds)
      n <- 0L
      for (nm in names(ds)) n <- n + complete_obj(ds[[nm]], nm, value)
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
      toggle_msg(n, value)
    })

    # Working (edited) log if present, otherwise the stored one.
    export_log_df <- function(obj, nm, kind) {
      df <- working_log(obj, nm, kind) %||% log_df_of(obj, log_fields[[kind]])
      if (is.null(df)) data.frame() else iphra_display_frame(as.data.frame(df))
    }

    # Excel sheet names: max 31 chars, no []:*?/\ and unique.
    sheet_name <- function(nm, suffix, used) {
      base <- gsub("[\\[\\]:*?/\\\\]", "_", nm, perl = TRUE)
      out <- substr(base, 1, 31 - nchar(suffix) - 1)
      out <- paste(out, suffix)
      i <- 1L
      while (tolower(out) %in% tolower(used)) {
        i <- i + 1L
        tag <- paste0(suffix, i)
        out <- paste(substr(base, 1, 31 - nchar(tag) - 1), tag)
      }
      out
    }

    log_sheets <- function(ds_list) {
      sheets <- list()
      for (nm in names(ds_list)) {
        for (kind in names(log_fields)) {
          suffix <- if (kind == "cleaning") "Cleaning" else "Deletion"
          sheets[[sheet_name(nm, suffix, names(sheets))]] <-
            export_log_df(ds_list[[nm]], nm, kind)
        }
      }
      sheets
    }

    output$export_one <- shiny::downloadHandler(
      filename = function() {
        paste0("cleaning_logs_", gsub("[^A-Za-z0-9_-]", "_", input$dataset %||% "dataset"),
               "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        obj <- current(); shiny::req(obj)
        nm <- input$dataset
        sheets <- list(
          Cleaning = export_log_df(obj, nm, "cleaning"),
          Deletion = export_log_df(obj, nm, "deletion")
        )
        writexl::write_xlsx(sheets, path = file)
      }
    )

    output$export_all <- shiny::downloadHandler(
      filename = function() paste0("cleaning_logs_all_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        ds <- datasets(); shiny::req(length(ds) > 0)
        writexl::write_xlsx(log_sheets(ds), path = file)
      }
    )

    clean_obj <- function(obj, label) {
      clear_work(label)
      res <- tryCatch(obj$clean(), error = function(e) e)
      failed <- inherits(res, "error") ||
        isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE))
      if (failed) {
        msg <- if (inherits(res, "error")) conditionMessage(res) else tx("the step reported a failure, see the log.")
        shiny::showNotification(paste0(label, ": ", msg), type = "error", duration = 15)
      }
      !failed
    }

    shiny::observeEvent(input$clean_one, {
      obj <- current(); shiny::req(obj)
      if (clean_obj(obj, input$dataset)) {
        shiny::showNotification(paste0(tx("Clean"), ": ", tx("done")), type = "message")
      }
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
    })

    shiny::observeEvent(input$clean_all, {
      ds <- datasets()
      if (!length(ds)) return()
      ok <- 0L
      shiny::withProgress(message = tx("Cleaning datasets"), value = 0, {
        for (nm in names(ds)) {
          shiny::incProgress(1 / length(ds), detail = nm)
          if (clean_obj(ds[[nm]], nm)) ok <- ok + 1L
        }
      })
      shiny::showNotification(
        paste0(tx("Datasets cleaned"), ": ", ok, "/", length(ds)),
        type = if (ok == length(ds)) "message" else "warning"
      )
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
    })

    stage_data <- function(obj, stage) {
      df <- tryCatch(obj$get_data(stage), error = function(e) NULL)
      if (is.data.frame(df)) df else NULL
    }

    output$stage_select <- shiny::renderUI({
      refresh()
      obj <- current(); shiny::req(obj)
      stages <- c("raw", "standardized", "clean")
      avail <- Filter(function(s) !is.null(stage_data(obj, s)), stages)
      if (!length(avail)) avail <- "raw"
      labels <- c(raw = tx("Raw"), standardized = tx("Standardized"), clean = tx("Clean"))
      sel <- shiny::isolate(input$stage)
      if (is.null(sel) || !sel %in% avail) sel <- avail[1]
      do.call(shiny::tabsetPanel, c(
        list(id = ns("stage"), selected = sel),
        lapply(avail, function(s) shiny::tabPanel(labels[[s]], value = s))
      ))
    })

    # Stage table with the object's UUID column moved to the far left.
    stage_frame <- shiny::reactive({
      refresh()
      obj <- current(); shiny::req(obj, input$stage)
      df <- stage_data(obj, input$stage)
      shiny::req(df)
      df <- as.data.frame(df)
      uuid <- tryCatch(obj$get(field = "..uuid"), error = function(e) NULL)
      if (is.character(uuid) && length(uuid) == 1L && uuid %in% names(df)) {
        df <- df[, c(uuid, setdiff(names(df), uuid)), drop = FALSE]
      } else uuid <- NULL
      attr(df, "uuid") <- uuid
      df
    })

    output$view_filters <- shiny::renderUI({
      if (!length(datasets())) return(NULL)
      cols <- names(stage_frame())
      shiny::div(
        style = "display:flex; gap:8px; flex-wrap:wrap; margin:6px 0;",
        shiny::div(
          style = "min-width:140px; flex:1;",
          shiny::textInput(ns("col_search"), label = NULL, width = "100%",
                           value = shiny::isolate(input$col_search) %||% "",
                           placeholder = tx("Search columns..."))
        ),
        shiny::div(
          style = "min-width:140px; flex:1;",
          shiny::selectInput(
            ns("filter_col"), label = NULL, width = "100%",
            choices = c(stats::setNames("", tx("Filter rows by column...")), cols),
            selected = shiny::isolate(input$filter_col)
          )
        ),
        shiny::div(
          style = "min-width:140px; flex:1;",
          shiny::textInput(ns("filter_val"), label = NULL, width = "100%",
                           value = shiny::isolate(input$filter_val) %||% "",
                           placeholder = tx("Value contains..."))
        )
      )
    })

    # Columns narrowed by the column search (UUID always kept), rows by value filter.
    view_frame <- shiny::reactive({
      df <- stage_frame()
      uuid <- attr(df, "uuid")
      fc <- input$filter_col %||% ""
      fv <- trimws(input$filter_val %||% "")
      if (nzchar(fc) && nzchar(fv) && fc %in% names(df)) {
        hit <- grepl(tolower(fv), tolower(as.character(df[[fc]])), fixed = TRUE)
        df <- df[which(hit), , drop = FALSE]
      }
      q <- tolower(trimws(input$col_search %||% ""))
      if (nzchar(q)) {
        keep <- grepl(q, tolower(names(df)), fixed = TRUE) | names(df) %in% uuid
        df <- df[, keep, drop = FALSE]
      }
      attr(df, "uuid") <- uuid
      df
    })

    output$data_view <- rhandsontable::renderRHandsontable({
      df <- view_frame()
      shiny::req(ncol(df) > 0)
      fixed <- if (!is.null(attr(df, "uuid"))) 1L else 0L
      rhandsontable::rhandsontable(
        iphra_display_frame(utils::head(df, max_rows)),
        readOnly = TRUE, height = 520, rowHeaders = NULL,
        highlightRow = TRUE, highlightCol = TRUE, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE,
                                fixedColumnsLeft = fixed)
    })
  })
}
