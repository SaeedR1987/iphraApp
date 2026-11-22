#' Quantitative data analysis module
#'
#' Select a stored `DataAnalytics` object, edit and save its Data Analysis
#' Plan (DAP) on the left and browse its stored analysis results, tables and
#' visualizations on the right.
#'
#' @param id Module id.
#' @param lang_r Reactive returning the active UI language.
#' @noRd
mod_analysis_quant_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("toolbar")),
    shiny::uiOutput(ns("empty")),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h4(shiny::textOutput(ns("lbl_dap"), inline = TRUE), style = "margin:6px 0 4px;"),
        shiny::div(style = "margin:4px 0 6px;", shiny::uiOutput(ns("dap_status"))),
        rhandsontable::rHandsontableOutput(ns("dap_table"), height = "auto")
      ),
      shiny::column(
        6,
        shiny::tabsetPanel(
          id = ns("result_tabs"),
          shiny::tabPanel(
            shiny::textOutput(ns("lbl_results"), inline = TRUE), value = "results",
            shiny::div(style = "margin-top:10px;",
                       shiny::uiOutput(ns("results_nav")),
                       shiny::uiOutput(ns("results_view")))
          ),
          shiny::tabPanel(
            shiny::textOutput(ns("lbl_tables"), inline = TRUE), value = "tables",
            shiny::div(style = "margin-top:10px;",
                       shiny::uiOutput(ns("tables_nav")),
                       shiny::uiOutput(ns("tables_view")))
          ),
          shiny::tabPanel(
            shiny::textOutput(ns("lbl_viz"), inline = TRUE), value = "viz",
            shiny::div(style = "margin-top:10px;",
                       shiny::uiOutput(ns("viz_nav")),
                       shiny::uiOutput(ns("viz_view")))
          )
        )
      )
    )
  )
}

# A node of a nested result structure that can be navigated into: a plain,
# non-empty list (data frames and classed objects such as ggplot or flextable
# are leaves).
iphra_is_container <- function(x) {
  is.list(x) && !is.data.frame(x) && is.null(attr(x, "class")) && length(x) > 0L
}

# Chain of dropdowns, one per nesting level, that walks down a nested list.
# Returns the dropdown UI and the leaf the current selections point at.
iphra_nested_nav <- function(ns, prefix, x, input) {
  ui <- list()
  node <- x
  level <- 0L
  while (iphra_is_container(node)) {
    level <- level + 1L
    nms <- names(node)
    if (is.null(nms)) nms <- rep("", length(node))
    nms[!nzchar(nms)] <- as.character(seq_along(node))[!nzchar(nms)]
    id <- paste0(prefix, "_", level)
    cur <- input[[id]]
    if (is.null(cur) || !cur %in% nms) cur <- nms[1]
    labels <- gsub("_", " ", nms, fixed = TRUE)
    ui[[level]] <- shiny::div(
      style = "min-width:170px; flex:1;",
      shiny::selectInput(ns(id), NULL, choices = stats::setNames(nms, labels),
                         selected = cur, width = "100%")
    )
    node <- node[[match(cur, nms)]]
  }
  list(
    ui = shiny::div(style = "display:flex; gap:8px; flex-wrap:wrap;", ui),
    leaf = node
  )
}

iphra_print_leaf <- function(x, max_lines = 300L) {
  out <- tryCatch(utils::capture.output(print(x)), error = function(e) conditionMessage(e))
  shiny::tags$pre(style = "max-height:520px; overflow:auto;",
                  paste(utils::head(out, max_lines), collapse = "\n"))
}

mod_analysis_quant_server <- function(id, lang_r = function() "en") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iphra_session <- get_session(session)
    tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))

    output$lbl_dap <- shiny::renderText(tx("Data Analysis Plan"))
    output$lbl_results <- shiny::renderText(tx("Results"))
    output$lbl_tables <- shiny::renderText(tx("Tables"))
    output$lbl_viz <- shiny::renderText(tx("Visualization"))
    for (o in c("lbl_results", "lbl_tables", "lbl_viz")) {
      shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
    }

    analytics_r <- iphra_session$get_module_reactive("analytics")
    analytics <- shiny::reactive(analytics_r() %||% list())
    refresh <- shiny::reactiveVal(0L)

    current <- shiny::reactive({
      an <- analytics()
      nm <- input$analytics_obj
      if (is.null(nm) || !nzchar(nm) || !nm %in% names(an)) return(NULL)
      an[[nm]]
    })

    # ---- field access helpers ------------------------------------------------
    public_fields <- c("tables", "visualizations")

    get_field <- function(obj, name) {
      if (name %in% public_fields) return(tryCatch(obj[[name]], error = function(e) NULL))
      for (f in c(paste0("..", name), name)) {
        v <- tryCatch(obj$get(field = f), error = function(e) NULL)
        if (!is.null(v)) return(v)
      }
      tryCatch(obj[[name]], error = function(e) NULL)
    }

    set_field <- function(obj, name, value) {
      err <- NULL
      candidates <- if (name %in% public_fields) name else c(paste0("..", name), name)
      for (f in candidates) {
        r <- tryCatch({ obj$set(field = f, value = value); TRUE },
                      error = function(e) { err <<- e; FALSE })
        if (isTRUE(r)) return(NULL)
      }
      err
    }

    dap_of <- function(obj) get_field(obj, "data_analysis_plan")

    dap_df_of <- function(obj) {
      dap <- dap_of(obj)
      if (is.null(dap)) return(NULL)
      df <- iphra_log_get(dap, "log_df")
      if (is.data.frame(df)) df else NULL
    }

    # ---- toolbar -------------------------------------------------------------
    output$toolbar <- shiny::renderUI({
      an <- analytics()
      if (!length(an)) return(NULL)
      choices <- stats::setNames(names(an), vapply(names(an), function(n) {
        paste0(n, " (", class(an[[n]])[1], ")")
      }, character(1)))
      btn <- function(x, color) shiny::tagAppendAttributes(x, class = paste("phr-act", paste0("phr-act--", color)))
      phr_toolbar(
        shiny::tags$style(shiny::HTML(
          ".phr-act{width:100%; height:100%; min-height:34px; font-size:11px; padding:4px 5px;
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
            style = "min-width:300px;",
            shiny::selectInput(ns("analytics_obj"), label = NULL, choices = choices,
                               selected = shiny::isolate(input$analytics_obj), width = "100%")
          ),
          shiny::div(
            style = "display:grid; grid-template-columns:repeat(7, minmax(100px, 1fr)); gap:6px; flex:1;",
            btn(shiny::actionButton(ns("gen_one"), tx("Generate DAP"), icon = shiny::icon("list-alt")), "blue"),
            btn(shiny::actionButton(ns("validate_one"), tx("Validate DAP"), icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::actionButton(ns("save_one"), tx("Save DAP"), icon = shiny::icon("save")), "teal"),
            btn(shiny::actionButton(ns("run_one"), tx("Run Analysis"), icon = shiny::icon("play")), "grey"),
            btn(shiny::actionButton(ns("import_one"), tx("Import Results"), icon = shiny::icon("file-import")), "purple"),
            btn(shiny::downloadButton(ns("export_one"), tx("Export Results")), "orange"),
            shiny::uiOutput(ns("complete_one_ui"), style = "display:contents;"),
            btn(shiny::actionButton(ns("gen_all"), tx("Generate All DAP"), icon = shiny::icon("list")), "blue"),
            btn(shiny::actionButton(ns("validate_all"), tx("Validate All DAP"), icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::actionButton(ns("save_all"), tx("Save All DAPs"), icon = shiny::icon("floppy-disk")), "teal"),
            btn(shiny::actionButton(ns("run_all"), tx("Run All Analyses"), icon = shiny::icon("forward")), "grey"),
            btn(shiny::actionButton(ns("import_all"), tx("Import All Results"), icon = shiny::icon("file-import")), "purple"),
            btn(shiny::downloadButton(ns("export_all"), tx("Export All Results")), "orange"),
            shiny::uiOutput(ns("complete_all_ui"), style = "display:contents;")
          )
        )
      )
    })

    output$empty <- shiny::renderUI({
      if (length(analytics())) return(NULL)
      phr_info_panel(
        tx("Generate the Data Analytics objects in the Data Monitoring and Quality step first."),
        title = tx("No analytics objects available")
      )
    })

    # ---- DAP working copies --------------------------------------------------
    work <- new.env(parent = emptyenv())
    work_ver <- shiny::reactiveVal(0L)

    ensure_work <- function(obj, nm) {
      if (!exists(nm, envir = work, inherits = FALSE)) {
        orig <- dap_df_of(obj)
        if (is.null(orig)) return(NULL)
        assign(nm, iphra_display_frame(as.data.frame(orig, stringsAsFactors = FALSE)), envir = work)
      }
      get(nm, envir = work, inherits = FALSE)
    }

    output$dap_table <- rhandsontable::renderRHandsontable({
      refresh(); work_ver()
      obj <- current(); shiny::req(obj)
      df <- ensure_work(obj, input$analytics_obj)
      shiny::req(is.data.frame(df))
      rhandsontable::rhandsontable(df, height = 560, rowHeaders = NULL, stretchH = "none") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_cols(columnSorting = FALSE, manualColumnResize = TRUE)
    })

    shiny::observeEvent(input$dap_table, {
      nm <- input$analytics_obj
      if (is.null(nm) || !nzchar(nm)) return()
      df <- tryCatch(rhandsontable::hot_to_r(input$dap_table), error = function(e) NULL)
      if (is.data.frame(df)) assign(nm, df, envir = work)
    })

    # Working copy with the stored column order and classes restored.
    working_dap <- function(obj, nm) {
      if (!exists(nm, envir = work, inherits = FALSE)) return(NULL)
      w <- get(nm, envir = work, inherits = FALSE)
      orig <- dap_df_of(obj)
      if (is.null(orig) || !all(names(orig) %in% names(w))) return(NULL)
      w <- w[, names(orig), drop = FALSE]
      for (cl in names(orig)) {
        cls <- class(orig[[cl]])[1]
        if (!identical(class(w[[cl]])[1], cls)) {
          w[[cl]] <- tryCatch(methods::as(w[[cl]], cls), error = function(e) w[[cl]])
        }
      }
      tibble::as_tibble(w)
    }

    notify_err <- function(nm, e, what = NULL) {
      shiny::showNotification(
        paste0(nm, if (!is.null(what)) paste0(" (", what, ")") else "", ": ",
               if (inherits(e, "error")) conditionMessage(e) else e),
        type = "error", duration = 15)
    }

    save_dap <- function(obj, nm) {
      df <- working_dap(obj, nm)
      if (is.null(df)) return(0L)
      dap <- dap_of(obj)
      res <- tryCatch(
        obj$set(field = "..data_analysis_plan", member = "..log_df", value = df),
        error = function(e) iphra_log_set(dap, "log_df", df) %||% TRUE
      )
      if (inherits(res, "error")) {
        notify_err(nm, res)
        return(0L)
      }
      1L
    }

    touch <- function() {
      iphra_session$touch_module("analytics")
      refresh(refresh() + 1L)
    }

    each_obj <- function(objs, f) {
      n <- 0L
      for (nm in names(objs)) n <- n + f(objs[[nm]], nm)
      n
    }

    report <- function(label, n, total = NULL) {
      shiny::showNotification(
        paste0(label, ": ", n, if (!is.null(total)) paste0("/", total) else ""),
        type = if (n > 0) "message" else "warning")
    }

    shiny::observeEvent(input$save_one, {
      obj <- current(); shiny::req(obj)
      n <- save_dap(obj, input$analytics_obj)
      touch(); report(tx("DAPs saved"), n)
    })
    shiny::observeEvent(input$save_all, {
      an <- analytics(); if (!length(an)) return()
      n <- each_obj(an, save_dap)
      touch(); report(tx("DAPs saved"), n, length(an))
    })

    # The DAPs are built when the analytics object is created, so "generate"
    # reloads the stored plan into the editor (dropping unsaved edits).
    reload_dap <- function(obj, nm) {
      if (exists(nm, envir = work, inherits = FALSE)) rm(list = nm, envir = work)
      as.integer(!is.null(dap_df_of(obj)))
    }
    shiny::observeEvent(input$gen_one, {
      obj <- current(); shiny::req(obj)
      n <- reload_dap(obj, input$analytics_obj)
      work_ver(work_ver() + 1L); touch()
      if (n == 0L) shiny::showNotification(tx("No data analysis plan is available for this object."), type = "warning")
      else report(tx("DAPs loaded"), n)
    })
    shiny::observeEvent(input$gen_all, {
      an <- analytics(); if (!length(an)) return()
      n <- each_obj(an, reload_dap)
      work_ver(work_ver() + 1L); touch()
      report(tx("DAPs loaded"), n, length(an))
    })

    # ---- validation ----------------------------------------------------------
    dap_validated <- function(obj) {
      dap <- dap_of(obj)
      if (is.null(dap)) return(NA)
      v <- iphra_log_get(dap, "validated")
      if (is.logical(v) && length(v) == 1L) v else NA
    }

    validate_dap <- function(obj, nm) {
      save_dap(obj, nm)
      dap <- dap_of(obj)
      if (is.null(dap)) return(0L)
      res <- tryCatch(dap$validate(), error = function(e) e)
      if (inherits(res, "error")) {
        res2 <- tryCatch(obj$validate_plan(), error = function(e) e)
        if (inherits(res2, "error")) { notify_err(nm, res); return(0L) }
      }
      1L
    }

    report_validation <- function(objs) {
      v <- vapply(objs, dap_validated, logical(1))
      bad <- sum(!v, na.rm = TRUE)
      shiny::showNotification(
        paste0(tx("DAPs validated"), ": ", sum(v, na.rm = TRUE), "/", length(v),
               if (bad > 0) paste0(" (", bad, " ", tx("with issues"), ")") else ""),
        type = if (bad > 0) "warning" else "message")
    }

    shiny::observeEvent(input$validate_one, {
      obj <- current(); shiny::req(obj)
      validate_dap(obj, input$analytics_obj)
      touch(); report_validation(stats::setNames(list(obj), input$analytics_obj))
    })
    shiny::observeEvent(input$validate_all, {
      an <- analytics(); if (!length(an)) return()
      each_obj(an, validate_dap)
      touch(); report_validation(an)
    })

    output$dap_status <- shiny::renderUI({
      refresh()
      obj <- current(); shiny::req(obj)
      if (is.null(dap_df_of(obj))) {
        return(shiny::div(class = "text-muted", tx("No data analysis plan is available for this object.")))
      }
      v <- dap_validated(obj)
      spec <- if (isTRUE(v)) list("circle-check", "#2e7d32", tx("DAP validated"))
      else if (isFALSE(v)) list("circle-xmark", "#c62828", tx("DAP not validated"))
      else list("circle-question", "#9e9e9e", tx("Validation status unknown"))
      shiny::span(style = paste0("color:", spec[[2]]), shiny::icon(spec[[1]]), " ", spec[[3]])
    })

    # ---- run analysis --------------------------------------------------------
    failed_step <- function(res) {
      inherits(res, "error") ||
        isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE))
    }

    run_obj <- function(obj, nm) {
      save_dap(obj, nm)
      res <- tryCatch(obj$run_analysis(), error = function(e) e)
      if (failed_step(res)) {
        notify_err(nm, if (inherits(res, "error")) res else tx("the analysis reported a failure, see the log."))
        return(0L)
      }
      if (is.function(obj$run_outputs)) {
        out <- tryCatch(obj$run_outputs(), error = function(e) e)
        if (failed_step(out)) {
          notify_err(nm, if (inherits(out, "error")) out else tx("the outputs reported a failure, see the log."),
                     tx("outputs"))
        }
      }
      1L
    }

    shiny::observeEvent(input$run_one, {
      obj <- current(); shiny::req(obj)
      n <- shiny::withProgress(message = tx("Running analysis"), value = 0.5,
                               run_obj(obj, input$analytics_obj))
      touch(); report(tx("Analyses run"), n)
    })
    shiny::observeEvent(input$run_all, {
      an <- analytics(); if (!length(an)) return()
      n <- 0L
      shiny::withProgress(message = tx("Running analyses"), value = 0, {
        for (nm in names(an)) {
          shiny::incProgress(1 / length(an), detail = nm)
          n <- n + run_obj(an[[nm]], nm)
        }
      })
      touch(); report(tx("Analyses run"), n, length(an))
    })

    # ---- mark complete -------------------------------------------------------
    is_complete <- function(obj) {
      dap <- dap_of(obj)
      if (is.null(dap)) return(FALSE)
      md <- iphra_log_get(dap, "metadata")
      is.list(md) && isTRUE(md$complete)
    }

    complete_obj <- function(obj, nm, value = TRUE) {
      if (value) save_dap(obj, nm)
      dap <- dap_of(obj)
      if (is.null(dap)) return(0L)
      md <- iphra_log_get(dap, "metadata")
      if (!is.list(md)) md <- list()
      md$complete <- value
      md$completed_datetime <- if (value) Sys.time() else NULL
      err <- iphra_log_set(dap, "metadata", md)
      if (is.null(err)) 1L else { notify_err(nm, err); 0L }
    }

    complete_btn <- function(id, all) {
      refresh()
      an <- analytics()
      objs <- if (all) an else if (!is.null(current())) stats::setNames(list(current()), input$analytics_obj) else list()
      done <- length(objs) > 0 && all(vapply(objs, is_complete, logical(1)))
      lab <- if (all) {
        if (done) tx("Mark All Analyses Incomplete") else tx("Mark All Analyses Complete")
      } else {
        if (done) tx("Mark Analysis Incomplete") else tx("Mark Analysis Complete")
      }
      shiny::actionButton(ns(id), lab, icon = shiny::icon(if (done) "rotate-left" else "flag-checkered"),
                          class = paste("phr-act", if (done) "phr-act--red" else "phr-act--green"))
    }
    output$complete_one_ui <- shiny::renderUI(complete_btn("complete_one", FALSE))
    output$complete_all_ui <- shiny::renderUI(complete_btn("complete_all", TRUE))

    shiny::observeEvent(input$complete_one, {
      obj <- current(); shiny::req(obj)
      value <- !is_complete(obj)
      n <- complete_obj(obj, input$analytics_obj, value)
      touch()
      report(if (value) tx("Analyses marked complete") else tx("Analyses marked incomplete"), n)
    })
    shiny::observeEvent(input$complete_all, {
      an <- analytics(); if (!length(an)) return()
      value <- !all(vapply(an, is_complete, logical(1)))
      n <- each_obj(an, function(o, nm) complete_obj(o, nm, value))
      touch()
      report(if (value) tx("Analyses marked complete") else tx("Analyses marked incomplete"), n)
    })

    # ---- result browsers -----------------------------------------------------
    nav_state <- function(prefix, field) {
      shiny::reactive({
        refresh()
        obj <- current(); shiny::req(obj)
        iphra_nested_nav(ns, prefix, get_field(obj, field), input)
      })
    }
    results_nav <- nav_state("res", "analysis_results")
    tables_nav <- nav_state("tbl", "tables")
    viz_nav <- nav_state("viz", "visualizations")

    empty_msg <- function(x, msg) {
      if (is.null(x) || (is.list(x) && length(x) == 0L)) {
        shiny::div(class = "text-muted", msg)
      } else NULL
    }

    output$results_nav <- shiny::renderUI({
      nv <- results_nav()
      msg <- empty_msg(nv$leaf, tx("No analysis results available."))
      if (!is.null(msg)) return(msg)
      hide <- NULL
      if (is.data.frame(nv$leaf)) {
        cols <- names(nv$leaf)
        hide <- shiny::div(
          class = "phr-hide-col", style = "min-width:200px; flex:1;",
          shiny::tags$style(shiny::HTML(
            ".phr-hide-col .selectize-input, .phr-hide-col .selectize-input input,
             .phr-hide-col .selectize-dropdown{font-size:12px;}")),
          shiny::selectizeInput(
            ns("hide_res"), label = NULL, choices = cols, multiple = TRUE, width = "100%",
            selected = intersect(shiny::isolate(input$hide_res), cols),
            options = list(placeholder = tx("Hide Columns"), plugins = list("remove_button")))
        )
      }
      shiny::div(style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-start;",
                 nv$ui, hide)
    })
    output$results_view <- shiny::renderUI({
      leaf <- results_nav()$leaf
      if (is.null(leaf) || (is.list(leaf) && !length(leaf))) return(NULL)
      if (is.data.frame(leaf)) rhandsontable::rHandsontableOutput(ns("results_hot"), height = "auto")
      else if (inherits(leaf, "flextable")) iphra_table_html(leaf)
      else iphra_print_leaf(leaf)
    })
    output$results_hot <- rhandsontable::renderRHandsontable({
      leaf <- results_nav()$leaf
      shiny::req(is.data.frame(leaf))
      df <- iphra_display_frame(as.data.frame(leaf))
      df <- df[, setdiff(names(df), input$hide_res), drop = FALSE]
      shiny::req(ncol(df) > 0)
      rhandsontable::rhandsontable(df, readOnly = TRUE, height = 520, rowHeaders = NULL,
                                   stretchH = "none") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE)
    })

    output$tables_nav <- shiny::renderUI({
      nv <- tables_nav()
      empty_msg(nv$leaf, tx("No tables available.")) %||% nv$ui
    })
    output$tables_view <- shiny::renderUI({
      leaf <- tables_nav()$leaf
      if (is.null(leaf) || (is.list(leaf) && !is.data.frame(leaf) && !length(leaf))) return(NULL)
      if (is.data.frame(leaf) || inherits(leaf, "flextable")) iphra_table_html(leaf)
      else iphra_print_leaf(leaf)
    })

    plot_classes <- c("ggplot", "gg", "gtable", "grob", "recordedplot")
    output$viz_nav <- shiny::renderUI({
      nv <- viz_nav()
      empty_msg(nv$leaf, tx("No visualizations available.")) %||% nv$ui
    })
    output$viz_view <- shiny::renderUI({
      leaf <- viz_nav()$leaf
      if (is.null(leaf) || (is.list(leaf) && !length(leaf))) return(NULL)
      if (inherits(leaf, plot_classes)) shiny::plotOutput(ns("viz_plot"), height = 480)
      else if (is.data.frame(leaf) || inherits(leaf, "flextable")) iphra_table_html(leaf)
      else iphra_print_leaf(leaf)
    })
    output$viz_plot <- shiny::renderPlot({
      leaf <- viz_nav()$leaf
      shiny::req(inherits(leaf, plot_classes))
      print(leaf)
    })

    # ---- export --------------------------------------------------------------
    safe_sheet <- function(label, used) {
      base <- substr(gsub("[\\[\\]:*?/\\\\]", "_", label, perl = TRUE), 1, 31)
      out <- base
      i <- 1L
      while (tolower(out) %in% tolower(used)) {
        i <- i + 1L
        tag <- paste0("_", i)
        out <- paste0(substr(base, 1, 31 - nchar(tag)), tag)
      }
      out
    }

    # DAP plus every data frame found in the results and tables of each object.
    result_sheets <- function(objs) {
      sheets <- list()
      add <- function(label, df) {
        sheets[[safe_sheet(label, names(sheets))]] <<- iphra_display_frame(as.data.frame(df))
      }
      multi <- length(objs) > 1L
      for (nm in names(objs)) {
        obj <- objs[[nm]]
        pre <- if (multi) paste0(nm, " ") else ""
        dap <- working_dap(obj, nm) %||% dap_df_of(obj)
        if (is.data.frame(dap)) add(paste0(pre, "DAP"), dap)
        for (field in c("analysis_results", "tables")) {
          items <- iphra_flatten_items(get_field(obj, field))
          items <- items[vapply(items, is.data.frame, logical(1))]
          for (k in names(items)) add(paste0(pre, if (field == "tables") "T " else "", k), items[[k]])
        }
      }
      if (!length(sheets)) sheets[["Results"]] <- data.frame()
      sheets
    }

    output$export_one <- shiny::downloadHandler(
      filename = function() {
        paste0("analysis_results_", gsub("[^A-Za-z0-9_-]", "_", input$analytics_obj %||% "analytics"),
               "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        obj <- current(); shiny::req(obj)
        writexl::write_xlsx(result_sheets(stats::setNames(list(obj), input$analytics_obj)), path = file)
      }
    )
    output$export_all <- shiny::downloadHandler(
      filename = function() paste0("analysis_results_all_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        an <- analytics(); shiny::req(length(an) > 0)
        writexl::write_xlsx(result_sheets(an), path = file)
      }
    )

    # ---- import results ------------------------------------------------------
    imp <- shiny::reactiveVal(NULL)

    open_import <- function(mode) {
      an <- analytics()
      if (!length(an)) return()
      target <- if (mode == "one") input$analytics_obj else NULL
      if (mode == "one" && is.null(target)) return()
      imp(list(mode = mode, target = target, sheets = NULL))
      shiny::showModal(shiny::modalDialog(
        title = if (mode == "one") paste(tx("Import Results"), "-", target) else tx("Import All Results"),
        size = "l", easyClose = FALSE,
        shiny::p(class = "text-muted",
                 tx("Choose an Excel, CSV or RDS file, then match each sheet to an analytics object and a results field. Imported items are added to the stored results of that object.")),
        shiny::fileInput(ns("res_file"), label = NULL, accept = c(".xlsx", ".xls", ".xlsm", ".csv", ".txt", ".rds")),
        shiny::uiOutput(ns("res_map_ui")),
        footer = shiny::tagList(
          shiny::modalButton(tx("Cancel")),
          shiny::actionButton(ns("confirm_res_import"), tx("Import"), class = "btn-primary")
        )
      ))
    }
    shiny::observeEvent(input$import_one, open_import("one"))
    shiny::observeEvent(input$import_all, open_import("all"))

    read_results_file <- function(path, name) {
      if (grepl("\\.rds$", name, ignore.case = TRUE)) {
        x <- readRDS(path)
        if (is.data.frame(x)) x <- list(Results = x)
        if (!is.list(x)) stop("The RDS file must contain a data frame or a list.")
        flat <- iphra_flatten_items(x)
        if (is.null(names(flat)) || !length(flat)) stop("The RDS file is empty.")
        return(flat)
      }
      iphra_read_import_file(path, name)
    }

    shiny::observeEvent(input$res_file, {
      f <- input$res_file
      p <- imp(); shiny::req(p)
      parsed <- tryCatch(read_results_file(f$datapath, f$name), error = function(e) e)
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

    guess_object <- function(sheet, nms) {
      safe <- tolower(gsub("[\\[\\]:*?/\\\\]", "_", nms, perl = TRUE))
      s <- tolower(sheet)
      hit <- which(vapply(safe, function(z) startsWith(s, substr(z, 1, 20)), logical(1)))
      if (length(hit)) nms[hit[1]] else "__skip__"
    }

    output$res_map_ui <- shiny::renderUI({
      p <- imp()
      if (is.null(p) || is.null(p$sheets)) return(NULL)
      nms <- names(analytics())
      fields <- stats::setNames(c("skip", "analysis_results", "tables"),
                                c(tx("Do not import"), tx("Results"), tx("Tables")))
      obj_choices <- c(stats::setNames("__skip__", tx("(do not import)")), nms)
      rows <- lapply(seq_along(p$sheets), function(i) {
        df <- p$sheets[[i]]
        dims <- if (is.data.frame(df)) {
          paste0(format(nrow(df), big.mark = ","), " ", tx("rows"), " \u00d7 ", ncol(df), " ", tx("columns"))
        } else class(df)[1]
        shiny::tags$tr(
          shiny::tags$td(shiny::strong(names(p$sheets)[i]), shiny::tags$br(),
                         shiny::span(class = "text-muted", dims)),
          shiny::tags$td(shiny::selectInput(ns(paste0("resfield_", i)), NULL, fields,
                                            selected = "analysis_results", width = "100%")),
          if (p$mode == "all") {
            shiny::tags$td(shiny::selectInput(ns(paste0("resobj_", i)), NULL, obj_choices, width = "100%",
                                              selected = guess_object(names(p$sheets)[i], nms)))
          }
        )
      })
      shiny::tags$table(
        class = "table table-condensed phr-import-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(tx("Sheet")), shiny::tags$th(tx("Import as")),
          if (p$mode == "all") shiny::tags$th(tx("Analytics object"))
        )),
        shiny::tags$tbody(rows)
      )
    })

    shiny::observeEvent(input$confirm_res_import, {
      p <- imp()
      if (is.null(p) || is.null(p$sheets)) {
        shiny::showNotification(tx("Select a file first."), type = "warning")
        return()
      }
      an <- analytics()
      # Collect per object and field so each stored list is set once.
      pending <- list()
      for (i in seq_along(p$sheets)) {
        field <- input[[paste0("resfield_", i)]]
        if (is.null(field) || field == "skip") next
        nm <- if (p$mode == "one") p$target else input[[paste0("resobj_", i)]]
        if (is.null(nm) || nm == "__skip__" || is.null(an[[nm]])) next
        key <- paste(nm, field, sep = "\r")
        if (is.null(pending[[key]])) pending[[key]] <- list(nm = nm, field = field, items = list())
        pending[[key]]$items[[names(p$sheets)[i]]] <- p$sheets[[i]]
      }
      done <- 0L
      for (pe in pending) {
        obj <- an[[pe$nm]]
        existing <- get_field(obj, pe$field)
        if (!is.list(existing) || is.data.frame(existing) || !is.null(attr(existing, "class"))) existing <- list()
        for (k in names(pe$items)) existing[[k]] <- pe$items[[k]]
        err <- set_field(obj, pe$field, existing)
        if (is.null(err)) done <- done + length(pe$items) else notify_err(pe$nm, err, pe$field)
      }
      shiny::removeModal()
      touch()
      report(tx("Results imported"), done)
    })
  })
}
