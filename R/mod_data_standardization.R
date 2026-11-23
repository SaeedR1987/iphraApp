#' Data standardization module
#'
#' Pick one of the imported `phr::Data` objects (stored in the `"datasets"`
#' session module), run its `validate()`, `standardize()` and
#' `data_diagnose()` methods, and view its `data_diagnostics` field read-only.
#'
#' @param id Module id.
#' @param lang_r Reactive returning the active UI language.
#' @noRd
mod_data_standardization_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("toolbar")),
    shiny::uiOutput(ns("empty")),
    shiny::div(
      style = "display:flex; gap:12px; align-items:flex-start;",
      shiny::div(style = "flex:3; min-width:0; overflow:auto;",
                 rhandsontable::rHandsontableOutput(ns("calc_vars"), height = "auto")),
      shiny::div(style = "flex:7; min-width:0; overflow:auto;",
                 rhandsontable::rHandsontableOutput(ns("diagnostics"), height = "auto"))
    )
  )
}

#' Indicator mapping status of a Data object.
#'
#' Rebuilds and returns the private `..indicator_status` table (phr Data
#' class): indicator name, variables found / not found in the variable map,
#' whether all variables are in the value map, and a green / yellow / red row
#' status.
#' @noRd
iphra_indicator_status <- function(obj, stage = "raw") {
  priv <- tryCatch(obj$.__enclos_env__$private, error = function(e) NULL)
  if (!is.null(priv) && is.function(priv$..build_indicator_status)) {
    try(priv$..build_indicator_status(stage = stage), silent = TRUE)
  }
  out <- tryCatch(obj$get(field = paste0("..indicator_status_", stage)), error = function(e) NULL)
  if (is.data.frame(out)) out else NULL
}
mod_data_standardization_server <- function(id, lang_r = function() "en") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iphra_session <- get_session(session)
    tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))
    max_rows <- 5000L

    datasets_r <- iphra_session$get_module_reactive("datasets")
    datasets <- shiny::reactive(datasets_r() %||% list())
    refresh <- shiny::reactiveVal(0L)

    iphra_schema_repair_server(input, output, session, iphra_session, datasets, tx)

    current <- shiny::reactive({
      ds <- datasets()
      nm <- input$dataset
      if (is.null(nm) || !nm %in% names(ds)) return(NULL)
      ds[[nm]]
    })

    available_stages <- shiny::reactive({
      datasets()
      refresh()
      iphra_available_stages(current())
    })

    output$stage_ui <- shiny::renderUI({
      st <- available_stages()
      if (!length(st)) return(NULL)
      sel <- shiny::isolate(input$stage)
      if (is.null(sel) || !sel %in% st) sel <- st[1]
      shiny::div(
        style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
        shiny::span(style = "font-size:13px; font-weight:600;", tx("Data Object Selection")),
        shiny::radioButtons(ns("stage"), label = NULL, inline = TRUE,
                            choices = stats::setNames(st, vapply(st, tx, character(1))),
                            selected = sel)
      )
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
            shiny::uiOutput(ns("stage_ui"))
          ),
          shiny::div(
            style = "display:grid; grid-template-columns:repeat(6, minmax(100px, 1fr)); gap:6px; flex:1;",
            btn(shiny::actionButton(ns("validate"), tx("Validate"), icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::actionButton(ns("standardize"), tx("Standardize"), icon = shiny::icon("list-check")), "blue"),
            btn(shiny::actionButton(ns("diagnose"), tx("Diagnose"), icon = shiny::icon("stethoscope")), "teal"),
            btn(shiny::actionButton(ns("gen_analytics"), tx("Generate Analytics"), icon = shiny::icon("chart-column")), "purple"),
            btn(shiny::actionButton(ns("repair"), tx("Repair Mappings"), icon = shiny::icon("wrench")), "orange"),
            shiny::uiOutput(ns("complete_one_ui"), style = "display:contents;"),
            btn(shiny::actionButton(ns("validate_all"), tx("Validate All"), icon = shiny::icon("circle-check")), "yellow"),
            btn(shiny::actionButton(ns("standardize_all"), tx("Standardize All"), icon = shiny::icon("list-check")), "blue"),
            btn(shiny::actionButton(ns("diagnose_all"), tx("Diagnose All"), icon = shiny::icon("stethoscope")), "teal"),
            btn(shiny::actionButton(ns("gen_analytics_all"), tx("Generate Analytics All"), icon = shiny::icon("chart-column")), "purple"),
            shiny::div(),
            shiny::uiOutput(ns("complete_all_ui"), style = "display:contents;")
          )
        )
      )
    })

    # Completion is session-local (not stored on the Data object).
    completed <- shiny::reactiveVal(character(0))
    complete_btn <- function(id, all) {
      ds <- datasets()
      nms <- if (all) names(ds) else input$dataset
      done <- length(nms) > 0 && all(nms %in% completed())
      lab <- if (all) {
        if (done) tx("Mark All Incomplete") else tx("Mark All Complete")
      } else {
        if (done) tx("Mark Incomplete") else tx("Mark Complete")
      }
      shiny::actionButton(ns(id), lab, icon = shiny::icon(if (done) "rotate-left" else "flag-checkered"),
                          class = paste("phr-act", if (done) "phr-act--red" else "phr-act--green"))
    }
    output$complete_one_ui <- shiny::renderUI(complete_btn("complete_one", FALSE))
    output$complete_all_ui <- shiny::renderUI(complete_btn("complete_all", TRUE))
    toggle_complete <- function(nms) {
      shiny::req(length(nms) > 0)
      cur <- completed()
      completed(if (all(nms %in% cur)) setdiff(cur, nms) else union(cur, nms))
    }
    shiny::observeEvent(input$complete_one, toggle_complete(input$dataset))
    shiny::observeEvent(input$complete_all, toggle_complete(names(datasets())))

    output$empty <- shiny::renderUI({
      if (length(datasets())) return(NULL)
      phr_info_panel(
        tx("Import a dataset in the Data Import step first."),
        title = tx("No datasets available")
      )
    })

    sel_stage <- function() {
      st <- available_stages()
      if (!is.null(input$stage) && input$stage %in% st) input$stage else "raw"
    }

    run_method <- function(method, label) {
      obj <- current()
      shiny::req(obj)
      stage <- if (!is.null(input$stage) && input$stage %in% available_stages()) input$stage else "raw"
      if (identical(method, "data_diagnose")) {
        stages <- intersect(c("raw", "standardized", "clean"), iphra_available_stages(obj))
        if (!length(stages)) stages <- "raw"
        res <- NULL
        for (st in stages) {
          tryCatch(
            obj$.__enclos_env__$private$..map_schema_vars(stage = st),
            error = function(e) shiny::showNotification(
              paste0(tx("Mapping schema variables"), ": ", conditionMessage(e)),
              type = "warning", duration = 10
            )
          )
          r <- tryCatch(obj$data_diagnose(stage = st), error = function(e) e)
          if (inherits(r, "error") || isTRUE(tryCatch(phrutils::phr_failed(r), error = function(e) FALSE))) {
            res <- r
            break
          }
          if (identical(st, sel_stage())) res <- r
          if (is.null(res)) res <- r
        }
      } else {
        res <- tryCatch(obj[[method]](), error = function(e) e)
      }
      failed <- inherits(res, "error") ||
        (!is.null(res) && isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE)))
      if (failed) {
        msg <- if (inherits(res, "error")) conditionMessage(res) else tx("the step reported a failure, see the log.")
        shiny::showNotification(paste0(label, ": ", msg), type = "error", duration = 15)
      } else {
        extra <- if (identical(method, "data_diagnose") && is.data.frame(res)) {
          paste0(" (", nrow(res), " ", tx("rows"), ")")
        } else ""
        shiny::showNotification(paste0(label, ": ", tx("done"), extra), type = "message")
        iphra_session$touch_module("datasets")
      }
      refresh(refresh() + 1L)
    }

    # Diagnose the standardized data (used after Standardize / Standardize All)
    diagnose_standardized <- function(o) {
      if (!"standardized" %in% tryCatch(iphra_available_stages(o), error = function(e) character(0))) return(invisible(NULL))
      tryCatch(o$.__enclos_env__$private$..map_schema_vars(stage = "standardized"), error = function(e) NULL)
      tryCatch(o$data_diagnose(stage = "standardized"), error = function(e) e)
    }

    shiny::observeEvent(input$validate, run_method("validate", tx("Validate")))
    shiny::observeEvent(input$standardize, {
      run_method("standardize", tx("Standardize"))
      obj <- current(); shiny::req(obj)
      res <- diagnose_standardized(obj)
      if (inherits(res, "error")) {
        shiny::showNotification(paste0(tx("Diagnose"), ": ", conditionMessage(res)), type = "error", duration = 15)
      }
      iphra_session$touch_module("datasets")
      refresh(refresh() + 1L)
    })
    shiny::observeEvent(input$standardize_all, {
      ds <- datasets()
      shiny::req(length(ds))
      failed <- character(0)
      for (nm in names(ds)) {
        res <- tryCatch(ds[[nm]]$standardize(), error = function(e) e)
        bad <- inherits(res, "error") ||
          (!is.null(res) && isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE)))
        if (bad) failed <- c(failed, nm) else {
          dres <- diagnose_standardized(ds[[nm]])
          if (inherits(dres, "error")) failed <- c(failed, paste0(nm, " (", tx("Diagnose"), ")"))
        }
      }
      iphra_session$touch_module("datasets")
      if (length(failed)) {
        shiny::showNotification(
          paste0(tx("Standardize All"), ": ", tx("failed for"), " ", paste(failed, collapse = ", ")),
          type = "error", duration = 15)
      } else {
        shiny::showNotification(
          paste0(tx("Standardize All"), ": ", tx("done"), " (", length(ds), ")"), type = "message")
      }
      refresh(refresh() + 1L)
    })
    shiny::observeEvent(input$diagnose, run_method("data_diagnose", tx("Diagnose")))

    run_all <- function(label, fn) {
      ds <- datasets()
      shiny::req(length(ds))
      failed <- character(0)
      for (nm in names(ds)) {
        res <- tryCatch(fn(ds[[nm]]), error = function(e) e)
        bad <- inherits(res, "error") ||
          (!is.null(res) && isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE)))
        if (bad) failed <- c(failed, nm)
      }
      iphra_session$touch_module("datasets")
      if (length(failed)) {
        shiny::showNotification(
          paste0(label, ": ", tx("failed for"), " ", paste(failed, collapse = ", ")),
          type = "error", duration = 15)
      } else {
        shiny::showNotification(paste0(label, ": ", tx("done"), " (", length(ds), ")"), type = "message")
      }
      refresh(refresh() + 1L)
    }
    shiny::observeEvent(input$validate_all, run_all(tx("Validate All"), function(o) o$validate()))
    shiny::observeEvent(input$diagnose_all, {
      run_all(tx("Diagnose All"), function(o) {
        out <- NULL
        for (st in intersect(c("raw", "standardized", "clean"), iphra_available_stages(o))) {
          tryCatch(o$.__enclos_env__$private$..map_schema_vars(stage = st), error = function(e) NULL)
          r <- o$data_diagnose(stage = st)
          if (isTRUE(tryCatch(phrutils::phr_failed(r), error = function(e) FALSE))) return(r)
          out <- r
        }
        out
      })
    })

    # ---- analytics generation ------------------------------------------------
    analytics_r <- iphra_session$get_module_reactive("analytics")
    generate_analytics <- function(objs) {
      store <- analytics_r() %||% list()
      created <- 0L
      existing <- 0L
      problems <- character()
      shiny::withProgress(message = tx("Generating analytics"), value = 0, {
        new_keys <- character()
        for (nm in names(objs)) {
          obj <- objs[[nm]]
          types <- tryCatch(names(obj$.__enclos_env__$private$..analytics_registry()),
                            error = function(e) character())
          for (type in types) {
            key <- paste0(nm, " / ", type)
            if (!is.null(store[[key]])) { existing <- existing + 1L; next }
            shiny::incProgress(0, detail = key)
            an <- tryCatch(obj$generate_data_analytics(stage = "standardized", type = type),
                           error = function(e) e)
            if (inherits(an, "error")) {
              problems <- c(problems, paste0(key, ": ", conditionMessage(an)))
            } else if (inherits(an, "DataAnalytics")) {
              store[[key]] <- an
              new_keys <- c(new_keys, key)
              created <- created + 1L
            } else {
              problems <- c(problems, paste0(key, ": ", tx("could not be generated (is the data standardized?)")))
            }
          }
        }
        n <- max(length(new_keys), 1L)
        for (key in new_keys) {
          shiny::incProgress(1 / n, detail = paste(tx("Running quality checks"), key))
          res <- tryCatch(store[[key]]$run_quality_checks(), error = function(e) e)
          failed <- inherits(res, "error") ||
            isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE))
          if (failed) {
            problems <- c(problems, paste0(key, ": ", if (inherits(res, "error")) conditionMessage(res)
                                           else tx("quality checks reported a failure, see the log.")))
          } else if (is.function(store[[key]]$run_outputs)) {
            tryCatch(store[[key]]$run_outputs(), error = function(e) {
              problems <<- c(problems, paste0(key, " (", tx("outputs"), "): ", conditionMessage(e)))
            })
          }
        }
      })
      iphra_session$set_module("analytics", store)
      refresh(refresh() + 1L)
      shiny::showNotification(
        paste0(tx("Analytics"), ": ", created, " ", tx("created"), ", ",
               existing, " ", tx("already existed"), "."),
        type = "message")
      for (p in utils::head(problems, 5L)) shiny::showNotification(p, type = "warning", duration = 15)
    }
    shiny::observeEvent(input$gen_analytics, {
      obj <- current(); shiny::req(obj)
      generate_analytics(stats::setNames(list(obj), input$dataset))
    })
    shiny::observeEvent(input$gen_analytics_all, {
      ds <- datasets()
      shiny::req(length(ds))
      generate_analytics(ds)
    })

    output$calc_vars <- rhandsontable::renderRHandsontable({
      refresh()
      datasets()
      obj <- current()
      shiny::req(obj)
      df <- iphra_indicator_status(obj, sel_stage())
      shiny::req(is.data.frame(df), nrow(df) > 0)
      df <- df[, c("indicator_name", "variables_mapped", "variables_not_mapped",
                   "in_value_map", "status")]
      names(df) <- c(tx("Indicator Name"), tx("In variable map"), tx("Not in variable map"),
                     tx("In value map"), "status")
      rend <- "function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var s = instance.getDataAtRow(row)[4];
          td.style.background = s === 'green' ? '#d4efd6' : (s === 'yellow' ? '#fdf3c4' : '#f8d0d0');
          td.style.color = '#222';
        }"
      hot <- rhandsontable::rhandsontable(
        df, readOnly = TRUE, height = 520, rowHeaders = NULL, stretchH = "none"
      )
      for (i in 1:5) hot <- rhandsontable::hot_col(hot, i, renderer = rend)
      hot <- rhandsontable::hot_cols(hot, manualColumnResize = TRUE,
                                     colWidths = c(170, 200, 200, 90, 1))
      hot$x$hiddenColumns <- list(columns = list(4L), indicators = FALSE)
      hot
    })

    output$diagnostics <- rhandsontable::renderRHandsontable({
      refresh()
      obj <- current()
      shiny::req(obj)
      diag <- obj$get(field = paste0("..data_diagnostics_", sel_stage()))
      shiny::req(is.data.frame(diag), nrow(diag) > 0)
      df <- iphra_display_frame(utils::head(as.data.frame(diag), max_rows))
      rhandsontable::rhandsontable(
        df, readOnly = TRUE, height = 520, rowHeaders = NULL,
        highlightRow = TRUE, highlightCol = TRUE, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE,
                                format = "0.[########]")
    })
  })
}
