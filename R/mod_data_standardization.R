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
    rhandsontable::rHandsontableOutput(ns("diagnostics"), height = "auto")
  )
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
        style = "display:inline-block; vertical-align:middle; margin:0 8px;",
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
      phr_toolbar(
        shiny::div(
          style = "display:inline-block; min-width:320px;",
          shiny::selectInput(ns("dataset"), label = NULL, choices = choices,
                             selected = shiny::isolate(input$dataset), width = "100%")
        ),
        shiny::actionButton(ns("validate"), tx("Validate"),
                            icon = shiny::icon("check"), class = "btn-default"),
        shiny::actionButton(ns("standardize"), tx("Standardize"),
                            icon = shiny::icon("sort-by-attributes", lib = "glyphicon"),
                            class = "btn-primary"),
        shiny::actionButton(ns("standardize_all"), tx("Standardize All"),
                            icon = shiny::icon("sort-by-attributes-alt", lib = "glyphicon"),
                            class = "btn-primary"),
        shiny::uiOutput(ns("stage_ui"), inline = TRUE),
        shiny::actionButton(ns("diagnose"), tx("Run Diagnostics"),
                            icon = shiny::icon("stethoscope"), class = "btn-default"),
        shiny::actionButton(ns("repair"), tx("Repair Mappings"),
                            icon = shiny::icon("wrench"), class = "btn-default")
      )
    })

    output$empty <- shiny::renderUI({
      if (length(datasets())) return(NULL)
      phr_info_panel(
        tx("Import a dataset in the Data Import step first."),
        title = tx("No datasets available")
      )
    })

    run_method <- function(method, label) {
      obj <- current()
      shiny::req(obj)
      stage <- if (!is.null(input$stage) && input$stage %in% available_stages()) input$stage else "raw"
      if (identical(method, "data_diagnose")) {
        # Re-map schema variables so schema edits are reflected in the maps
        tryCatch(
          obj$.__enclos_env__$private$..map_schema_vars(stage = stage),
          error = function(e) shiny::showNotification(
            paste0(tx("Mapping schema variables"), ": ", conditionMessage(e)),
            type = "warning", duration = 10
          )
        )
      }
      res <- tryCatch(
        if (identical(method, "data_diagnose")) obj$data_diagnose(stage = stage) else obj[[method]](),
        error = function(e) e
      )
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

    shiny::observeEvent(input$validate, run_method("validate", tx("Validate")))
    shiny::observeEvent(input$standardize, run_method("standardize", tx("Standardize")))
    shiny::observeEvent(input$standardize_all, {
      ds <- datasets()
      shiny::req(length(ds))
      failed <- character(0)
      for (nm in names(ds)) {
        res <- tryCatch(ds[[nm]]$standardize(), error = function(e) e)
        bad <- inherits(res, "error") ||
          (!is.null(res) && isTRUE(tryCatch(phrutils::phr_failed(res), error = function(e) FALSE)))
        if (bad) failed <- c(failed, nm)
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
    shiny::observeEvent(input$diagnose, run_method("data_diagnose", tx("Run Diagnostics")))

    output$diagnostics <- rhandsontable::renderRHandsontable({
      refresh()
      obj <- current()
      shiny::req(obj)
      diag <- obj$get(field = "..data_diagnostics")
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
