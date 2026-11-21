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
                                     rhandsontable::rHandsontableOutput(ns("cleaning_log"), height = "auto"))),
          shiny::tabPanel(shiny::textOutput(ns("lbl_deletion"), inline = TRUE), value = "deletion",
                          shiny::div(style = "margin-top:10px;",
                                     rhandsontable::rHandsontableOutput(ns("deletion_log"), height = "auto")))
        )
      ),
      shiny::column(
        6,
        shiny::div(style = "margin-top:6px;",
                   shiny::uiOutput(ns("stage_select"))),
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
      phr_toolbar(
        shiny::div(
          style = "display:inline-block; min-width:320px;",
          shiny::selectInput(ns("dataset"), label = NULL, choices = choices,
                             selected = shiny::isolate(input$dataset), width = "100%")
        ),
        shiny::div(style = "display:inline-block; vertical-align:middle; margin:0 8px;",
                   shiny::uiOutput(ns("gen_stage_ui"), inline = TRUE)),
        shiny::actionButton(ns("gen_one"), tx("Generate Cleaning Log"),
                            icon = shiny::icon("list-alt"), class = "btn-primary"),
        shiny::actionButton(ns("gen_all"), tx("Generate All Cleaning Logs"),
                            icon = shiny::icon("list"), class = "btn-default")
      )
    })

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
      shiny::radioButtons(ns("gen_stage"), NULL, inline = TRUE,
                          choices = stats::setNames(avail, labels[avail]), selected = sel)
    })

    gen_stage <- function() {
      st <- input$gen_stage
      if (is.null(st) || !st %in% c("raw", "standardized")) "standardized" else st
    }

    generate <- function(obj, label) {
      stage <- gen_stage()
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
      if (generate(obj, input$dataset)) {
        n_of <- function(field) {
          lg <- tryCatch(obj$get(field = field), error = function(e) NULL)
          df <- if (is.null(lg)) NULL else tryCatch(lg$get("log_df"), error = function(e) NULL)
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

    log_table <- function(field) {
      refresh()
      obj <- current(); shiny::req(obj)
      lg <- tryCatch(obj$get(field = field), error = function(e) NULL)
      df <- if (is.null(lg)) NULL else tryCatch(lg$get("log_df"), error = function(e) NULL)
      shiny::req(is.data.frame(df), nrow(df) > 0)
      rhandsontable::rhandsontable(
        iphra_display_frame(utils::head(as.data.frame(df), max_rows)),
        readOnly = TRUE, height = 520, rowHeaders = NULL, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE)
    }
    output$cleaning_log <- rhandsontable::renderRHandsontable(log_table("..cleaning_log"))
    output$deletion_log <- rhandsontable::renderRHandsontable(log_table("..deletion_log"))

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
      shiny::radioButtons(ns("stage"), NULL, inline = TRUE,
                          choices = stats::setNames(avail, labels[avail]), selected = sel)
    })

    output$data_view <- rhandsontable::renderRHandsontable({
      refresh()
      obj <- current(); shiny::req(obj, input$stage)
      df <- stage_data(obj, input$stage)
      shiny::req(df)
      rhandsontable::rhandsontable(
        iphra_display_frame(utils::head(as.data.frame(df), max_rows)),
        readOnly = TRUE, height = 520, rowHeaders = NULL,
        highlightRow = TRUE, highlightCol = TRUE, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE)
    })
  })
}
