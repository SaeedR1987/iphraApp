#' Collect every object whose `..change_log` can be viewed
#'
#' Returns the PHRSession itself plus every `phr::Asset`-based object stored
#' in the session's modules, including nested ones: objects inside list
#' modules (e.g. `"datasets"`), a protocol's `tools`, and objects held in
#' fields of other objects (a Data's cleaning/deletion logs, a DataAnalytics'
#' data analysis plans, ...). Fields are discovered generically from each R6
#' object's public and private environments, so new nested logs appear
#' without changes here. Each object is listed once.
#'
#' @param iphra_session A `PHRSession` object.
#' @return Named list of R6 objects; names are the display labels.
#' @noRd
iphra_log_sources <- function(iphra_session) {
  sources <- list()
  visited <- list()
  max_depth <- 6L

  seen <- function(obj) any(vapply(visited, identical, logical(1), obj))

  r6_fields <- function(obj) {
    out <- list()
    envs <- list(
      obj,
      tryCatch(obj$.__enclos_env__$private, error = function(e) NULL)
    )
    for (env in envs) {
      if (!is.environment(env)) next
      for (nm in setdiff(ls(env, all.names = TRUE), c("clone", "self", "private", "super"))) {
        if (startsWith(nm, ".__") || nm %in% names(out)) next
        if (bindingIsActive(nm, env)) {
          if (!identical(nm, "tools")) next
        }
        val <- tryCatch(get(nm, envir = env), error = function(e) NULL)
        if (inherits(val, "R6") || (is.list(val) && length(val) > 0L)) out[[nm]] <- val
      }
    }
    out
  }

  walk <- function(label, obj, depth) {
    if (depth > max_depth) return(invisible(NULL))
    if (inherits(obj, "R6")) {
      if (seen(obj)) return(invisible(NULL))
      visited[[length(visited) + 1L]] <<- obj
      if (inherits(obj, "Asset") && is.function(obj$get)) {
        sources[[sprintf("%s (%s)", label, class(obj)[1])]] <<- obj
      }
      fields <- r6_fields(obj)
      for (nm in names(fields)) {
        walk(paste0(label, " / ", sub("^\\.\\.", "", nm)), fields[[nm]], depth + 1L)
      }
    } else if (is.list(obj) && !is.data.frame(obj)) {
      for (i in seq_along(obj)) {
        nm <- names(obj)[i]
        if (is.null(nm) || !nzchar(nm)) nm <- as.character(i)
        walk(paste0(label, " / ", nm), obj[[i]], depth + 1L)
      }
    }
    invisible(NULL)
  }

  sources[[sprintf("PHRSession (%s)", class(iphra_session)[1])]] <- iphra_session
  visited[[1L]] <- iphra_session
  for (id in iphra_session$list_modules()) {
    walk(id, iphra_session$get_module(id), 1L)
  }
  sources
}

#' Excel-safe, unique sheet names (max 31 chars) from log display labels
#'
#' Drops the trailing "(Class)" and keeps the most specific end of long paths.
#' @noRd
iphra_log_sheet_names <- function(keys) {
  nm <- sub("\\s*\\([^)]*\\)$", "", keys)
  nm <- gsub("[\\[\\]:*?/\\\\]", "-", nm, perl = TRUE)
  nm <- gsub("\\s*-\\s*", "-", nm)
  nm <- ifelse(nchar(nm) > 28L, substring(nm, nchar(nm) - 27L), nm)
  make.unique(nm, sep = "_")
}

#' Log viewer modal UI (opened from File > Debug Console)
#'
#' @param id Module id.
#' @noRd
mod_log_viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::modalDialog(
    title = phrutils::phr_txt("Debug Console"),
    size = "l",
    easyClose = TRUE,
    footer = shiny::modalButton(phrutils::phr_txt("Close")),
    shiny::selectInput(
      ns("source"),
      label = phrutils::phr_txt("Select log to view"),
      choices = NULL,
      width = "100%"
    ),
    shiny::actionButton(ns("refresh"), phrutils::phr_txt("Refresh"),
                        icon = shiny::icon("refresh")),
    shiny::actionButton(ns("export"), phrutils::phr_txt("Export"),
                        icon = shiny::icon("file-excel")),
    shiny::downloadButton(ns("download_all"), phrutils::phr_txt("Export All")),
    shiny::tags$hr(),
    DT::DTOutput(ns("log_table"))
  )
}

#' Log viewer server
#'
#' @param id Module id.
#' @param open Reactive trigger (e.g. a button counter) fired when the modal is
#'   opened, so the list of available logs is rebuilt.
#' @noRd
mod_log_viewer_server <- function(id, open = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    iphra_session <- get_session(session)
    ns <- session$ns
    reopened <- shiny::reactiveVal(0L)

    sources <- shiny::reactive({
      open()
      reopened()
      input$refresh
      iphra_session$version_signal()()
      iphra_log_sources(iphra_session)
    })

    shiny::observeEvent(input$export, {
      choices <- names(sources())
      shiny::showModal(shiny::modalDialog(
        title = phrutils::phr_txt("Export logs to Excel"),
        easyClose = TRUE,
        shiny::checkboxGroupInput(
          ns("export_sources"),
          label = phrutils::phr_txt("Select one or more logs to export"),
          choices = choices,
          selected = if (isTRUE(input$source %in% choices)) input$source else NULL,
          width = "100%"
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("export_back"), phrutils::phr_txt("Back")),
          shiny::downloadButton(ns("download"), phrutils::phr_txt("Export"))
        )
      ))
    })

    shiny::observeEvent(input$export_back, {
      shiny::showModal(mod_log_viewer_ui("log_viewer"))
      reopened(reopened() + 1L)
    })

    write_logs <- function(src, keys, file) {
      sheets <- lapply(keys, function(k) {
        df <- tryCatch(src[[k]]$get(field = "..change_log"), error = function(e) NULL)
        if (is.data.frame(df)) {
          as.data.frame(df)
        } else {
          data.frame(id = character(0), version = integer(0), name = character(0),
                     datetime = as.POSIXct(character(0)), log_message = character(0))
        }
      })
      names(sheets) <- iphra_log_sheet_names(keys)
      writexl::write_xlsx(sheets, path = file)
    }
    export_filename <- function() {
      paste0("iphra_logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    }

    output$download <- shiny::downloadHandler(
      filename = export_filename,
      content = function(file) {
        src <- sources()
        keys <- intersect(input$export_sources, names(src))
        if (length(keys) == 0L) {
          shiny::showNotification(phrutils::phr_txt("Select at least one log to export."),
                                  type = "warning")
          stop("No logs selected for export.")
        }
        write_logs(src, keys, file)
      }
    )

    output$download_all <- shiny::downloadHandler(
      filename = export_filename,
      content = function(file) {
        src <- sources()
        write_logs(src, names(src), file)
      }
    )

    shiny::observeEvent(sources(), {
      choices <- names(sources())
      selected <- if (isTRUE(input$source %in% choices)) input$source else choices[1]
      shiny::updateSelectInput(session, "source", choices = choices, selected = selected)
    })

    output$log_table <- DT::renderDT({
      src <- sources()
      key <- input$source
      shiny::req(key, key %in% names(src))
      log_df <- tryCatch(src[[key]]$get(field = "..change_log"), error = function(e) NULL)
      if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0L) {
        log_df <- data.frame(
          datetime = as.POSIXct(character(0)), version = integer(0),
          name = character(0), log_message = character(0)
        )
      }
      cols <- intersect(c("datetime", "version", "name", "log_message"), names(log_df))
      DT::datatable(
        as.data.frame(log_df)[, cols, drop = FALSE],
        rownames = FALSE,
        options = list(pageLength = 15, order = list(), scrollX = TRUE),
        class = "compact stripe"
      )
    })
  })
}
