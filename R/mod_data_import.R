#' Data import module
#'
#' Lets the user pick an Excel or CSV file, map each sheet to a `phr` Data
#' class in an Import Manager modal, and browse the imported datasets in a
#' read-only table. Imported Data objects are stored in the `PHRSession`
#' under the `"datasets"` module (a named list of `phr::Data` objects).
#'
#' @param id Module id.
#' @param lang_r Reactive returning the active UI language.
#' @noRd
mod_data_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "display:none;",
      shiny::fileInput(
        ns("file"), label = NULL,
        accept = c(".xlsx", ".xls", ".xlsm", ".csv", ".txt")
      )
    ),
    shiny::uiOutput(ns("toolbar")),
    shiny::uiOutput(ns("empty")),
    rhandsontable::rHandsontableOutput(ns("table"), height = "auto")
  )
}

#' Data types a sheet can be imported as.
#'
#' `hh` marks classes that accept a household-link column (`hh_uuid`).
#' @noRd
iphra_data_types <- function() {
  list(
    household = list(label = "Household data", class = "HouseholdData", hh = FALSE),
    individual = list(label = "Individual / roster data", class = "IndividualData", hh = TRUE),
    death = list(label = "Deaths (individual)", class = "DeathIndividualData", hh = TRUE),
    health = list(label = "Health (individual)", class = "HealthIndividualData", hh = TRUE),
    nutrition = list(label = "Nutrition (individual)", class = "NutritionIndividualData", hh = TRUE),
    woman = list(label = "Women (individual)", class = "WomanIndividualData", hh = TRUE),
    water = list(label = "Water containers", class = "WaterContainerData", hh = TRUE),
    survey = list(label = "Tool: survey sheet", class = NULL, hh = FALSE),
    choices = list(label = "Tool: choices sheet", class = NULL, hh = FALSE)
  )
}

#' Read every sheet of an Excel workbook, or the single table of a CSV.
#' @return Named list of data frames (empty sheets dropped).
#' @noRd
iphra_read_import_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  out <- if (ext %in% c("xlsx", "xlsm", "xls")) {
    sheets <- readxl::excel_sheets(path)
    res <- lapply(sheets, function(s) {
      as.data.frame(readxl::read_excel(path, sheet = s, .name_repair = "unique"),
                    stringsAsFactors = FALSE, check.names = FALSE)
    })
    stats::setNames(res, sheets)
  } else if (ext %in% c("csv", "txt")) {
    df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
    stats::setNames(list(df), tools::file_path_sans_ext(name))
  } else {
    stop("Unsupported file type: .", ext)
  }
  out[vapply(out, function(d) ncol(d) > 0L, logical(1))]
}

# First column matching one of `patterns` (case-insensitive, in priority order).
iphra_guess_column <- function(cols, patterns) {
  hit <- match(tolower(patterns), tolower(cols))
  hit <- hit[!is.na(hit)]
  if (length(hit)) cols[hit[1]] else NULL
}

iphra_guess_type <- function(sheet_name, n_sheets) {
  nm <- tolower(sheet_name)
  if (nm %in% c("survey", "choices")) return(nm)
  if (grepl("death|mortal", nm)) return("death")
  if (grepl("water|container", nm)) return("water")
  if (grepl("nutri|muac|child", nm)) return("nutrition")
  if (grepl("wom[ae]n|repro", nm)) return("woman")
  if (grepl("health", nm)) return("health")
  if (grepl("roster|individual|member|person", nm)) return("individual")
  if (grepl("household|hh|main|survey", nm) || n_sheets == 1L) return("household")
  "skip"
}

# Imported (raw stage) table of a Data object.
iphra_dataset_frame <- function(obj) {
  df <- obj$get_data("raw")
  if (is.null(df)) NULL else as.data.frame(df)
}

# Make every column safe for display in rhandsontable.
iphra_display_frame <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) format(x)
    else if (is.numeric(x) || is.logical(x) || is.character(x)) x
    else as.character(x)
  })
  df
}

mod_data_import_server <- function(id, lang_r = function() "en") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iphra_session <- get_session(session)
    tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))
    types <- iphra_data_types()
    max_rows <- 5000L

    options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 5 * 1024^2),
                                       500 * 1024^2))

    pending <- shiny::reactiveVal(NULL)
    datasets_r <- iphra_session$get_module_reactive("datasets")
    datasets <- shiny::reactive(datasets_r() %||% list())

    go_to_import <- function() {
      shiny::updateNavbarPage(session$rootScope(), "workflow_nav", selected = "import")
    }

    # ---- toolbar and empty state ------------------------------------------
    output$toolbar <- shiny::renderUI({
      ds <- datasets()
      choices <- if (length(ds)) {
        stats::setNames(names(ds), vapply(names(ds), function(n) {
          paste0(n, " (", class(ds[[n]])[1], ")")
        }, character(1)))
      }
      phr_toolbar(
        shiny::actionButton(
          ns("open_import"), tx("Import Dataset(s)"),
          icon = shiny::icon("upload"), class = "btn-primary",
          onclick = "iphraImportFile(); return false;"
        ),
        if (length(ds)) {
          shiny::tagList(
            shiny::div(
              style = "display:inline-block; min-width:320px; margin-left:12px;",
              shiny::selectInput(
                ns("dataset"), label = NULL, choices = choices,
                selected = shiny::isolate(input$dataset), width = "100%"
              )
            ),
            shiny::actionButton(ns("remove_dataset"), tx("Remove dataset"),
                                icon = shiny::icon("trash"), class = "btn-default")
          )
        },
        if (length(ds)) shiny::span(class = "phr-toolbar__info", shiny::textOutput(ns("dataset_info"), inline = TRUE))
      )
    })

    output$empty <- shiny::renderUI({
      if (length(datasets())) return(NULL)
      phr_info_panel(
        tx("Select an Excel or CSV file, then choose which type of dataset each sheet should be imported as."),
        title = tx("No datasets imported yet")
      )
    })

    current <- shiny::reactive({
      ds <- datasets()
      nm <- input$dataset
      if (is.null(nm) || !nm %in% names(ds)) return(NULL)
      ds[[nm]]
    })

    output$dataset_info <- shiny::renderText({
      obj <- current()
      shiny::req(obj)
      df <- iphra_dataset_frame(obj)
      shiny::req(df)
      paste0(class(obj)[1], " \u2022 ", format(nrow(df), big.mark = ","), " ", tx("rows"),
             " \u00d7 ", ncol(df), " ", tx("columns"),
             if (nrow(df) > max_rows) paste0(" \u2022 ", tx("showing first"), " ",
                                              format(max_rows, big.mark = ",")))
    })

    output$table <- rhandsontable::renderRHandsontable({
      obj <- current()
      shiny::req(obj)
      df <- iphra_dataset_frame(obj)
      shiny::req(df)
      df <- iphra_display_frame(utils::head(df, max_rows))
      rhandsontable::rhandsontable(
        df, readOnly = TRUE, height = 520, rowHeaders = NULL,
        highlightRow = TRUE, highlightCol = TRUE, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(columnSorting = TRUE, manualColumnResize = TRUE,
                                format = "0.[########]")
    })

    # ---- file selection -> Import Manager ----------------------------------
    shiny::observeEvent(input$file, {
      f <- input$file
      parsed <- tryCatch(iphra_read_import_file(f$datapath, f$name), error = function(e) e)
      if (inherits(parsed, "error") || !length(parsed)) {
        shiny::showNotification(
          paste(tx("Could not read the file:"), f$name,
                if (inherits(parsed, "error")) conditionMessage(parsed)
                else tx("No data sheets found.")),
          type = "error", duration = 10
        )
        return()
      }
      pending(list(file = f$name, sheets = parsed))
      go_to_import()
      shiny::showModal(import_manager_modal(f$name, parsed))
    })

    import_manager_modal <- function(file, sheets) {
      type_choices <- c(
        stats::setNames("skip", tx("Do not import")),
        stats::setNames(names(types), vapply(types, function(t) tx(t$label), character(1)))
      )
      guesses <- vapply(names(sheets), iphra_guess_type, character(1), n_sheets = length(sheets))
      for (t in c("household", "survey", "choices")) {
        dup <- which(guesses == t)
        if (length(dup) > 1L) guesses[dup[-1L]] <- "skip"
      }
      rows <- lapply(seq_along(sheets), function(i) {
        df <- sheets[[i]]
        cols <- names(df)
        guess <- guesses[[i]]
        uuid_guess <- iphra_guess_column(cols, c("uuid", "_uuid", "person_id", "individual_id",
                                                 "instanceID", "_id", "hhid", "hh_id", "id"))
        hh_guess <- iphra_guess_column(cols, c("hh_uuid", "_parent_index", "parent_uuid",
                                               "_submission__uuid", "hhid", "hh_id"))
        shiny::tags$tr(
          shiny::tags$td(
            shiny::strong(names(sheets)[i]),
            shiny::tags$br(),
            shiny::span(class = "text-muted",
                        paste0(format(nrow(df), big.mark = ","), " ", tx("rows"), " \u00d7 ",
                               ncol(df), " ", tx("columns")))
          ),
          shiny::tags$td(shiny::selectInput(ns(paste0("type_", i)), NULL, type_choices,
                                            selected = guess, width = "100%")),
          shiny::tags$td(shiny::textInput(
            ns(paste0("name_", i)), NULL, width = "100%",
            value = if (length(sheets) == 1L) tools::file_path_sans_ext(file) else names(sheets)[i]
          )),
          shiny::tags$td(shiny::selectInput(
            ns(paste0("uuid_", i)), NULL, width = "100%",
            choices = c(stats::setNames("__generate__", tx("(generate row IDs)")), cols),
            selected = uuid_guess %||% "__generate__"
          )),
          shiny::tags$td(shiny::conditionalPanel(
            condition = sprintf("input.type_%d != 'skip' && input.type_%d != 'household'", i, i),
            ns = ns,
            shiny::selectInput(
              ns(paste0("hh_", i)), NULL, width = "100%",
              choices = c(stats::setNames("__none__", tx("(none)")), cols),
              selected = hh_guess %||% "__none__"
            )
          ))
        )
      })

      shiny::modalDialog(
        title = paste(tx("Import Manager"), "-", file),
        size = "l", easyClose = FALSE,
        shiny::p(class = "text-muted",
                 tx("Choose the type of dataset each sheet should be imported as. Sheets set to 'Do not import' are ignored.")),
        shiny::tags$table(
          class = "table table-condensed phr-import-table",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th(tx("Sheet")), shiny::tags$th(tx("Dataset type")),
            shiny::tags$th(tx("Dataset name")), shiny::tags$th(tx("Unique ID column")),
            shiny::tags$th(tx("Household link column"))
          )),
          shiny::tags$tbody(rows)
        ),
        footer = shiny::tagList(
          shiny::modalButton(tx("Cancel")),
          shiny::actionButton(ns("confirm_import"), tx("Import"), class = "btn-primary")
        )
      )
    }

    # ---- build Data objects and store them in the session -------------------
    shiny::observeEvent(input$confirm_import, {
      p <- pending()
      shiny::req(p)
      existing <- shiny::isolate(datasets())
      added <- character(0)
      problems <- character(0)

      sel <- vapply(seq_along(p$sheets), function(i) input[[paste0("type_", i)]] %||% "skip", character(1))
      if (sum(sel == "household") > 1L || sum(sel == "survey") > 1L || sum(sel == "choices") > 1L) {
        shiny::showNotification(
          tx("Only one sheet can be imported as Household data, one as the survey sheet and one as the choices sheet."),
          type = "error", duration = 10
        )
        return()
      }
      if (xor("survey" %in% sel, "choices" %in% sel)) {
        shiny::showNotification(
          tx("A tool needs both a survey sheet and a choices sheet."),
          type = "error", duration = 10
        )
        return()
      }

      if (all(c("survey", "choices") %in% sel)) {
        tool_name <- tools::file_path_sans_ext(p$file)
        tool <- tryCatch(
          phr::Tool$new(name = tool_name,
                        survey = p$sheets[[which(sel == "survey")]],
                        choices = p$sheets[[which(sel == "choices")]]),
          error = function(e) e
        )
        if (inherits(tool, "error")) {
          problems <- c(problems, paste0(tool_name, ": ", conditionMessage(tool)))
        } else {
          iphra_session$set_module("imported_tool", tool)
          added <- c(added, paste0(tool_name, " (", tx("tool"), ")"))
        }
      }

      for (i in seq_along(p$sheets)) {
        type <- sel[i]
        if (type %in% c("skip", "survey", "choices")) next
        spec <- types[[type]]
        df <- p$sheets[[i]]
        nm <- trimws(input[[paste0("name_", i)]] %||% "")
        if (!nzchar(nm)) nm <- names(p$sheets)[i]
        base <- nm
        k <- 1L
        while (nm %in% c(names(existing), added)) {
          k <- k + 1L
          nm <- paste0(base, " (", k, ")")
        }

        uuid_col <- input[[paste0("uuid_", i)]] %||% "__generate__"
        if (identical(uuid_col, "__generate__")) {
          uuid_col <- if ("uuid" %in% names(df)) "row_uuid" else "uuid"
          df[[uuid_col]] <- as.character(seq_len(nrow(df)))
        }
        gen <- getExportedValue("phr", spec$class)
        args <- list(data = df, dataset_name = nm, uuid = uuid_col)
        hh_col <- input[[paste0("hh_", i)]]
        if (isTRUE(spec$hh) && !is.null(hh_col) && !identical(hh_col, "__none__")) {
          args$hh_uuid <- hh_col
        }

        obj <- tryCatch(do.call(gen$new, args), error = function(e) e)
        if (inherits(obj, "error") || !inherits(obj, "Data")) {
          problems <- c(problems, paste0(
            nm, ": ", if (inherits(obj, "error")) conditionMessage(obj)
            else tx("the dataset could not be created. Check the unique ID column.")
          ))
          next
        }
        existing[[nm]] <- obj
        added <- c(added, nm)
      }

      if (length(problems)) {
        shiny::showNotification(paste(problems, collapse = "\n"), type = "error", duration = 15)
      }
      if (length(added)) {
        if (length(existing) > length(shiny::isolate(datasets()))) {
          iphra_session$set_module("datasets", existing)
        }
        shiny::showNotification(
          paste(tx("Imported:"), paste(added, collapse = ", ")), type = "message"
        )
        if (length(existing)) shiny::updateSelectInput(session, "dataset", selected = utils::tail(names(existing), 1))
        pending(NULL)
        shiny::removeModal()
      } else if (!length(problems)) {
        shiny::showNotification(tx("No sheets were selected for import."), type = "warning")
      }
    })

    # ---- remove a dataset ----------------------------------------------------
    shiny::observeEvent(input$remove_dataset, {
      shiny::req(input$dataset)
      shiny::showModal(shiny::modalDialog(
        title = tx("Remove dataset"), size = "s",
        paste(tx("Remove this dataset from the project?"), input$dataset),
        footer = shiny::tagList(
          shiny::modalButton(tx("Cancel")),
          shiny::actionButton(ns("confirm_remove"), tx("Remove dataset"), class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_remove, {
      ds <- shiny::isolate(datasets())
      ds[[input$dataset]] <- NULL
      iphra_session$set_module("datasets", ds)
      shiny::removeModal()
    })
  })
}
