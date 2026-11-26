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
    shiny::div(
      style = "display:none;",
      shiny::fileInput(
        ns("tool_files"), label = NULL, multiple = TRUE,
        accept = c(".xlsx", ".xls", ".xlsm")
      )
    ),
    shiny::uiOutput(ns("toolbar")),
    shiny::uiOutput(ns("empty")),
    shiny::uiOutput(ns("filters")),
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
    water = list(label = "Water containers", class = "WaterContainerData", hh = TRUE)
  )
}

#' Link newly imported datasets to the HouseholdData object.
#'
#' Uses the household imported in this batch, or else one already loaded, and
#' calls `add_linked_dataset()` for every other newly imported dataset.
#' @param new_objs Named list of `list(type, obj)` created in this import.
#' @param existing Named list of all loaded datasets.
#' @return Character vector of problem messages (empty on success).
#' @noRd
iphra_link_datasets_to_household <- function(new_objs, existing) {
  link_names <- c(
    individual = "roster", death = "deaths", health = "health",
    nutrition = "nutrition", woman = "women", water = "water_containers"
  )
  is_hh <- function(o) inherits(o, "HouseholdData")
  hh <- Filter(is_hh, lapply(new_objs, `[[`, "obj"))
  if (!length(hh)) hh <- Filter(is_hh, existing)
  if (!length(hh)) return(character(0))
  hh <- hh[[1]]

  problems <- character(0)
  for (nm in names(new_objs)) {
    link_name <- link_names[new_objs[[nm]]$type]
    if (is.na(link_name)) next
    res <- tryCatch(
      hh$add_linked_dataset(name = unname(link_name), other_object = new_objs[[nm]]$obj,
                            by_self_role = "uuid", by_other_role = "hh_uuid"),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      problems <- c(problems, paste0(nm, " (link to Household data): ", conditionMessage(res)))
    }
  }
  problems
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
  if (grepl("death|mortal", nm)) return("death")
  if (grepl("water|container", nm)) return("water")
  if (grepl("nutri|muac|child", nm)) return("nutrition")
  if (grepl("wom[ae]n|repro", nm)) return("woman")
  if (grepl("health", nm)) return("health")
  if (grepl("roster|individual|member|person", nm)) return("individual")
  if (grepl("household|hh|main", nm) || n_sheets == 1L) return("household")
  "skip"
}

# Table of a Data object at the given stage (raw = as imported).
iphra_dataset_frame <- function(obj, stage = "raw") {
  df <- obj$get_data(stage)
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
    tools_r <- iphra_session$get_module_reactive("imported_tools")
    tools_imported <- shiny::reactive(tools_r() %||% list())
    pending_tools <- shiny::reactiveVal(NULL)

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
        shiny::actionButton(
          ns("open_tool_import"), tx("Import Data Collection Tool(s)"),
          icon = shiny::icon("file-import"), class = "btn-primary",
          onclick = "iphraImportToolFiles(); return false;"
        ),
        if (length(tools_imported())) {
          shiny::span(class = "phr-toolbar__info",
                      paste0(length(tools_imported()), " ", tx("tool(s) imported:"), " ",
                             paste(names(tools_imported()), collapse = ", ")))
        },
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
        if (length(ds)) shiny::uiOutput(ns("stage_ui"), inline = TRUE),
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

    available_stages <- shiny::reactive({
      datasets()
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

    stage <- shiny::reactive({
      if (!is.null(input$stage) && input$stage %in% available_stages()) input$stage else "raw"
    })

    # Stage table with the object's UUID column moved to the far left.
    stage_frame <- shiny::reactive({
      obj <- current()
      shiny::req(obj)
      df <- iphra_dataset_frame(obj, stage())
      shiny::req(df)
      uuid <- tryCatch(obj$get(field = "..uuid"), error = function(e) NULL)
      if (is.character(uuid) && length(uuid) == 1L && uuid %in% names(df)) {
        df <- df[, c(uuid, setdiff(names(df), uuid)), drop = FALSE]
      } else uuid <- NULL
      attr(df, "uuid") <- uuid
      df
    })

    output$filters <- shiny::renderUI({
      if (!length(datasets())) return(NULL)
      cols <- names(stage_frame())
      shiny::div(
        style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:8px;",
        shiny::div(
          style = "min-width:240px; flex:1;",
          shiny::textInput(ns("col_search"), label = NULL, width = "100%",
                           value = shiny::isolate(input$col_search) %||% "",
                           placeholder = tx("Search columns..."))
        ),
        shiny::div(
          style = "min-width:200px;",
          shiny::selectInput(
            ns("filter_col"), label = NULL, width = "100%",
            choices = c(stats::setNames("", tx("Filter rows by column...")), cols),
            selected = shiny::isolate(input$filter_col)
          )
        ),
        shiny::div(
          style = "min-width:240px; flex:1;",
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

    output$dataset_info <- shiny::renderText({
      obj <- current()
      shiny::req(obj)
      df <- view_frame()
      paste0(class(obj)[1], " \u2022 ", format(nrow(df), big.mark = ","), " ", tx("rows"),
             " \u00d7 ", ncol(df), " ", tx("columns"),
             if (nrow(df) > max_rows) paste0(" \u2022 ", tx("showing first"), " ",
                                              format(max_rows, big.mark = ",")))
    })

    output$table <- rhandsontable::renderRHandsontable({
      df <- view_frame()
      shiny::req(ncol(df) > 0)
      fixed <- if (!is.null(attr(df, "uuid"))) 1L else 0L
      df <- iphra_display_frame(utils::head(df, max_rows))
      tbl <- rhandsontable::rhandsontable(
        df, readOnly = TRUE, height = 520, rowHeaders = NULL,
        highlightRow = TRUE, highlightCol = TRUE, stretchH = "none"
      ) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE,
                                format = "0.[########]", fixedColumnsLeft = fixed)
      # Sort arrow in each header (new Handsontable: indicator; old: sortIndicator)
      tbl$x$columnSorting <- list(indicator = TRUE, headerAction = TRUE, sortEmptyCells = TRUE)
      tbl$x$sortIndicator <- TRUE
      tbl
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
      for (t in "household") {
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
      if (sum(sel == "household") > 1L) {
        shiny::showNotification(
          tx("Only one sheet can be imported as Household data."),
          type = "error", duration = 10
        )
        return()
      }
      new_objs <- list()
      for (i in seq_along(p$sheets)) {
        type <- sel[i]
        if (type == "skip") next
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
        diag_res <- tryCatch({
          tryCatch(obj$.__enclos_env__$private$..map_schema_vars(stage = "raw"), error = function(e) NULL)
          obj$data_diagnose(stage = "raw")
        }, error = function(e) e)
        if (inherits(diag_res, "error")) {
          problems <- c(problems, paste0(nm, " (", tx("Diagnose"), "): ", conditionMessage(diag_res)))
        }
        existing[[nm]] <- obj
        new_objs[[nm]] <- list(type = type, obj = obj)
        added <- c(added, nm)
      }

      problems <- c(problems, iphra_link_datasets_to_household(new_objs, existing))

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

    # ---- data collection tool (XLSFORM) import ---------------------------------
    tool_defs <- iphra_tool_definitions()
    tool_type_choices <- c(
      stats::setNames("skip", tx("Do not import")),
      stats::setNames(vapply(tool_defs, `[[`, "", "name"),
                      vapply(tool_defs, function(t) tx(t$label), ""))
    )
    role_choices <- c(
      stats::setNames("skip", tx("Do not import")),
      stats::setNames(c("survey", "choices", "settings"),
                      c(tx("Survey sheet"), tx("Choices sheet"), tx("Settings sheet")))
    )

    guess_tool_type <- function(file) {
      nm <- tolower(tools::file_path_sans_ext(file))
      keys <- vapply(tool_defs, function(t) sub("_iphra_v[0-9]+$", "", t$name), "")
      hit <- which(vapply(keys, function(k) grepl(k, nm, fixed = TRUE), logical(1)))
      if (length(hit)) tool_defs[[hit[1]]]$name else "skip"
    }

    guess_role <- function(sheet) {
      nm <- tolower(trimws(sheet))
      if (nm %in% c("survey", "choices", "settings")) nm else "skip"
    }

    shiny::observeEvent(input$tool_files, {
      f <- input$tool_files
      files <- list()
      for (i in seq_len(nrow(f))) {
        parsed <- tryCatch(iphra_read_import_file(f$datapath[i], f$name[i]), error = function(e) e)
        if (inherits(parsed, "error") || !length(parsed)) {
          shiny::showNotification(
            paste(tx("Could not read the file:"), f$name[i],
                  if (inherits(parsed, "error")) conditionMessage(parsed)
                  else tx("No data sheets found.")),
            type = "error", duration = 10
          )
          next
        }
        files[[length(files) + 1L]] <- list(file = f$name[i], sheets = parsed)
      }
      if (!length(files)) return()
      pending_tools(files)
      go_to_import()
      shiny::showModal(tool_import_modal(files))
    })

    tool_import_modal <- function(files) {
      blocks <- lapply(seq_along(files), function(i) {
        fl <- files[[i]]
        sheet_rows <- lapply(seq_along(fl$sheets), function(j) {
          df <- fl$sheets[[j]]
          shiny::tags$tr(
            shiny::tags$td(
              shiny::strong(names(fl$sheets)[j]), shiny::tags$br(),
              shiny::span(class = "text-muted",
                          paste0(format(nrow(df), big.mark = ","), " ", tx("rows"), " \u00d7 ",
                                 ncol(df), " ", tx("columns")))
            ),
            shiny::tags$td(shiny::selectInput(
              ns(paste0("trole_", i, "_", j)), NULL, role_choices,
              selected = guess_role(names(fl$sheets)[j]), width = "100%"
            ))
          )
        })
        shiny::div(
          style = "margin-bottom:18px;",
          shiny::fluidRow(
            shiny::column(6, shiny::h5(shiny::strong(fl$file))),
            shiny::column(6, shiny::selectInput(
              ns(paste0("ttype_", i)), tx("Tool type"), tool_type_choices,
              selected = guess_tool_type(fl$file), width = "100%"
            ))
          ),
          shiny::tags$table(
            class = "table table-condensed phr-import-table",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th(tx("Sheet")), shiny::tags$th(tx("Sheet role"))
            )),
            shiny::tags$tbody(sheet_rows)
          )
        )
      })

      shiny::modalDialog(
        title = tx("Import Data Collection Tool(s)"),
        size = "l", easyClose = FALSE,
        shiny::p(class = "text-muted",
                 tx("For each file, choose the type of tool and the role of each sheet. Each tool needs one survey sheet and one choices sheet; the settings sheet is optional. Files set to 'Do not import' are ignored.")),
        blocks,
        footer = shiny::tagList(
          shiny::modalButton(tx("Cancel")),
          shiny::actionButton(ns("confirm_tool_import"), tx("Import"), class = "btn-primary")
        )
      )
    }

    shiny::observeEvent(input$confirm_tool_import, {
      files <- pending_tools()
      shiny::req(files)
      existing <- shiny::isolate(tools_imported())
      added <- character(0)
      problems <- character(0)
      last_tool <- NULL

      for (i in seq_along(files)) {
        fl <- files[[i]]
        type <- input[[paste0("ttype_", i)]] %||% "skip"
        if (identical(type, "skip")) next
        roles <- vapply(seq_along(fl$sheets),
                        function(j) input[[paste0("trole_", i, "_", j)]] %||% "skip", "")
        if (sum(roles == "survey") != 1L || sum(roles == "choices") != 1L || sum(roles == "settings") > 1L) {
          problems <- c(problems, paste0(
            fl$file, ": ", tx("assign exactly one survey sheet, one choices sheet and at most one settings sheet.")
          ))
          next
        }
        settings <- if ("settings" %in% roles) fl$sheets[[which(roles == "settings")]] else NULL
        tool_name <- tools::file_path_sans_ext(fl$file)
        tool <- tryCatch(
          phr::Tool$new(name = tool_name,
                        survey = fl$sheets[[which(roles == "survey")]],
                        choices = fl$sheets[[which(roles == "choices")]],
                        settings = settings),
          error = function(e) e
        )
        if (inherits(tool, "error")) {
          problems <- c(problems, paste0(fl$file, ": ", conditionMessage(tool)))
          next
        }
        nm <- tool_name
        k <- 1L
        while (nm %in% c(names(existing), added)) {
          k <- k + 1L
          nm <- paste0(tool_name, " (", k, ")")
        }
        existing[[nm]] <- list(tool_type = type, file = fl$file, tool = tool)
        added <- c(added, nm)
        last_tool <- tool
      }

      if (length(problems)) {
        shiny::showNotification(paste(problems, collapse = "\n"), type = "error", duration = 15)
      }
      if (length(added)) {
        iphra_session$set_module("imported_tools", existing)
        # Keep the latest tool available to modules that use a single tool.
        iphra_session$set_module("imported_tool", last_tool)
        shiny::showNotification(
          paste(tx("Imported:"), paste(added, collapse = ", ")), type = "message"
        )
        pending_tools(NULL)
        shiny::removeModal()
      } else if (!length(problems)) {
        shiny::showNotification(tx("No tools were selected for import."), type = "warning")
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
