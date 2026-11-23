#' Data monitoring and quality module
#'
#' Two high-level tabs:
#' * Monitoring: a leaflet map of household GPS points plus Overall,
#'   Enumerator and Cluster/Site summaries of the household dataset.
#' * Quality Checks: browse the `phr::DataAnalytics` objects (generated in the
#'   Data Standardization step), their visualizations and plausibility tables.
#'
#' Analytics objects are stored in the `"analytics"` session module as a named
#' list keyed `"<dataset> / <type>"`.
#'
#' @param id Module id.
#' @param lang_r Reactive returning the active UI language.
#' @noRd
mod_data_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  summary_tab <- function(key, title) {
    shiny::tabPanel(
      title, value = key,
      shiny::div(
        style = "margin-top:10px;",
        shiny::uiOutput(ns(paste0(key, "_select"))),
        shiny::uiOutput(ns(paste0(key, "_view")))
      )
    )
  }
  shiny::tabsetPanel(
    id = ns("main_tabs"),
    shiny::tabPanel(
      shiny::textOutput(ns("lbl_monitoring"), inline = TRUE), value = "monitoring",
      shiny::div(
        style = "margin-top:12px;",
        shiny::fluidRow(
          shiny::column(
            5,
            shiny::uiOutput(ns("map_note")),
            leaflet::leafletOutput(ns("map"), height = 520)
          ),
          shiny::column(
            7,
            shiny::uiOutput(ns("monitor_empty")),
            shiny::tabsetPanel(
              id = ns("summary_tabs"),
              summary_tab("overall", "Overall"),
              summary_tab("enumerator", "Enumerator"),
              summary_tab("cluster", "Cluster/Site")
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      shiny::textOutput(ns("lbl_quality"), inline = TRUE), value = "quality_checks",
      shiny::div(
        style = "margin-top:12px;",
        shiny::uiOutput(ns("qc_toolbar")),
        shiny::uiOutput(ns("qc_empty")),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns("viz_select")),
            shiny::plotOutput(ns("viz_plot"), height = 480)
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("tbl_select")),
            shiny::uiOutput(ns("tbl_view"))
          )
        )
      )
    )
  )
}

# Recursively list the leaf items (non-plain-list objects) of a nested list
iphra_flatten_items <- function(x, path = character()) {
  if (is.null(x)) return(list())
  is_container <- is.list(x) && !is.data.frame(x) && is.null(attr(x, "class"))
  if (!is_container) return(stats::setNames(list(x), paste(path, collapse = "/")))
  out <- list()
  nms <- names(x) %||% as.character(seq_along(x))
  for (i in seq_along(x)) out <- c(out, iphra_flatten_items(x[[i]], c(path, nms[i])))
  out
}

iphra_item_choices <- function(items) {
  keys <- names(items)
  if (length(items) == 0L || is.null(keys)) return(character(0))
  labels <- vapply(keys, function(k) {
    gsub("/", " \u203a ", tools::toTitleCase(gsub("_", " ", k)), fixed = TRUE)
  }, character(1))
  stats::setNames(keys, labels)
}

# Render a data frame or flextable as scrollable read-only HTML
iphra_table_html <- function(x) {
  if (is.data.frame(x)) x <- flextable::flextable(utils::head(iphra_display_frame(as.data.frame(x)), 2000L))
  if (inherits(x, "flextable")) {
    shiny::div(style = "overflow:auto; max-height:520px;", flextable::htmltools_value(x))
  } else {
    shiny::div(class = "text-muted", "This item cannot be displayed as a table.")
  }
}

iphra_count_by <- function(df, col, label) {
  tab <- as.data.frame(table(df[[col]], useNA = "no"), stringsAsFactors = FALSE)
  names(tab) <- c(label, "Interviews")
  tab[order(-tab$Interviews), , drop = FALSE]
}

iphra_bar_plot <- function(tab, title) {
  tab <- utils::head(tab, 40L)
  names(tab)[1] <- "Label"
  tab$Label <- factor(tab$Label, levels = rev(tab$Label))
  ggplot2::ggplot(tab, ggplot2::aes(x = Label, y = Interviews)) +
    ggplot2::geom_col(fill = "#EE5859") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = "Interviews") +
    ggplot2::theme_minimal(base_size = 14)
}

mod_data_quality_server <- function(id, lang_r = function() "en") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iphra_session <- get_session(session)
    tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))

    output$lbl_monitoring <- shiny::renderText(tx("Monitoring"))
    output$lbl_quality <- shiny::renderText(tx("Quality Checks"))
    shiny::outputOptions(output, "lbl_monitoring", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "lbl_quality", suspendWhenHidden = FALSE)

    datasets_r <- iphra_session$get_module_reactive("datasets")
    datasets <- shiny::reactive(datasets_r() %||% list())
    analytics_r <- iphra_session$get_module_reactive("analytics")
    analytics <- shiny::reactive(analytics_r() %||% list())
    refresh <- shiny::reactiveVal(0L)

    # ---- Monitoring -------------------------------------------------------
    hh_obj <- shiny::reactive({
      refresh()
      ds <- datasets()
      hit <- Filter(function(o) inherits(o, "HouseholdData"), ds)
      if (length(hit)) hit[[1]] else NULL
    })

    hh_info <- shiny::reactive({
      obj <- hh_obj()
      if (is.null(obj)) return(NULL)
      df <- tryCatch(obj$get_data("standardized"), error = function(e) NULL)
      if (is.null(df)) df <- tryCatch(obj$get_data("raw"), error = function(e) NULL)
      if (!is.data.frame(df)) return(NULL)
      vm <- tryCatch(obj$get(field = "..variable_map"), error = function(e) list())
      col <- function(role) {
        v <- vm[[role]]
        v <- if (is.null(v)) NA_character_ else as.character(unlist(v))[1]
        if (!is.na(v) && v %in% names(df)) v else NA_character_
      }
      list(
        df = as.data.frame(df),
        lat = col("gps_lat"), lon = col("gps_lon"), enum = col("enum_id"),
        cluster = col("cluster_id"), stratum = col("stratum"), date = col("date_survey")
      )
    })

    output$monitor_empty <- shiny::renderUI({
      if (!is.null(hh_info())) return(NULL)
      phr_info_panel(
        tx("Import a Household dataset in the Data Import step to see monitoring summaries."),
        title = tx("No household data available")
      )
    })

    output$map_note <- shiny::renderUI({
      info <- hh_info()
      if (is.null(info) || (!is.na(info$lat) && !is.na(info$lon))) return(NULL)
      shiny::div(class = "text-muted", style = "margin-bottom:6px;",
                 tx("No household GPS variables (gps_lat, gps_lon) are mapped in the variable map."))
    })

    output$map <- leaflet::renderLeaflet({
      info <- hh_info()
      m <- leaflet::addTiles(leaflet::leaflet())
      if (is.null(info) || is.na(info$lat) || is.na(info$lon)) {
        return(leaflet::setView(m, lng = 0, lat = 10, zoom = 2))
      }
      df <- info$df
      lat <- suppressWarnings(as.numeric(df[[info$lat]]))
      lon <- suppressWarnings(as.numeric(df[[info$lon]]))
      ok <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180 & !(lat == 0 & lon == 0)
      if (!any(ok)) return(leaflet::setView(m, lng = 0, lat = 10, zoom = 2))
      popup <- vapply(which(ok), function(i) {
        parts <- c(
          if (!is.na(info$enum)) paste0(tx("Enumerator"), ": ", df[[info$enum]][i]),
          if (!is.na(info$cluster)) paste0(tx("Cluster"), ": ", df[[info$cluster]][i]),
          if (!is.na(info$stratum)) paste0(tx("Stratum"), ": ", df[[info$stratum]][i])
        )
        paste(htmltools::htmlEscape(parts), collapse = "<br>")
      }, character(1))
      leaflet::addCircleMarkers(
        m, lng = lon[ok], lat = lat[ok], radius = 4, stroke = FALSE,
        fillColor = "#EE5859", fillOpacity = 0.7, popup = popup,
        clusterOptions = leaflet::markerClusterOptions()
      )
    })

    summary_defs <- function(scope, info) {
      df <- info$df
      cnt <- function(col, label) {
        if (is.na(col)) return(NULL)
        iphra_count_by(df, col, label)
      }
      switch(scope,
        overall = {
          dates <- if (!is.na(info$date)) suppressWarnings(as.Date(df[[info$date]])) else NULL
          list(
            summary = list(label = tx("Summary table"), type = "table", build = function() {
              data.frame(
                Indicator = c(tx("Interviews"), tx("Enumerators"), tx("Clusters"), tx("Strata"),
                              tx("First interview date"), tx("Last interview date")),
                Value = c(
                  nrow(df),
                  if (!is.na(info$enum)) length(unique(stats::na.omit(df[[info$enum]]))) else NA,
                  if (!is.na(info$cluster)) length(unique(stats::na.omit(df[[info$cluster]]))) else NA,
                  if (!is.na(info$stratum)) length(unique(stats::na.omit(df[[info$stratum]]))) else NA,
                  if (!is.null(dates) && any(!is.na(dates))) format(min(dates, na.rm = TRUE)) else NA,
                  if (!is.null(dates) && any(!is.na(dates))) format(max(dates, na.rm = TRUE)) else NA
                ),
                stringsAsFactors = FALSE
              )
            }),
            by_date = list(label = tx("Interviews by date"), type = "plot", build = function() {
              shiny::req(!is.null(dates), any(!is.na(dates)))
              tab <- as.data.frame(table(dates), stringsAsFactors = FALSE)
              names(tab) <- c("Date", "Interviews")
              tab$Date <- as.Date(tab$Date)
              ggplot2::ggplot(tab, ggplot2::aes(Date, Interviews)) +
                ggplot2::geom_col(fill = "#EE5859") +
                ggplot2::labs(title = tx("Interviews by date"), x = NULL) +
                ggplot2::theme_minimal(base_size = 14)
            }),
            by_stratum = list(label = tx("Interviews by stratum"), type = "table",
                              build = function() cnt(info$stratum, tx("Stratum")))
          )
        },
        enumerator = list(
          table = list(label = tx("Interviews per enumerator (table)"), type = "table",
                       build = function() cnt(info$enum, tx("Enumerator"))),
          plot = list(label = tx("Interviews per enumerator (chart)"), type = "plot",
                      build = function() {
                        tab <- cnt(info$enum, tx("Enumerator")); shiny::req(tab)
                        iphra_bar_plot(tab, tx("Interviews per enumerator"))
                      })
        ),
        cluster = list(
          table = list(label = tx("Interviews per cluster/site (table)"), type = "table",
                       build = function() cnt(info$cluster, tx("Cluster"))),
          plot = list(label = tx("Interviews per cluster/site (chart)"), type = "plot",
                      build = function() {
                        tab <- cnt(info$cluster, tx("Cluster")); shiny::req(tab)
                        iphra_bar_plot(tab, tx("Interviews per cluster/site"))
                      })
        )
      )
    }

    for (scope in c("overall", "enumerator", "cluster")) {
      local({
        scope <- scope
        sel_id <- paste0(scope, "_select")
        view_id <- paste0(scope, "_view")
        plot_id <- paste0(scope, "_plot")
        defs <- shiny::reactive({
          info <- hh_info(); shiny::req(info)
          summary_defs(scope, info)
        })
        current_def <- shiny::reactive({
          d <- defs(); k <- input[[paste0(scope, "_choice")]]
          if (is.null(k) || is.null(d[[k]])) NULL else d[[k]]
        })

        output[[sel_id]] <- shiny::renderUI({
          d <- defs()
          shiny::selectInput(
            ns(paste0(scope, "_choice")), NULL,
            choices = stats::setNames(names(d), vapply(d, function(z) z$label, character(1))),
            selected = shiny::isolate(input[[paste0(scope, "_choice")]]), width = "100%"
          )
        })
        output[[view_id]] <- shiny::renderUI({
          d <- current_def(); shiny::req(d)
          if (identical(d$type, "plot")) {
            shiny::plotOutput(ns(plot_id), height = 440)
          } else {
            tab <- d$build()
            if (is.null(tab)) {
              shiny::div(class = "text-muted", tx("The variables needed for this summary are not mapped."))
            } else iphra_table_html(tab)
          }
        })
        output[[plot_id]] <- shiny::renderPlot({
          d <- current_def(); shiny::req(d, identical(d$type, "plot"))
          d$build()
        })
      })
    }

    # ---- Quality checks -----------------------------------------------------
    output$qc_toolbar <- shiny::renderUI({
      an <- analytics()
      phr_toolbar(
        shiny::div(
          style = "display:inline-block; min-width:360px;",
          shiny::selectInput(
            ns("analytics_obj"), label = NULL,
            choices = if (length(an)) names(an) else stats::setNames("", tx("No analytics objects yet")),
            selected = shiny::isolate(input$analytics_obj), width = "100%"
          )
        )
      )
    })

    output$qc_empty <- shiny::renderUI({
      if (length(datasets())) return(NULL)
      phr_info_panel(
        tx("Import and standardize a dataset first, then use Generate Analytics in the Data Standardization step."),
        title = tx("No datasets available")
      )
    })

    current_an <- shiny::reactive({
      refresh()
      an <- analytics()
      nm <- input$analytics_obj
      if (is.null(nm) || !nzchar(nm) || is.null(an[[nm]])) NULL else an[[nm]]
    })

    viz_items <- shiny::reactive({
      an <- current_an(); shiny::req(an)
      Filter(function(x) inherits(x, "ggplot"), iphra_flatten_items(an$visualizations))
    })
    tbl_items <- shiny::reactive({
      an <- current_an(); shiny::req(an)
      items <- iphra_flatten_items(an$tables)
      plaus <- items[grepl("plausibility", names(items), fixed = TRUE)]
      if (length(plaus)) plaus else items
    })

    output$viz_select <- shiny::renderUI({
      items <- viz_items()
      if (length(items) == 0L) {
        return(shiny::div(class = "text-muted", tx("No visualizations available.")))
      }
      shiny::selectInput(ns("viz_choice"), tx("Visualization"),
                         choices = iphra_item_choices(items), width = "100%")
    })
    output$viz_plot <- shiny::renderPlot({
      items <- viz_items()
      k <- input$viz_choice
      shiny::req(k, items[[k]])
      items[[k]]
    })

    output$tbl_select <- shiny::renderUI({
      items <- tbl_items()
      if (length(items) == 0L) {
        return(shiny::div(class = "text-muted", tx("No tables available.")))
      }
      shiny::selectInput(ns("tbl_choice"), tx("Plausibility table"),
                         choices = iphra_item_choices(items), width = "100%")
    })
    output$tbl_view <- shiny::renderUI({
      items <- tbl_items()
      k <- input$tbl_choice
      shiny::req(k, items[[k]])
      iphra_table_html(items[[k]])
    })
  })
}
