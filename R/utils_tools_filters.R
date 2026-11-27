# ────────────────────────────────────────────────────────────────────────────────
# Shared helpers for the mod_tools_* modules
# ────────────────────────────────────────────────────────────────────────────────
#
# Every `mod_tools_*` module (Household, Community KII, Community Observation,
# FSL Provider KII, Markets KII, Crop & Livestock Observation, Health Facility
# KII, Health Facility Observation, Nutrition Provider KII, WASH Provider KII,
# Water Point Observation, Latrine Observation) exposes the same
# Sector / Pillar / Sub-Pillar cascading filters plus an "available" /
# "selected" drag-and-drop indicator picker.
#
# The current filter values and the selected indicator codes are persisted on
# the corresponding `phr` Tool R6 object (`selected_sectors`,
# `selected_pillars`, `selected_subpillars`, `selected_indicator_codes`,
# `available_indicator_codes` public fields) so that they round-trip through
# `save_project()` / `load_project()` (the Tool object itself is serialized
# via `saveRDS()`/`readRDS()` as part of the `.iphra` project file).
#
# These helpers centralize the restore-on-load and save-on-change logic so it
# does not need to be duplicated (and drift) across every `mod_tools_*` file.
# ────────────────────────────────────────────────────────────────────────────────

#' @title Restore a Tool's Sector / Pillar / Sub-Pillar Filters
#' @description
#' Pushes the `selected_sectors`, `selected_pillars` and `selected_subpillars`
#' fields stored on a `phr` Tool object back into the module's
#' `sector_filter` / `pillar_filter` / `subpillar_filter` `selectInput()`s.
#'
#' Because the Pillar and Sub-Pillar choices normally cascade from the
#' currently selected Sector(s) / Pillar(s) via `renderUI()`, and Shiny's
#' `update*Input()` calls only round-trip to the server on the *next*
#' reactive flush, restoring is done by computing the pillar/sub-pillar
#' `choices` directly from `objective_filters` (independent of the current
#' `input$sector_filter` / `input$pillar_filter` values) and pushing both
#' `choices` and `selected` in the same `updateSelectInput()` call. This
#' avoids relying on a client round-trip between each cascade step.
#'
#' @param session The module's `session` (so the `update*Input()` calls are
#'   automatically namespaced).
#' @param tool The `phr` Tool R6 object stored on the protocol. If `NULL`,
#'   this is a no-op.
#' @param objective_filters A data frame with (at least) `sector`, `pillar`
#'   and `sub_pillar` columns, unfiltered by the current inputs.
#'
#' @noRd
iphra_restore_tool_filters <- function(session, tool, objective_filters) {

  if (is.null(tool) || is.null(objective_filters)) {
    return(invisible(NULL))
  }

  sectors    <- as.character(tool$selected_sectors %||% character(0))
  pillars    <- as.character(tool$selected_pillars %||% character(0))
  subpillars <- as.character(tool$selected_subpillars %||% character(0))

  if (length(sectors) > 0) {
    shiny::updateSelectInput(session, "sector_filter", selected = sectors)
  }

  if (length(pillars) > 0) {
    pillar_choices <- sort(unique(
      objective_filters$pillar[objective_filters$sector %in% sectors]
    ))
    shiny::updateSelectInput(
      session, "pillar_filter",
      choices  = pillar_choices,
      selected = pillars
    )
  }

  if (length(subpillars) > 0) {
    subpillar_choices <- sort(unique(
      objective_filters$sub_pillar[
        objective_filters$sector %in% sectors &
          objective_filters$pillar %in% pillars
      ]
    ))
    shiny::updateSelectInput(
      session, "subpillar_filter",
      choices  = subpillar_choices,
      selected = subpillars
    )
  }

  invisible(NULL)
}

#' @title Save the Current Sector / Pillar / Sub-Pillar Filter onto a Tool
#' @description
#' Small helper used by each module's `observeEvent(input$sector_filter, ...)`
#' (and equivalents for pillar / sub-pillar) to write the current filter
#' selection onto the corresponding public field of a `phr` Tool object.
#' Guards against a `NULL` tool (e.g. before the tool has been added to the
#' protocol).
#'
#' @param tool The `phr` Tool R6 object stored on the protocol, or `NULL`.
#' @param field The public field name to update (e.g. `"selected_sectors"`).
#' @param value The new value (coerced to `character()`).
#'
#' @noRd
iphra_save_tool_field <- function(tool, field, value) {
  if (is.null(tool)) return(invisible(NULL))

  tool$set(field = field, value = as.character(value %||% character(0)))

  invisible(NULL)
}

#' @title Sector / Pillar / Sub-Pillar picker filters for a tool module
#' @description
#' Registers the `sector_filter_ui`, `pillar_filter_ui` and
#' `subpillar_filter_ui` outputs (`shinyWidgets::pickerInput()`s, all values
#' selected by default) and the cascade between them: Pillar choices follow
#' the selected Sector(s), Sub-Pillar choices follow the selected Pillar(s).
#' Input ids stay `sector_filter`, `pillar_filter` and `subpillar_filter`.
#'
#' @param protocol_r Reactive returning the protocol.
#' @param role Role of the tool in `protocol$tools`.
#' @param all_indicators_fn Function returning the tool's indicator data
#'   frame (with an `objective_code` column).
#'
#' @return A list with `available` (reactive: indicators matching the filters)
#'   and `restore` (function restoring the saved filters from the Tool).
#'
#' @noRd
iphra_tool_filters_server <- function(input, output, session, protocol_r,
                                      role, all_indicators_fn) {

  cols <- c("sector", "pillar", "sub_pillar")
  ids <- c(sector = "sector_filter", pillar = "pillar_filter",
           sub_pillar = "subpillar_filter")
  labels <- c(sector = "Sector", pillar = "Pillar", sub_pillar = "Sub-Pillar")
  parents <- list(sector = character(0), pillar = "sector",
                  sub_pillar = c("sector", "pillar"))

  seen <- new.env()
  prev <- new.env()
  restore_rv <- shiny::reactiveVal(NULL)
  filters_rv <- shiny::reactiveVal(NULL)

  shiny::observe({
    proto <- protocol_r()
    schema <- proto$framework$master_objectives_schema
    shiny::req(!is.null(schema))
    filters_rv(schema[, c("objective_code", cols)])
  })

  # Current selection of a filter. NULL = picker not rendered yet (no filter);
  # character(0) = the user emptied it.
  sel_of <- function(col) {
    r <- restore_rv()
    if (!is.null(r)) return(r[[col]])
    v <- input[[ids[[col]]]]
    if (is.null(v)) {
      if (isTRUE(seen[[col]])) return(character(0))
      return(NULL)
    }
    seen[[col]] <- TRUE
    v
  }

  choices_of <- function(col) {
    ref <- filters_rv()
    keep <- rep(TRUE, nrow(ref))
    for (p in parents[[col]]) {
      s <- sel_of(p)
      if (!is.null(s)) keep <- keep & (as.character(ref[[p]]) %in% s)
    }
    out <- sort(unique(as.character(ref[[col]][keep])))
    out[!is.na(out)]
  }

  render_picker <- function(col) {
    shiny::renderUI({
      shiny::req(filters_rv())
      choices <- shiny::isolate(choices_of(col))
      shinyWidgets::pickerInput(
        session$ns(ids[[col]]),
        label = labels[[col]],
        choices = choices,
        selected = shiny::isolate(input[[ids[[col]]]]) %||% choices,
        multiple = TRUE,
        width = "100%",
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectedTextFormat = "count > 2",
          countSelectedText = "{0} selected",
          liveSearch = TRUE
        )
      )
    })
  }

  output$sector_filter_ui <- render_picker("sector")
  output$pillar_filter_ui <- render_picker("pillar")
  output$subpillar_filter_ui <- render_picker("sub_pillar")

  cascade <- function(col) {
    shiny::observe({
      shiny::req(filters_rv())
      r <- restore_rv()
      for (p in parents[[col]]) {
        if (is.null(r)) shiny::req(!is.null(sel_of(p)))
      }
      choices <- choices_of(col)
      cur <- if (is.null(r)) sel_of(col) else NULL
      old <- prev[[col]] %||% choices
      new_sel <- if (!is.null(r)) {
        intersect(r[[col]], choices)
      } else if (is.null(cur)) {
        choices
      } else {
        union(intersect(cur, choices), setdiff(choices, old))
      }
      prev[[col]] <- choices
      shinyWidgets::updatePickerInput(
        session, ids[[col]], choices = choices, selected = new_sel
      )
    })
  }
  cascade("pillar")
  cascade("sub_pillar")

  # Drop the restore override once the inputs reflect it
  shiny::observe({
    r <- restore_rv()
    shiny::req(r)
    same <- vapply(cols, function(col) {
      setequal(input[[ids[[col]]]] %||% character(0), r[[col]])
    }, logical(1))
    if (all(same)) restore_rv(NULL)
  })

  restore <- function() {
    proto <- shiny::isolate(protocol_r())
    tool <- tryCatch(proto$tools[[role]], error = function(e) NULL) %||%
      tryCatch(proto$get(field = "tools", role = role), error = function(e) NULL)
    if (is.null(tool) || is.null(shiny::isolate(filters_rv()))) return(invisible(NULL))
    r <- list(
      sector = as.character(tool$selected_sectors %||% character(0)),
      pillar = as.character(tool$selected_pillars %||% character(0)),
      sub_pillar = as.character(tool$selected_subpillars %||% character(0))
    )
    if (sum(lengths(r)) == 0) return(invisible(NULL))
    restore_rv(r)
    shinyWidgets::updatePickerInput(
      session, ids[["sector"]],
      choices = shiny::isolate(choices_of("sector")),
      selected = r$sector
    )
    invisible(NULL)
  }

  available <- shiny::reactive({
    ref <- filters_rv()
    shiny::req(ref)
    keep <- rep(TRUE, nrow(ref))
    for (col in cols) {
      s <- sel_of(col)
      if (!is.null(s)) keep <- keep & (as.character(ref[[col]]) %in% s)
    }
    codes <- unique(ref$objective_code[keep])
    inds <- all_indicators_fn()
    inds[inds$objective_code %in% codes, , drop = FALSE]
  })

  list(available = available, restore = restore)
}
