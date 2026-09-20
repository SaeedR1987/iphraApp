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
