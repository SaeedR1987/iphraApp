# -----------------------------------------------------------------------------
# Module: Tools Master (Tool Design action card)
# -----------------------------------------------------------------------------
#
# Card shown below the step banner on the Tool Design tab. It provides:
#   * a pickerInput of all allowable IPHRAProtocol tools: selecting a tool adds
#     it to the protocol, deselecting it removes it;
#   * Validate / Save / Export / Mark Complete buttons acting on the active
#     tool tab, and "All" variants acting on every selected tool.
#
# The IPHRAProtocol R6 object is not reactive on its own, so every mutation
# (`protocol$add_tools()` / `protocol$remove_tools()`) is followed by
# `phr_touch_module("protocol", session)`. The picker is kept in sync with the
# protocol (e.g. after a project is loaded) through the same signal.
# -----------------------------------------------------------------------------

# --- Allowable tools + human-readable labels ---------------------------------
#
# Kept in sync with `phr::IPHRAProtocol$new()$get_allowable_tools()`.
# `code` is a short prefix used for sheet / file names on export.
iphra_tool_definitions <- function() {
  list(
    list(name = "tool_household_iphra_v2",                     label = "Household",                       code = "household"),
    list(name = "tool_kii_community_iphra_v2",                 label = "Community KII",                   code = "kii_community"),
    list(name = "tool_obs_community_iphra_v2",                 label = "Community Observation",           code = "obs_community"),
    list(name = "tool_kii_fsl_service_provider_iphra_v2",      label = "FSL Service Provider KII",        code = "kii_fsl"),
    list(name = "tool_kii_markets_iphra_v2",                   label = "Markets KII",                     code = "kii_markets"),
    list(name = "tool_obs_crop_livestock_iphra_v1",            label = "Crop & Livestock Observation",    code = "obs_crop_livestock"),
    list(name = "tool_kii_health_service_provider_iphra_v2",   label = "Health Service Provider KII",     code = "kii_health"),
    list(name = "tool_obs_health_facility_iphra_v2",           label = "Health Facility Observation",     code = "obs_health"),
    list(name = "tool_kii_nutrition_service_provider_iphra_v2",label = "Nutrition Service Provider KII",  code = "kii_nutrition"),
    list(name = "tool_kii_wash_service_provider_iphra_v2",     label = "WASH Service Provider KII",       code = "kii_wash"),
    list(name = "tool_obs_water_point_iphra_v2",               label = "Water Point Observation",         code = "obs_water_point"),
    list(name = "tool_obs_latrine_iphra_v2",                   label = "Latrine Observation",             code = "obs_latrine")
  )
}

#' tools_master UI Function
#'
#' @param id Module id.
#' @noRd
mod_tools_master_ui <- function(id) {
  ns <- shiny::NS(id)

  defs <- iphra_tool_definitions()
  choices <- stats::setNames(
    vapply(defs, function(t) t$name, character(1)),
    vapply(defs, function(t) t$label, character(1))
  )

  btn <- function(id, label, icon, class = "btn-default") {
    shiny::actionButton(ns(id), label, icon = shiny::icon(icon),
                        class = paste(class, "btn-sm"), style = "width:100%; white-space:normal;")
  }

  shiny::fluidRow(
    shiny::column(
      12,
      shiny::div(
        class = "phr-toolbar",
        style = "display:block; background:#fff;",
        shiny::h4("Tool Actions", style = "margin-top:0;"),
        shiny::fluidRow(
          shiny::column(
            2,
            shinyWidgets::pickerInput(
              ns("tool_select"),
              label = "Tools",
              choices = choices,
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} tools selected",
                noneSelectedText = "No tools selected"
              )
            )
          ),
          shiny::column(
            10,
            shiny::div(
              style = "display:grid; grid-template-columns:repeat(6, minmax(0, 1fr)); gap:6px;",
              btn("validate_tool", "Validate Tool", "check"),
              btn("filter_tool", "Filter Tool", "filter"),
              btn("save_tool", "Save Tool", "save"),
              btn("export_tool", "Export Tool", "download", "btn-success"),
              btn("core_update", "Update Core Survey Questions", "edit", "btn-info"),
              btn("complete_tool", "Mark Complete", "check-circle"),
              btn("validate_all", "Validate All Tools", "check-double"),
              btn("filter_all", "Filter All Tools", "filter"),
              btn("save_all", "Save All Tools", "save"),
              btn("export_all", "Export All Tools", "download", "btn-success"),
              btn("ctx_update", "Contextualize Sectoral Questions", "globe", "btn-info"),
              btn("complete_all", "Mark All Complete", "check-circle")
            )
          )
        )
      )
    )
  )
}

#' tools_master Server Functions
#'
#' @param id Module id.
#' @param active_tab Reactive returning the `value` (tool role) of the active
#'   tab in the tool tabset.
#' @noRd
mod_tools_master_server <- function(id, active_tab = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    protocol_r <- phr_get_module_reactive("protocol", session)
    tools_complete <- iphra_tools_complete_rv(session)

    defs <- iphra_tool_definitions()
    label_of <- function(role) {
      d <- Filter(function(t) t$name == role, defs)
      if (length(d)) d[[1]]$label else role
    }
    code_of <- function(role) {
      d <- Filter(function(t) t$name == role, defs)
      if (length(d)) d[[1]]$code else role
    }

    notify <- function(msg, type = "message") shiny::showNotification(msg, type = type)

    current_roles <- shiny::reactive({
      p <- protocol_r()
      r <- tryCatch(as.character(p$get_tool_names()), error = function(e) character(0))
      intersect(vapply(defs, function(t) t$name, character(1)), r)
    })

    get_tool <- function(role) {
      p <- shiny::isolate(protocol_r())
      tryCatch(p$get(field = "tools", role = role), error = function(e) NULL) %||%
        tryCatch(p$tools[[role]], error = function(e) NULL)
    }

    # ---- Picker <-> protocol ---------------------------------------------------
    shiny::observe({
      shinyWidgets::updatePickerInput(session, "tool_select", selected = current_roles())
    })

    shiny::observeEvent(input$tool_select, {
      sel <- as.character(input$tool_select %||% character(0))
      cur <- shiny::isolate(current_roles())
      to_add <- setdiff(sel, cur)
      to_remove <- setdiff(cur, sel)
      if (!length(to_add) && !length(to_remove)) return()

      phrutils::phr_try({
        p <- protocol_r()
        for (r in to_add) p$add_tools(r)
        for (r in to_remove) p$remove_tools(r)
        tools_complete(setdiff(tools_complete(), to_remove))
        phr_touch_module("protocol", session)
      },
      on_error = "warn",
      origin = "Tools Master: Update Tools",
      hint = "Verify the tool name matches protocol$get_allowable_tools().")
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ---- Helpers for the action buttons ---------------------------------------
    active_role <- function() {
      r <- active_tab()
      if (is.null(r) || !r %in% shiny::isolate(current_roles())) {
        notify("Select a tool tab first.", "warning")
        return(NULL)
      }
      r
    }

    all_roles <- function() {
      r <- shiny::isolate(current_roles())
      if (!length(r)) notify("No tools have been selected.", "warning")
      r
    }

    iphra_tools_editors_server(
      input, output, session,
      get_tool = get_tool,
      active_role = active_role,
      label_of = label_of,
      touch = function() phr_touch_module("protocol", session)
    )

    validate_role <- function(role) {
      tool <- get_tool(role)
      ok <- if (is.null(tool)) NA else tryCatch(isTRUE(tool$validate()), error = function(e) FALSE)
      errs <- if (is.null(tool)) list() else tryCatch(tool$get_validation_errors(), error = function(e) list())
      df <- data.frame(
        Row = vapply(errs, function(e) as.character(e$row %||% NA), character(1)),
        Message = vapply(errs, function(e) as.character(e$message %||% ""), character(1)),
        stringsAsFactors = FALSE
      )
      list(role = role, ok = ok, errors = df)
    }

    show_validation <- function(results) {
      max_rows <- 50L
      body <- lapply(results, function(r) {
        status <- if (isTRUE(r$ok)) {
          shiny::span(style = "color:#2e7d32;", shiny::icon("check-circle"), " Valid")
        } else {
          shiny::span(style = "color:#c62828;", shiny::icon("times-circle"), " Issues found")
        }
        tbl <- NULL
        if (nrow(r$errors) > 0L) {
          shown <- utils::head(r$errors, max_rows)
          tbl <- shiny::tagList(
            shiny::tags$table(
              class = "table table-condensed table-striped", style = "font-size:12px;",
              shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Row"), shiny::tags$th("Message"))),
              shiny::tags$tbody(lapply(seq_len(nrow(shown)), function(i) {
                shiny::tags$tr(shiny::tags$td(shown$Row[i]), shiny::tags$td(shown$Message[i]))
              }))
            ),
            if (nrow(r$errors) > max_rows) {
              shiny::p(class = "text-muted", sprintf("Showing %d of %d issues.", max_rows, nrow(r$errors)))
            }
          )
        }
        shiny::tagList(shiny::h5(shiny::strong(label_of(r$role)), " - ", status), tbl)
      })
      shiny::showModal(shiny::modalDialog(
        title = "Tool Validation", body, easyClose = TRUE, size = "l",
        footer = shiny::modalButton("Close")
      ))
    }

    run_validation <- function(roles) {
      phrutils::phr_try({
        results <- lapply(roles, validate_role)
        if (all(vapply(results, function(r) isTRUE(r$ok), logical(1)))) {
          notify(if (length(roles) == 1L) paste0(label_of(roles), ": valid.") else "All selected tools are valid.")
        } else {
          show_validation(results)
        }
      },
      on_error = "warn",
      origin = "Tools Master: Validate",
      hint = "Check that the tool exposes validate() and get_validation_errors().")
    }

    save_roles <- function(roles) {
      phrutils::phr_try({
        phr_touch_module("protocol", session)
        notify(paste0(
          if (length(roles) == 1L) label_of(roles) else paste(length(roles), "tools"),
          " saved to the project. Use File > Save Project to write the project to disk."
        ))
      },
      on_error = "warn",
      origin = "Tools Master: Save",
      hint = "Verify the protocol object is available.")
    }

    tool_sheets <- function(role) {
      tool <- get_tool(role)
      safe_df <- function(x) {
        tryCatch({
          if (is.null(x)) return(data.frame())
          if (is.data.frame(x)) x else as.data.frame(x)
        }, error = function(e) data.frame())
      }
      list(
        survey = safe_df(tool$get(field = "revised_survey")),
        choices = safe_df(tool$get(field = "revised_choices")),
        settings = safe_df(tool$get(field = "revised_settings"))
      )
    }

    stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

    # ---- Validate ---------------------------------------------------------------
    shiny::observeEvent(input$validate_tool, {
      r <- active_role()
      if (!is.null(r)) run_validation(r)
    })
    shiny::observeEvent(input$validate_all, {
      r <- all_roles()
      if (length(r)) run_validation(r)
    })

    # ---- Filter -----------------------------------------------------------------
    # Re-applies Tool$filter_survey_by_indicator() with the tool's selected
    # indicator codes to rebuild its revised survey/choices.
    filter_roles <- function(roles) {
      phrutils::phr_try({
        done <- character(0)
        for (r in roles) {
          tool <- get_tool(r)
          codes <- as.character(tool$get(field = "selected_indicator_codes") %||% character(0))
          if (!length(codes)) next
          tool$call(field = "filter_survey_by_indicator", indicator_codes = codes)
          done <- c(done, r)
        }
        phr_touch_module("protocol", session)
        if (length(done)) {
          notify(paste0("Filtered by selected indicators: ", paste(vapply(done, label_of, character(1)), collapse = ", "), "."))
        } else {
          notify("No indicators are selected for the chosen tool(s).", "warning")
        }
      },
      on_error = "warn",
      origin = "Tools Master: Filter",
      hint = "Select indicators in the tool tab before filtering.")
    }

    shiny::observeEvent(input$filter_tool, {
      r <- active_role()
      if (!is.null(r)) filter_roles(r)
    })
    shiny::observeEvent(input$filter_all, {
      r <- all_roles()
      if (length(r)) filter_roles(r)
    })

    # ---- Save -------------------------------------------------------------------
    shiny::observeEvent(input$save_tool, {
      r <- active_role()
      if (!is.null(r)) save_roles(r)
    })
    shiny::observeEvent(input$save_all, {
      r <- all_roles()
      if (length(r)) save_roles(r)
    })

    # ---- Export -----------------------------------------------------------------
    # Buttons open a modal that holds the actual downloadButton, because the
    # browser's save dialog is only reachable through a downloadHandler.
    export_role <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$export_tool, {
      r <- active_role()
      if (is.null(r)) return()
      export_role(r)
      shiny::showModal(shiny::modalDialog(
        title = paste("Export", label_of(r), "Tool"),
        shiny::p("Save the tool as an Excel workbook with three sheets: survey, choices, and settings."),
        shiny::downloadButton(ns("download_tool"), "Download Excel", class = "btn-success"),
        footer = shiny::modalButton("Cancel"), easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$export_all, {
      r <- all_roles()
      if (!length(r)) return()
      shiny::showModal(shiny::modalDialog(
        title = "Export All Tools",
        shiny::p(sprintf("Save the %d selected tool(s) as a zip file with one Excel workbook per tool.", length(r))),
        shiny::downloadButton(ns("download_all"), "Download Zip", class = "btn-success"),
        footer = shiny::modalButton("Cancel"), easyClose = TRUE
      ))
    })

    output$download_tool <- shiny::downloadHandler(
      filename = function() paste0(export_role() %||% "tool", "_", stamp(), ".xlsx"),
      content = function(file) {
        writexl::write_xlsx(tool_sheets(export_role()), path = file)
        phrutils::phr_message(
          paste0(label_of(export_role()), " tool exported to Excel."),
          origin = "Tools Master: Export"
        )
      }
    )

    output$download_all <- shiny::downloadHandler(
      filename = function() paste0("iphra_tools_", stamp(), ".zip"),
      content = function(file) {
        tmp <- tempfile("iphra_tools_")
        dir.create(tmp)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        files <- vapply(shiny::isolate(current_roles()), function(r) {
          f <- paste0(code_of(r), ".xlsx")
          writexl::write_xlsx(tool_sheets(r), path = file.path(tmp, f))
          f
        }, character(1))
        zip::zip(zipfile = file, files = files, root = tmp, mode = "cherry-pick")
        phrutils::phr_message("All selected tools exported.", origin = "Tools Master: Export")
      }
    )

    # ---- Mark complete ----------------------------------------------------------
    shiny::observeEvent(input$complete_tool, {
      r <- active_role()
      if (is.null(r)) return()
      done <- tools_complete()
      if (r %in% done) {
        tools_complete(setdiff(done, r))
        notify(paste0(label_of(r), ": marked incomplete."))
      } else {
        tools_complete(union(done, r))
        notify(paste0(label_of(r), ": marked complete."))
      }
    })

    shiny::observeEvent(input$complete_all, {
      r <- all_roles()
      if (!length(r)) return()
      tools_complete(union(tools_complete(), r))
      notify("All selected tools marked complete.")
    })
  })
}

## To be copied in the UI
# mod_tools_master_ui("tools_master_1")

## To be copied in the server
# mod_tools_master_server("tools_master_1")
