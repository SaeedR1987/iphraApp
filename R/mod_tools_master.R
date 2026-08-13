# ────────────────────────────────────────────────────────────────────────────────
# Module: Tools Master
# ────────────────────────────────────────────────────────────────────────────────
#
# Provides Add / Remove buttons for every allowable IPHRAProtocol tool.
# Each tool has a pair of buttons stacked vertically (Add on top, Remove
# below); the tools themselves are laid out horizontally in a scrollable
# row. The buttons drive the IPHRAProtocol object stored in
# `session$userData$modules$protocol` via helpers in `utils_session.R`.
#
# Downstream mod_tools_* modules watch the reactive tools vector
# (`session$userData$protocol_tools`) to dynamically show / hide their
# sections.
#
# ────────────────────────────────────────────────────────────────────────────────

# --- Allowable tools + human-readable labels ---------------------------------
#
# Kept in sync with `phr::IPHRAProtocol$new()$get_allowable_tools()`.
iphra_tool_definitions <- function() {
  list(
    list(name = "tool_household_iphra_v2",                     label = "Household"),
    list(name = "tool_kii_community_iphra_v2",                 label = "Community KII"),
    list(name = "tool_kii_fsl_service_provider_iphra_v2",      label = "FSL Service Provider KII"),
    list(name = "tool_kii_wash_service_provider_iphra_v2",     label = "WASH Service Provider KII"),
    list(name = "tool_kii_markets_iphra_v2",                   label = "Markets KII"),
    list(name = "tool_kii_nutrition_service_provider_iphra_v2",label = "Nutrition Service Provider KII"),
    list(name = "tool_kii_health_service_provider_iphra_v2",   label = "Health Service Provider KII"),
    list(name = "tool_obs_community_iphra_v2",                 label = "Community Observation"),
    list(name = "tool_obs_crop_livestock_iphra_v1",            label = "Crop & Livestock Observation"),
    list(name = "tool_obs_health_facility_iphra_v2",           label = "Health Facility Observation"),
    list(name = "tool_obs_latrine_iphra_v2",                   label = "Latrine Observation"),
    list(name = "tool_obs_water_point_iphra_v2",               label = "Water Point Observation")
  )
}

#' tools_master UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_master_ui <- function(id) {
  ns <- NS(id)

  tools <- iphra_tool_definitions()

  # Build one vertical Add/Remove stack per tool.
  tool_cards <- lapply(tools, function(t) {
    shiny::div(
      class = "iphra-tool-card",
      style = "
        display: inline-flex;
        flex-direction: column;
        align-items: stretch;
        min-width: 170px;
        margin: 4px 6px;
        padding: 8px 10px;
        border: 1px solid #d0d5db;
        border-radius: 6px;
        background-color: #f8f9fa;
        vertical-align: top;
        box-shadow: 0 1px 2px rgba(0,0,0,0.04);
      ",
      shiny::div(
        style = "font-weight: 600; font-size: 13px; text-align: center;
                 margin-bottom: 8px; line-height: 1.25em;",
        t$label
      ),
      shiny::div(
        style = "display: flex; flex-direction: column; gap: 6px;",
        shiny::actionButton(
          ns(paste0("add_", t$name)),
          label = "Add Tool",
          icon  = shiny::icon("plus"),
          class = "btn-success btn-sm",
          style = "width: 100%;"
        ),
        shiny::actionButton(
          ns(paste0("remove_", t$name)),
          label = "Remove Tool",
          icon  = shiny::icon("minus"),
          class = "btn-danger btn-sm",
          style = "width: 100%;"
        )
      ),
      shiny::div(
        style = "margin-top: 6px; font-size: 11px; text-align: center; color: #666;",
        shiny::uiOutput(ns(paste0("status_", t$name)), inline = TRUE)
      )
    )
  })

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(ns("export_tools"), "Export All Tools",
                            class = "btn-warning"),
        shiny::br(), shiny::br(),
        shiny::h4("Tools"),
        shiny::div(
          style = "
            white-space: nowrap;
            overflow-x: auto;
            padding: 6px 2px 12px 2px;
            border-top: 1px solid #eee;
            border-bottom: 1px solid #eee;
          ",
          tool_cards
        ),
        shiny::br()
      )
    )
  )
}

#' tools_master Server Functions
#'
#' @noRd
mod_tools_master_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    protocol  <- session$userData$modules[["protocol"]]

    tools <- iphra_tool_definitions()

    # ---- One observer per Add / Remove button --------------------------------
    lapply(tools, function(t) {
      tool_name <- t$name

      observeEvent(input[[paste0("add_", tool_name)]], {
        iphra_try({

          if(tool_name == "tool_household_iphra_v2") {
            protocol$add_tools("tool_household_iphra_v2")
          } else if(tool_name == "tool_kii_community_iphra_v2") {
            protocol$add_tools("tool_kii_community_iphra_v2")
          } else if(tool_name == "tool_kii_fsl_service_provider_iphra_v2") {
            protocol$add_tools("tool_kii_fsl_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_wash_service_provider_iphra_v2") {
            protocol$add_tools("tool_kii_wash_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_markets_iphra_v2") {
            protocol$add_tools("tool_kii_markets_iphra_v2")
          } else if(tool_name == "tool_kii_nutrition_service_provider_iphra_v2") {
            protocol$add_tools("tool_kii_nutrition_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_health_service_provider_iphra_v2") {
            protocol$add_tools("tool_kii_health_service_provider_iphra_v2")
          } else if(tool_name == "tool_obs_community_iphra_v2") {
            protocol$add_tools("tool_obs_community_iphra_v2")
          } else if(tool_name == "tool_obs_crop_livestock_iphra_v1") {
            protocol$add_tools("tool_obs_crop_livestock_iphra_v1")
          } else if(tool_name == "tool_obs_health_facility_iphra_v2") {
            protocol$add_tools("tool_obs_health_facility_iphra_v2")
          } else if(tool_name == "tool_obs_latrine_iphra_v2") {
            protocol$add_tools("tool_obs_latrine_iphra_v2")
          } else if(tool_name == "tool_obs_water_point_iphra_v2") {
            protocol$add_tools("tool_obs_water_point_iphra_v2")
          }

          # Notify downstream modules that the tool list changed.
          session$userData$protocol_tools(names(protocol$tools))

        },
        on_error = "warn",
        origin   = paste0("Tools Master: Add ", tool_name),
        hint     = "Verify the tool name matches protocol$get_allowable_tools().")
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("remove_", tool_name)]], {
        iphra_try({

          if(tool_name == "tool_household_iphra_v2") {
            protocol$remove_tools("tool_household_iphra_v2")
          } else if(tool_name == "tool_kii_community_iphra_v2") {
            protocol$remove_tools("tool_kii_community_iphra_v2")
          } else if(tool_name == "tool_kii_fsl_service_provider_iphra_v2") {
            protocol$remove_tools("tool_kii_fsl_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_wash_service_provider_iphra_v2") {
            protocol$remove_tools("tool_kii_wash_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_markets_iphra_v2") {
            protocol$remove_tools("tool_kii_markets_iphra_v2")
          } else if(tool_name == "tool_kii_nutrition_service_provider_iphra_v2") {
            protocol$remove_tools("tool_kii_nutrition_service_provider_iphra_v2")
          } else if(tool_name == "tool_kii_health_service_provider_iphra_v2") {
            protocol$remove_tools("tool_kii_health_service_provider_iphra_v2")
          } else if(tool_name == "tool_obs_community_iphra_v2") {
            protocol$remove_tools("tool_obs_community_iphra_v2")
          } else if(tool_name == "tool_obs_crop_livestock_iphra_v1") {
            protocol$remove_tools("tool_obs_crop_livestock_iphra_v1")
          } else if(tool_name == "tool_obs_health_facility_iphra_v2") {
            protocol$remove_tools("tool_obs_health_facility_iphra_v2")
          } else if(tool_name == "tool_obs_latrine_iphra_v2") {
            protocol$remove_tools("tool_obs_latrine_iphra_v2")
          } else if(tool_name == "tool_obs_water_point_iphra_v2") {
            protocol$remove_tools("tool_obs_water_point_iphra_v2")
          }

          # Notify downstream modules that the tool list changed.
          session$userData$protocol_tools(names(protocol$tools))

        },
        on_error = "warn",
        origin   = paste0("Tools Master: Remove ", tool_name),
        hint     = "The tool may not currently be added.")
      }, ignoreInit = TRUE)

      # Small status indicator underneath the buttons.
      output[[paste0("status_tool_household_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_household_iphra) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_community_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_community_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_fsl_service_provider_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_fsl_provider_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_wash_service_provider_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_wash_provider_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_markets_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_market_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_nutrition_service_provider_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_nutrition_facility_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_kii_health_service_provider_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_health_facility_kii) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_obs_community_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_community_observation) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_obs_crop_livestock_iphra_v1")]] <- shiny::renderUI({

        if(protocol$.tool_crops_livestock_observation) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_obs_health_facility_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_health_facility_observation) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_obs_latrine_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_latrine_observation) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

      output[[paste0("status_tool_obs_water_point_iphra_v2")]] <- shiny::renderUI({

        if(protocol$.tool_water_point_observation) {
          shiny::span(style = "color: #2b8a3e; font-weight: 600;", "Added")
        } else {
          shiny::span(style = "color: #868e96;", "Not added")
        }

      })

    })

  })
}

## To be copied in the UI
# mod_tools_master_ui("tools_master_1")

## To be copied in the server
# mod_tools_master_server("tools_master_1")
