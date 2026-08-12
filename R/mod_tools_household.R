#' tools_household UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_household_ui <- function(id, all_indicators) {
  ns <- NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.tool_present == true",
      ns = ns,
      shiny::fluidRow(

        # --- Existing Indicators Preset Box ---
        shinydashboard::box(
        title = "Household Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj"), "Match Objectives"),
        shiny::actionButton(ns("preset_core"), "Core"),
        shiny::actionButton(ns("export_tool"), "Export Household Tool", class = "btn-success")
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table"))
        )
      )
    )
    )  # /conditionalPanel
  )
}

#' tools_household Server Functions
#'
#' @noRd
mod_tools_household_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # SETUP ####
    protocol <- session$userData$modules[["protocol"]]
    framework <- protocol$access_nested(field = "framework")
    tool <- protocol$access_nested(field = "tools", name = "tool_household_iphra_v2")

    # ---- All indicators from framework

    all_indicators_static <- framework$master_indicator_bank[framework$master_indicator_bank$tool == "household", c("indicator_code", "indicator_name")]

    #OUTPUTS ####

    # ---- Tool presence flag for conditional UI ----
    output$tool_present <- shiny::reactive({
      protocol$.tool_household_iphra
    })

    shiny::outputOptions(output, "tool_present", suspendWhenHidden = FALSE)

    # ---- Reactive available indicators sourced from the framework's
    # modified_indicator_bank (indicator_name used for display). Falls
    # back to the static list when the bank is empty so the UI still
    # shows something during early / stub sessions. ----

    # REACTIVES ####

    all_indicators <- shiny::reactive({
      # Re-evaluate whenever the indicator bank version changes.

      framework$modified_indicator_bank[framework$modified_indicator_bank$tool == "household", c("indicator_code", "indicator_name")]

    })

    selected <- shiny::reactiveVal(character(0))

    selected_indicators <- shiny::reactive({
      inds <- all_indicators()
      sel  <- selected()

      inds[inds$indicator_name %in% sel, ]
    })

    # OUTPUTS ####


    # ---- UI for available list ----
    output$available_ui <- shiny::renderUI({

      labels <- all_indicators()

      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(labels$indicator_name, selected()),
        input_id = ns("available"),
        options = sortable::sortable_options(group = ns("indicators"))
      )
    })

    # ---- UI for selected list
    output$selected_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected(),
        input_id = ns("selected"),
        options = sortable::sortable_options(group = ns("indicators"))
      )
    })

    # ---- Summary table ----
    output$summary_table <- shiny::renderTable({

      sel <- selected()

      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      sector_summary <- lapply(names(indicators), function(sector) {
        inds <- indicators[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      sector_summary <- do.call(rbind, sector_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(sector_summary$Indicators),
        Minutes = sum(sector_summary$Minutes)
      )

      rbind(sector_summary, totals)
    })

    # OBSERVES ####

    # ---- Keep Tool in sync with selected
    observeEvent(input$selected, {
      iphra_try({

        indicators_selected <- selected_indicators()

        tool$filter_survey_by_indicator(indicator_codes = indicators_selected$indicator_code)

        iphra_message(
          paste0(
            iphra_txt("Selection synchronized with: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = iphra_txt("Household Tool: Selection Sync")
        )
      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Selection Sync"),
      hint = iphra_txt("Ensure the drag-and-drop or selection input is properly bound.")
      )
    })

    # ---- Presets ----

    # Preset: Objectives
    observeEvent(input$preset_obj, {
      iphra_try({


        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset Objectives")
        )
        }, step = "mod_tools_household_server/observeEvent_preset_obj/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected(c(
          indicators$Demographics,
          indicators$FSL_Core,
          indicators$WASH_Core,
          indicators$Health_Core,
          indicators$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Objectives preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset Objectives")
        )
        }, step = "mod_tools_household_server/observeEvent_preset_obj/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Preset Objectives selection completed."),
          origin = iphra_txt("Household Tool: Preset Objectives")
        )
        }, step = "mod_tools_household_server/observeEvent_preset_obj/Result Handling")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Objectives"),
      hint = iphra_txt("Verify that all indicator groups exist in the indicators object.")
      )
    })

  })
}

## To be copied in the UI
# mod_tools_household_ui("tools_household_1")

## To be copied in the server
# mod_tools_household_server("tools_household_1")
