#' tools_wash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_wash_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      # --- Modern Checkbox: WASH Tools Complete ---
      shiny::div(
        style = "
          display: flex;
          align-items: center;
          justify-content: left;
          padding: 6px 14px;             /* slightly more breathing room */
          border: 1px solid #ccc;
          border-radius: 6px;
          background-color: #f8f9fa;
          margin-top: 8px;               /* gentle space from top of tab */
          margin-bottom: 6px;            /* half previous gap to presets */
          margin-left: 15px;
          width: fit-content;
          line-height: 1.2em;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);  /* subtle depth */
        ",
        tags$label(
          class = "checkbox-inline",
          style = "
            margin: 0;
            font-weight: 600;
            font-size: 14px;
            display: flex;
            align-items: center;
            gap: 6px;                     /* tighter space between box and text */
          ",
          shiny::tags$input(
            type = "checkbox",
            id = ns("wash_complete"),
            name = ns("wash_complete"),
            onchange = sprintf("Shiny.setInputValue('%s', this.checked);", ns("wash_complete"))
          ),
          tags$span("WASH Tools Complete")
        )
      ),
      shinydashboard::box(
        title = "WASH Service Provider Key Informant Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj_wash_kii"), "Match Objectives"),
        shiny::actionButton(ns("preset_core_wash_kii"), "Core WASH Service Provider KII"),
        shiny::actionButton(ns("preset_full_wash_kii"), "Full WASH Service Provider KII"),
        shiny::actionButton(ns("export_tool_kobo_wash_kii"), "Export WASH Service Provider KII Kobo Tool", class = "btn-success"),
        shiny::actionButton(ns("export_tool_paper_wash_kii"), "Export WASH Service Provider KII Paper Tool", class = "btn-success"),

      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_wash_kii_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_wash_kii_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table_wash_kii"))
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Water Point Observation Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj_water_obs"), "Match Objectives"),
        shiny::actionButton(ns("preset_core_water_obs"), "Core Water Point Observation Tool"),
        shiny::actionButton(ns("preset_full_water_obs"), "Full Water Point Observation Tool"),
        shiny::actionButton(ns("export_tool_kobo_water_obs"), "Export Water Point Observation Kobo Tool", class = "btn-success"),
        shiny::actionButton(ns("export_tool_paper_water_obs"), "Export Water Point Observation Paper Tool", class = "btn-success")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_water_obs_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_water_obs_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table_water_obs"))
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Latrine Observation Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj_latrine_obs"), "Match Objectives"),
        shiny::actionButton(ns("preset_core_latrine_obs"), "Core Latrine Observation Tool"),
        shiny::actionButton(ns("preset_full_latrine_obs"), "Full Latrine Observation Tool"),
        shiny::actionButton(ns("export_tool_kobo_latrine_obs"), "Export Latrine Observation Kobo Tool", class = "btn-success"),
        shiny::actionButton(ns("export_tool_paper_latrine_obs"), "Export Latrine Observation Paper Tool", class = "btn-success")

      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_latrine_obs_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_latrine_obs_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table_latrine_obs"))
        )
      )
    )

  )
}

#' tools_wash Server Functions
#'
#' @noRd
mod_tools_wash_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Indicators definition (lives inside module) ----
    indicators_wash_kii <- list(
      Demographics        = c("Household size", "Age distribution"),
      Health_Core         = c("Access to care", "Illness prevalence"),
      Health_Supplemental = character(0),
      Nutrition_Core      = c("Perceived Nutrition Need", "Perceived Nutrition"),
      FSL_Core            = c("Market access", "Food consumption score"),
      FSL_Supplemental    = character(0),
      WASH_Core           = c("Water source", "Latrine access"),
      WASH_Supplemental   = character(0),
      Shelter_Core        = c("Shelter type", "Overcrowding"),
      Shelter_Supplemental= character(0),
      Protection_Core     = c("Protection Needs"),
      Protection_Supplemental = character(0),
      Priority_Needs      = c("Other Needs", "Top Three Perceived Needs")
    )

    # Estimated times per indicator (mins)
    indicator_times_wash_kii <- setNames(
      rep(5, length(unlist(indicators_wash_kii))),
      unlist(indicators_wash_kii)
    )

    all_indicators_wash_kii <- unlist(indicators_wash_kii, use.names = FALSE)

    # ---- Reactive state ----
    selected_wash_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_wash_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_wash_kii, selected_wash_kii()),
        input_id = ns("available_wash_kii"),
        options = sortable::sortable_options(group = ns("indicators_wash_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_wash_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_wash_kii(),
        input_id = ns("selected_wash_kii"),
        options = sortable::sortable_options(group = ns("indicators_wash_kii"))
      )
    })

    # ---- Keep selected_wash_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_wash_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_wash_kii)) {
          iphra_message(
            iphra_txt("No WASH KII selection detected — skipping sync update."),
            origin = iphra_txt("WASH KII Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_wash_server/observeEvent_selected_wash_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_wash_kii(input$selected_wash_kii)
        iphra_message(
          paste0(
            iphra_txt("WASH KII selection synchronized with: "),
            paste(input$selected_wash_kii, collapse = ", ")
          ),
          origin = iphra_txt("WASH KII Tool: Selection Sync")
        )

        # --- Future: sync WASH KII selection with session/project state ---
        # session$userData$project$set_selection("wash_kii", input$selected_wash_kii)
        }, step = "mod_tools_wash_server/observeEvent_selected_wash_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("WASH KII selection synchronization completed successfully."),
          origin = iphra_txt("WASH KII Tool: Selection Sync")
        )
        }, step = "mod_tools_wash_server/observeEvent_selected_wash_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("WASH KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for WASH KII Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_wash_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("WASH KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_wash_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_wash_kii(c(
          indicators_wash_kii$Demographics,
          indicators_wash_kii$FSL_Core,
          indicators_wash_kii$WASH_Core,
          indicators_wash_kii$Health_Core,
          indicators_wash_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("WASH KII objectives preset applied successfully."),
          origin = iphra_txt("WASH KII Tool: Preset Objectives")
        )

        # --- Future: store preset selection in session/project state ---
        # session$userData$project$set_selection("wash_kii_obj", selected_wash_kii())
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_wash_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("WASH KII objectives preset selection completed."),
          origin = iphra_txt("WASH KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_wash_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("WASH KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_wash_kii object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_wash_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("WASH KII Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_wash_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_wash_kii(c(
          indicators_wash_kii$FSL_Core,
          indicators_wash_kii$WASH_Core,
          indicators_wash_kii$Health_Core,
          indicators_wash_kii$Nutrition_Core,
          indicators_wash_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("WASH KII core preset applied successfully."),
          origin = iphra_txt("WASH KII Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("wash_kii_core", selected_wash_kii())
        }, step = "mod_tools_wash_server/observeEvent_preset_core_wash_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("WASH KII core preset selection completed."),
          origin = iphra_txt("WASH KII Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_wash_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("WASH KII Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for WASH KII are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_wash_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("WASH KII Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_wash_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_wash_kii(all_indicators_wash_kii)
        iphra_message(
          iphra_txt("Full WASH KII preset applied successfully."),
          origin = iphra_txt("WASH KII Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("wash_kii_full", selected_wash_kii())
        }, step = "mod_tools_wash_server/observeEvent_preset_full_wash_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full WASH KII preset selection completed."),
          origin = iphra_txt("WASH KII Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_wash_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("WASH KII Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_wash_kii is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_wash_kii <- shiny::renderTable({
      sel <- selected_wash_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_wash_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      wash_summary <- lapply(names(indicators_wash_kii), function(sector) {
        inds <- indicators_wash_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_wash_kii[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      wash_summary <- do.call(rbind, wash_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(wash_summary$Indicators),
        Minutes = sum(wash_summary$Minutes)
      )

      rbind(wash_summary, totals)
    })

    # ---- Indicators definition (lives inside module) ----
    indicators_water_obs <- list(
      Demographics        = c("Household size", "Age distribution"),
      Health_Core         = c("Access to care", "Illness prevalence"),
      Health_Supplemental = character(0),
      Nutrition_Core      = c("Perceived Nutrition Need", "Perceived Nutrition"),
      FSL_Core            = c("Market access", "Food consumption score"),
      FSL_Supplemental    = character(0),
      WASH_Core           = c("Water source", "Latrine access"),
      WASH_Supplemental   = character(0),
      Shelter_Core        = c("Shelter type", "Overcrowding"),
      Shelter_Supplemental= character(0),
      Protection_Core     = c("Protection Needs"),
      Protection_Supplemental = character(0),
      Priority_Needs      = c("Other Needs", "Top Three Perceived Needs")
    )

    # Estimated times per indicator (mins)
    indicator_times_water_obs <- setNames(
      rep(5, length(unlist(indicators_water_obs))),
      unlist(indicators_water_obs)
    )

    all_indicators_water_obs <- unlist(indicators_water_obs, use.names = FALSE)

    # ---- Reactive state ----
    selected_water_obs <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_water_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_water_obs, selected_water_obs()),
        input_id = ns("available_water_obs"),
        options = sortable::sortable_options(group = ns("indicators_water_obs"))
      )
    })

    # ---- UI for selected list ----
    output$selected_water_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_water_obs(),
        input_id = ns("selected_water_obs"),
        options = sortable::sortable_options(group = ns("indicators_water_obs"))
      )
    })

    # ---- Keep selected_water_obs() in sync with drag-and-drop ----
    observeEvent(input$selected_water_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_water_obs)) {
          iphra_message(
            iphra_txt("No Water Observation selection detected — skipping sync update."),
            origin = iphra_txt("Water Observation Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_wash_server/observeEvent_selected_water_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_water_obs(input$selected_water_obs)
        iphra_message(
          paste0(
            iphra_txt("Water Observation selection synchronized with: "),
            paste(input$selected_water_obs, collapse = ", ")
          ),
          origin = iphra_txt("Water Observation Tool: Selection Sync")
        )

        # --- Future: sync Water Observation selection with session/project state ---
        # session$userData$project$set_selection("water_obs", input$selected_water_obs)
        }, step = "mod_tools_wash_server/observeEvent_selected_water_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Water Observation selection synchronization completed successfully."),
          origin = iphra_txt("Water Observation Tool: Selection Sync")
        )
        }, step = "mod_tools_wash_server/observeEvent_selected_water_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Water Observation Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Water Observation Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_water_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Water Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_water_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_water_obs(c(
          indicators_water_obs$Demographics,
          indicators_water_obs$FSL_Core,
          indicators_water_obs$WASH_Core,
          indicators_water_obs$Health_Core,
          indicators_water_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Water Observation objectives preset applied successfully."),
          origin = iphra_txt("Water Observation Tool: Preset Objectives")
        )

        # --- Future: store preset selection in session/project state ---
        # session$userData$project$set_selection("water_obs_obj", selected_water_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_water_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Water Observation objectives preset selection completed."),
          origin = iphra_txt("Water Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_water_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Water Observation Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_water_obs object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_water_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Water Observation Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_water_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_water_obs(c(
          indicators_water_obs$FSL_Core,
          indicators_water_obs$WASH_Core,
          indicators_water_obs$Health_Core,
          indicators_water_obs$Nutrition_Core,
          indicators_water_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Water Observation core preset applied successfully."),
          origin = iphra_txt("Water Observation Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("water_obs_core", selected_water_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_core_water_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Water Observation core preset selection completed."),
          origin = iphra_txt("Water Observation Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_water_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Water Observation Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Water Observation are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_water_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Water Observation Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_water_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_water_obs(all_indicators_water_obs)
        iphra_message(
          iphra_txt("Full Water Observation preset applied successfully."),
          origin = iphra_txt("Water Observation Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("water_obs_full", selected_water_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_full_water_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Water Observation preset selection completed."),
          origin = iphra_txt("Water Observation Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_water_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Water Observation Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_water_obs is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_water_obs <- shiny::renderTable({
      sel <- selected_water_obs()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_water_obs), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      water_obs_summary <- lapply(names(indicators_water_obs), function(sector) {
        inds <- indicators_water_obs[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_water_obs[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      water_obs_summary <- do.call(rbind, water_obs_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(water_obs_summary$Indicators),
        Minutes = sum(water_obs_summary$Minutes)
      )

      rbind(water_obs_summary, totals)
    })




    # ---- Indicators definition (lives inside module) ----
    indicators_latrine_obs <- list(
      Demographics        = c("Household size", "Age distribution"),
      Health_Core         = c("Access to care", "Illness prevalence"),
      Health_Supplemental = character(0),
      Nutrition_Core      = c("Perceived Nutrition Need", "Perceived Nutrition"),
      FSL_Core            = c("Market access", "Food consumption score"),
      FSL_Supplemental    = character(0),
      WASH_Core           = c("Water source", "Latrine access"),
      WASH_Supplemental   = character(0),
      Shelter_Core        = c("Shelter type", "Overcrowding"),
      Shelter_Supplemental= character(0),
      Protection_Core     = c("Protection Needs"),
      Protection_Supplemental = character(0),
      Priority_Needs      = c("Other Needs", "Top Three Perceived Needs")
    )

    # Estimated times per indicator (mins)
    indicator_times_latrine_obs <- setNames(
      rep(5, length(unlist(indicators_latrine_obs))),
      unlist(indicators_latrine_obs)
    )

    all_indicators_latrine_obs <- unlist(indicators_latrine_obs, use.names = FALSE)

    # ---- Reactive state ----
    selected_latrine_obs <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_latrine_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_latrine_obs, selected_latrine_obs()),
        input_id = ns("available_latrine_obs"),
        options = sortable::sortable_options(group = ns("indicators_latrine_obs"))
      )
    })

    # ---- UI for selected list ----
    output$selected_latrine_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_latrine_obs(),
        input_id = ns("selected_latrine_obs"),
        options = sortable::sortable_options(group = ns("indicators_latrine_obs"))
      )
    })

    # ---- Keep selected_latrine_obs() in sync with drag-and-drop ----
    observeEvent(input$selected_latrine_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_latrine_obs)) {
          iphra_message(
            iphra_txt("No Latrine Observation selection detected — skipping sync update."),
            origin = iphra_txt("Latrine Observation Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_wash_server/observeEvent_selected_latrine_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_latrine_obs(input$selected_latrine_obs)
        iphra_message(
          paste0(
            iphra_txt("Latrine Observation selection synchronized with: "),
            paste(input$selected_latrine_obs, collapse = ", ")
          ),
          origin = iphra_txt("Latrine Observation Tool: Selection Sync")
        )

        # --- Future: sync Latrine Observation selection with session/project state ---
        # session$userData$project$set_selection("latrine_obs", input$selected_latrine_obs)
        }, step = "mod_tools_wash_server/observeEvent_selected_latrine_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Latrine Observation selection synchronization completed successfully."),
          origin = iphra_txt("Latrine Observation Tool: Selection Sync")
        )
        }, step = "mod_tools_wash_server/observeEvent_selected_latrine_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Latrine Observation Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Latrine Observation Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_latrine_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Latrine Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_latrine_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_latrine_obs(c(
          indicators_latrine_obs$Demographics,
          indicators_latrine_obs$FSL_Core,
          indicators_latrine_obs$WASH_Core,
          indicators_latrine_obs$Health_Core,
          indicators_latrine_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Latrine Observation objectives preset applied successfully."),
          origin = iphra_txt("Latrine Observation Tool: Preset Objectives")
        )

        # --- Future: store preset selection in session/project state ---
        # session$userData$project$set_selection("latrine_obs_obj", selected_latrine_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_latrine_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Latrine Observation objectives preset selection completed."),
          origin = iphra_txt("Latrine Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_obj_latrine_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Latrine Observation Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_latrine_obs object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_latrine_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Latrine Observation Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_latrine_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_latrine_obs(c(
          indicators_latrine_obs$FSL_Core,
          indicators_latrine_obs$WASH_Core,
          indicators_latrine_obs$Health_Core,
          indicators_latrine_obs$Nutrition_Core,
          indicators_latrine_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Latrine Observation core preset applied successfully."),
          origin = iphra_txt("Latrine Observation Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("latrine_obs_core", selected_latrine_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_core_latrine_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Latrine Observation core preset selection completed."),
          origin = iphra_txt("Latrine Observation Tool: Preset Core")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_core_latrine_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Latrine Observation Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Latrine Observation are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_latrine_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Latrine Observation Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_latrine_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_latrine_obs(all_indicators_latrine_obs)
        iphra_message(
          iphra_txt("Full Latrine Observation preset applied successfully."),
          origin = iphra_txt("Latrine Observation Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("latrine_obs_full", selected_latrine_obs())
        }, step = "mod_tools_wash_server/observeEvent_preset_full_latrine_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Latrine Observation preset selection completed."),
          origin = iphra_txt("Latrine Observation Tool: Preset Full")
        )
        }, step = "mod_tools_wash_server/observeEvent_preset_full_latrine_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Latrine Observation Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_latrine_obs is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_latrine_obs <- shiny::renderTable({
      sel <- selected_latrine_obs()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_latrine_obs), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      latrine_obs_summary <- lapply(names(indicators_latrine_obs), function(sector) {
        inds <- indicators_latrine_obs[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_latrine_obs[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      latrine_obs_summary <- do.call(rbind, latrine_obs_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(latrine_obs_summary$Indicators),
        Minutes = sum(latrine_obs_summary$Minutes)
      )

      rbind(latrine_obs_summary, totals)
    })

    # ---- WASH Tools: Completion Toggle ----
    observeEvent(input$wash_complete, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("WASH Tools: Completion Toggle")
        )
        }, step = "mod_tools_wash_server/observeEvent_wash_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (isTRUE(input$wash_complete)) {
          iphra_message(
            iphra_txt("WASH Tools marked as complete ✅"),
            origin = iphra_txt("WASH Tools: Completion Toggle")
          )

          # --- Future: update project/session state to mark WASH Tools as complete ---
          # session$userData$project$set_stage_complete("wash_tools", TRUE)

        } else {
          iphra_message(
            iphra_txt("WASH Tools marked as incomplete ❌"),
            origin = iphra_txt("WASH Tools: Completion Toggle")
          )

          # --- Future: update project/session state to mark WASH Tools as incomplete ---
          # session$userData$project$set_stage_complete("wash_tools", FALSE)
        }
        }, step = "mod_tools_wash_server/observeEvent_wash_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("WASH Tools completion toggle processed successfully."),
          origin = iphra_txt("WASH Tools: Completion Toggle")
        )
        }, step = "mod_tools_wash_server/observeEvent_wash_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("WASH Tools: Completion Toggle"),
      hint = iphra_txt("Check reactive binding or input state if toggle fails.")
      )
    })

  })
}

## To be copied in the UI
# mod_tools_wash_ui("tools_wash_1")

## To be copied in the server
# mod_tools_wash_server("tools_wash_1")
