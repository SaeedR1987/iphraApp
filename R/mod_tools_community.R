#' tools_community UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_community_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::fluidRow(


      # --- Modern Checkbox: Community Tools Complete ---
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
            id = ns("community_complete"),
            name = ns("community_complete"),
            onchange = sprintf("Shiny.setInputValue('%s', this.checked);", ns("community_complete"))
          ),
          tags$span("Community Tools Complete")
        )
      ),
      shinydashboard::box(
        title = "Community Key Informant Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj_kii"), "Match Objectives"),
        shiny::actionButton(ns("preset_core_kii"), "Core KII"),
        shiny::actionButton(ns("preset_full_kii"), "Full KII"),
        shiny::actionButton(ns("export_tool_kii"), "Export Community KII Tool", class = "btn-success"),
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_kii_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_kii_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table_kii"))
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Community Observation Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj_obs"), "Match Objectives"),
        shiny::actionButton(ns("preset_core_obs"), "Core Observation Tool"),
        shiny::actionButton(ns("preset_full_obs"), "Full Observation Tool"),
        shiny::actionButton(ns("export_tool_obs"), "Export Community Observation Tool", class = "btn-success")

      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("available_obs_ui"))
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("selected_obs_ui"))
      ),
      shiny::column(
        4,
        shinydashboard::box(
          title = "Summary of Selected Indicators",
          width = 12,
          shiny::tableOutput(ns("summary_table_obs"))
        )
      )
    )

  )
}

#' tools_community Server Functions
#'
#' @noRd
mod_tools_community_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Indicators definition (lives inside module) ----
    indicators_kii <- list(
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
    indicator_times_kii <- setNames(
      rep(5, length(unlist(indicators_kii))),
      unlist(indicators_kii)
    )

    all_indicators_kii <- unlist(indicators_kii, use.names = FALSE)

    # ---- Reactive state ----
    selected_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_kii, selected_kii()),
        input_id = ns("available_kii"),
        options = sortable::sortable_options(group = ns("indicators_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_kii(),
        input_id = ns("selected_kii"),
        options = sortable::sortable_options(group = ns("indicators_kii"))
      )
    })

    # ---- Keep selected_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_kii, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$selected_kii)) {
          iphra_message(
            iphra_txt("No KII selection detected — skipping sync update."),
            origin = iphra_txt("KII Tool: Selection Sync")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_selected_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected_kii(input$selected_kii)
        iphra_message(
          paste0(
            iphra_txt("KII selection synchronized with: "),
            paste(input$selected_kii, collapse = ", ")
          ),
          origin = iphra_txt("KII Tool: Selection Sync")
        )

        # --- Future: update project state or dependent reactives ---
        # session$userData$project$set_selection("kii", input$selected_kii)

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_selected_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("KII selection synchronization completed successfully."),
          origin = iphra_txt("KII Tool: Selection Sync")
        )
        }, step = "mod_tools_community_server/observeEvent_selected_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure KII drag-and-drop input is properly bound to the UI.")
      )
    })

    # ---- Presets (KII) ----

    # Preset: Objectives
    observeEvent(input$preset_obj_kii, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("KII Tool: Preset Objectives")
        )

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_preset_obj_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected_kii(c(
          indicators_kii$Demographics,
          indicators_kii$FSL_Core,
          indicators_kii$WASH_Core,
          indicators_kii$Health_Core,
          indicators_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("KII objectives preset applied successfully."),
          origin = iphra_txt("KII Tool: Preset Objectives")
        )

        # --- Future: update project session state for KII objectives ---
        # session$userData$project$set_selection("kii", selected_kii())

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_preset_obj_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("KII objectives preset selection completed."),
          origin = iphra_txt("KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_obj_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_kii object is correctly defined and accessible.")
      )
    })


    # Preset: Core
    observeEvent(input$preset_core_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("KII Tool: Preset Core")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_core_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_kii(c(
          indicators_kii$FSL_Core,
          indicators_kii$WASH_Core,
          indicators_kii$Health_Core,
          indicators_kii$Nutrition_Core,
          indicators_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("KII core preset applied successfully."),
          origin = iphra_txt("KII Tool: Preset Core")
        )

        # --- Future: propagate selection to session for KII core indicators ---
        # session$userData$project$set_selection("kii_core", selected_kii())
        }, step = "mod_tools_community_server/observeEvent_preset_core_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("KII core preset selection completed."),
          origin = iphra_txt("KII Tool: Preset Core")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_core_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("KII Tool: Preset Core"),
      hint = iphra_txt("Check that indicators_kii core components exist and are populated.")
      )
    })


    # Preset: Full
    observeEvent(input$preset_full_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("KII Tool: Preset Full")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_full_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_kii(all_indicators_kii)
        iphra_message(
          iphra_txt("Full KII preset applied successfully."),
          origin = iphra_txt("KII Tool: Preset Full")
        )

        # --- Future: sync with project session-level KII data ---
        # session$userData$project$set_selection("kii_full", selected_kii())
        }, step = "mod_tools_community_server/observeEvent_preset_full_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full KII preset selection completed."),
          origin = iphra_txt("KII Tool: Preset Full")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_full_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("KII Tool: Preset Full"),
      hint = iphra_txt("Ensure all_indicators_kii object is properly defined in the environment.")
      )
    })

    # ---- Summary table ----
    output$summary_table_kii <- shiny::renderTable({
      sel <- selected_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      sector_summary <- lapply(names(indicators_kii), function(sector) {
        inds <- indicators_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_kii[sel[sel %in% inds]])
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






    # ---- Indicators definition (lives inside module) ----
    indicators_obs <- list(
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
    indicator_times_obs <- setNames(
      rep(5, length(unlist(indicators_obs))),
      unlist(indicators_obs)
    )

    all_indicators_obs <- unlist(indicators_obs, use.names = FALSE)

    # ---- Reactive state ----
    selected_obs <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_obs, selected_obs()),
        input_id = ns("available_obs"),
        options = sortable::sortable_options(group = ns("indicators_obs"))
      )
    })

    # ---- UI for selected list ----
    output$selected_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_obs(),
        input_id = ns("selected_obs"),
        options = sortable::sortable_options(group = ns("indicators_obs"))
      )
    })

    # ---- Keep selected_obs() in sync with drag-and-drop ----
    observeEvent(input$selected_obs, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$selected_obs)) {
          iphra_message(
            iphra_txt("No Observation Tool selection detected — skipping sync update."),
            origin = iphra_txt("Observation Tool: Selection Sync")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_selected_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected_obs(input$selected_obs)
        iphra_message(
          paste0(
            iphra_txt("Observation Tool selection synchronized with: "),
            paste(input$selected_obs, collapse = ", ")
          ),
          origin = iphra_txt("Observation Tool: Selection Sync")
        )

        # --- Future: update project or user session selection state ---
        # session$userData$project$set_selection("observation", input$selected_obs)

        # ────────────────────────────────────────────────
        }, step = "mod_tools_community_server/observeEvent_selected_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Observation Tool selection synchronization completed successfully."),
          origin = iphra_txt("Observation Tool: Selection Sync")
        )
        }, step = "mod_tools_community_server/observeEvent_selected_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Observation Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop or selection input for Observation Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_obj_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_obs(c(
          indicators_obs$Demographics,
          indicators_obs$FSL_Core,
          indicators_obs$WASH_Core,
          indicators_obs$Health_Core,
          indicators_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Observation objectives preset applied successfully."),
          origin = iphra_txt("Observation Tool: Preset Objectives")
        )

        # --- Future: update KII preset state at project/session level ---
        # session$userData$project$set_selection("observation_obj", selected_obs())
        }, step = "mod_tools_community_server/observeEvent_preset_obj_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Observation objectives preset selection completed."),
          origin = iphra_txt("Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_obj_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Observation Tool: Preset Objectives"),
      hint = iphra_txt("Check that indicators_obs object is properly defined and accessible.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Observation Tool: Preset Core")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_core_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_obs(c(
          indicators_obs$FSL_Core,
          indicators_obs$WASH_Core,
          indicators_obs$Health_Core,
          indicators_obs$Nutrition_Core,
          indicators_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Observation core preset applied successfully."),
          origin = iphra_txt("Observation Tool: Preset Core")
        )

        # --- Future: update project session with Observation Core selection ---
        # session$userData$project$set_selection("observation_core", selected_obs())
        }, step = "mod_tools_community_server/observeEvent_preset_core_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Observation core preset selection completed."),
          origin = iphra_txt("Observation Tool: Preset Core")
        )
        }, step = "mod_tools_community_server/observeEvent_preset_core_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Observation Tool: Preset Core"),
      hint = iphra_txt("Ensure indicator core lists for Observation Tool are valid and loaded.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_obs, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Observation Tool: Preset Full")
        )

        selected_obs(all_indicators_obs)
        iphra_message(
          iphra_txt("Full Observation preset applied successfully."),
          origin = iphra_txt("Observation Tool: Preset Full")
        )

        # --- Future: sync full Observation selection with session/project ---
        # session$userData$project$set_selection("observation_full", selected_obs())

        iphra_message(
          iphra_txt("Full Observation preset selection completed."),
          origin = iphra_txt("Observation Tool: Preset Full")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Observation Tool: Preset Full"),
      hint = iphra_txt("Confirm all_indicators_obs is correctly defined in the environment.")
      )
    })

    # ---- Summary table ----
    output$summary_table_obs <- shiny::renderTable({
      sel <- selected_obs()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_obs), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      sector_summary_obs <- lapply(names(indicators_obs), function(sector) {
        inds <- indicators_obs[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_obs[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      sector_summary_obs <- do.call(rbind, sector_summary_obs)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(sector_summary_obs$Indicators),
        Minutes = sum(sector_summary_obs$Minutes)
      )

      rbind(sector_summary_obs, totals)
    })

    # ---- Toggle: Community Tools Complete ----
    observeEvent(input$community_complete, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$community_complete)) {
          iphra_message(
            iphra_txt("Checkbox state is NULL — skipping update."),
            origin = iphra_txt("Community Tool: Completion Toggle")
          )
          return(NULL)
        }
        }, step = "mod_tools_community_server/observeEvent_community_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (isTRUE(input$community_complete)) {
          iphra_message(
            iphra_txt("Community Tools marked as complete ✅"),
            origin = iphra_txt("Community Tool: Completion Toggle")
          )

          # --- Future: save completion status to project/session ---
          # session$userData$project$set_stage_completed("community_tools", TRUE)

        } else {
          iphra_message(
            iphra_txt("Community Tools marked as incomplete ❌"),
            origin = iphra_txt("Community Tool: Completion Toggle")
          )

          # --- Future: reset completion flag in session/project ---
          # session$userData$project$set_stage_completed("community_tools", FALSE)
        }
        }, step = "mod_tools_community_server/observeEvent_community_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Community Tools completion status updated successfully."),
          origin = iphra_txt("Community Tool: Completion Toggle")
        )
        }, step = "mod_tools_community_server/observeEvent_community_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Community Tool: Completion Toggle"),
      hint = iphra_txt("Verify checkbox binding and project completion update logic if this fails.")
      )
    })

  })

}

## To be copied in the UI
# mod_tools_community_ui("tools_community_1")

## To be copied in the server
# mod_tools_community_server("tools_community_1")
