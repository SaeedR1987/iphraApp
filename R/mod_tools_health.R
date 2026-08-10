#' tools_health UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_health_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::conditionalPanel(
      condition = "output.health_kii_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Health Facility Key Informant Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_health_kii"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_health_kii"), "Core Health Facility KII"),
          shiny::actionButton(ns("preset_full_health_kii"), "Full Health Facility KII"),
          shiny::actionButton(ns("export_tool_health_kii"), "Export Health Facility KII Tool", class = "btn-success"),
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_health_kii_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_health_kii_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_health_kii"))
          )
        )
      )
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.health_obs_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Health Facility Observation Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_health_obs"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_health_obs"), "Core Health Facility Observation Tool"),
          shiny::actionButton(ns("preset_full_health_obs"), "Full Health Facility Observation Tool"),
          shiny::actionButton(ns("export_tool_health_obs"), "Export Health Facility Observation Tool", class = "btn-success")

        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_health_obs_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_health_obs_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_health_obs"))
          )
        )
      )
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.nutrition_kii_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Nutrition Facility KII Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_nutrition_kii"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_nutrition_kii"), "Core Nutrition Facility KII Tool"),
          shiny::actionButton(ns("preset_full_nutrition_kii"), "Full Nutrition Facility KII Tool"),
          shiny::actionButton(ns("export_tool_nutrition_kii"), "Export Nutrition Facility KII Tool", class = "btn-success")

        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_nutrition_kii_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_nutrition_kii_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_nutrition_kii"))
          )
        )
      )
    )


  )
}

#' tools_health Server Functions
#'
#' @noRd
mod_tools_health_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Indicators definition (lives inside module) ----
    indicators_health_kii <- list(
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
    indicator_times_health_kii <- setNames(
      rep(5, length(unlist(indicators_health_kii))),
      unlist(indicators_health_kii)
    )

    all_indicators_health_kii_static <- unlist(indicators_health_kii, use.names = FALSE)

    output$health_kii_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_kii_health_service_provider_iphra_v2", session)
    })
    shiny::outputOptions(output, "health_kii_present", suspendWhenHidden = FALSE)

    all_indicators_health_kii <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_health_kii_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_health_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_health_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_health_kii(), selected_health_kii()),
        input_id = ns("available_health_kii"),
        options = sortable::sortable_options(group = ns("indicators_health_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_health_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_health_kii(),
        input_id = ns("selected_health_kii"),
        options = sortable::sortable_options(group = ns("indicators_health_kii"))
      )
    })

    # ---- Keep selected_health_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_health_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_health_kii)) {
          iphra_message(
            iphra_txt("No Health KII selection detected — skipping sync update."),
            origin = iphra_txt("Health KII Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_health_server/observeEvent_selected_health_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_kii(input$selected_health_kii)

          codes <- iphra_indicator_names_to_codes(input$selected_health_kii, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_kii_health_service_provider_iphra_v2",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("Health KII selection synchronized with: "),
            paste(input$selected_health_kii, collapse = ", ")
          ),
          origin = iphra_txt("Health KII Tool: Selection Sync")
        )

        # --- Future: sync Health KII selection with session/project state ---
        # session$userData$project$set_selection("health_kii", input$selected_health_kii)
        }, step = "mod_tools_health_server/observeEvent_selected_health_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health KII selection synchronization completed successfully."),
          origin = iphra_txt("Health KII Tool: Selection Sync")
        )
        }, step = "mod_tools_health_server/observeEvent_selected_health_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Health KII Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_health_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_kii(c(
          indicators_health_kii$Demographics,
          indicators_health_kii$FSL_Core,
          indicators_health_kii$WASH_Core,
          indicators_health_kii$Health_Core,
          indicators_health_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Health KII objectives preset applied successfully."),
          origin = iphra_txt("Health KII Tool: Preset Objectives")
        )

        # --- Future: save preset selection to session/project state ---
        # session$userData$project$set_selection("health_kii_obj", selected_health_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health KII objectives preset selection completed."),
          origin = iphra_txt("Health KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_health_kii object is correctly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_health_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health KII Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_kii(c(
          indicators_health_kii$FSL_Core,
          indicators_health_kii$WASH_Core,
          indicators_health_kii$Health_Core,
          indicators_health_kii$Nutrition_Core,
          indicators_health_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Health KII core preset applied successfully."),
          origin = iphra_txt("Health KII Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("health_kii_core", selected_health_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health KII core preset selection completed."),
          origin = iphra_txt("Health KII Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health KII Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Health KII are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_health_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health KII Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_kii(all_indicators_health_kii())
        iphra_message(
          iphra_txt("Full Health KII preset applied successfully."),
          origin = iphra_txt("Health KII Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("health_kii_full", selected_health_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Health KII preset selection completed."),
          origin = iphra_txt("Health KII Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health KII Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_health_kii is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_health_kii <- shiny::renderTable({
      sel <- selected_health_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_health_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      health_summary <- lapply(names(indicators_health_kii), function(sector) {
        inds <- indicators_health_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_health_kii[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      health_summary <- do.call(rbind, health_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(health_summary$Indicators),
        Minutes = sum(health_summary$Minutes)
      )

      rbind(health_summary, totals)
    })



    # ---- Indicators definition (lives inside module) ----
    indicators_health_obs <- list(
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
    indicator_times_health_obs <- setNames(
      rep(5, length(unlist(indicators_health_obs))),
      unlist(indicators_health_obs)
    )

    all_indicators_health_obs_static <- unlist(indicators_health_obs, use.names = FALSE)

    output$health_obs_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_obs_health_facility_iphra_v2", session)
    })
    shiny::outputOptions(output, "health_obs_present", suspendWhenHidden = FALSE)

    all_indicators_health_obs <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_health_obs_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_health_obs <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_health_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_health_obs(), selected_health_obs()),
        input_id = ns("available_health_obs"),
        options = sortable::sortable_options(group = ns("indicators_health_obs"))
      )
    })

    # ---- UI for selected list ----
    output$selected_health_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_health_obs(),
        input_id = ns("selected_health_obs"),
        options = sortable::sortable_options(group = ns("indicators_health_obs"))
      )
    })

    # ---- Keep selected_health_obs() in sync with drag-and-drop ----
    observeEvent(input$selected_health_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_health_obs)) {
          iphra_message(
            iphra_txt("No Health Observation selection detected — skipping sync update."),
            origin = iphra_txt("Health Observation Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_health_server/observeEvent_selected_health_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_obs(input$selected_health_obs)

          codes <- iphra_indicator_names_to_codes(input$selected_health_obs, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_obs_health_facility_iphra_v2",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("Health Observation selection synchronized with: "),
            paste(input$selected_health_obs, collapse = ", ")
          ),
          origin = iphra_txt("Health Observation Tool: Selection Sync")
        )

        # --- Future: sync Health Observation selection with session/project state ---
        # session$userData$project$set_selection("health_obs", input$selected_health_obs)
        }, step = "mod_tools_health_server/observeEvent_selected_health_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health Observation selection synchronization completed successfully."),
          origin = iphra_txt("Health Observation Tool: Selection Sync")
        )
        }, step = "mod_tools_health_server/observeEvent_selected_health_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health Observation Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Health Observation Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_health_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_obs(c(
          indicators_health_obs$Demographics,
          indicators_health_obs$FSL_Core,
          indicators_health_obs$WASH_Core,
          indicators_health_obs$Health_Core,
          indicators_health_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Health Observation objectives preset applied successfully."),
          origin = iphra_txt("Health Observation Tool: Preset Objectives")
        )

        # --- Future: store preset selection in session/project state ---
        # session$userData$project$set_selection("health_obs_obj", selected_health_obs())
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health Observation objectives preset selection completed."),
          origin = iphra_txt("Health Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_health_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health Observation Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_health_obs object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_health_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health Observation Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_obs(c(
          indicators_health_obs$FSL_Core,
          indicators_health_obs$WASH_Core,
          indicators_health_obs$Health_Core,
          indicators_health_obs$Nutrition_Core,
          indicators_health_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Health Observation core preset applied successfully."),
          origin = iphra_txt("Health Observation Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("health_obs_core", selected_health_obs())
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Health Observation core preset selection completed."),
          origin = iphra_txt("Health Observation Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_health_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health Observation Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Health Observation are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_health_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Health Observation Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_health_obs(all_indicators_health_obs())
        iphra_message(
          iphra_txt("Full Health Observation preset applied successfully."),
          origin = iphra_txt("Health Observation Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("health_obs_full", selected_health_obs())
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Health Observation preset selection completed."),
          origin = iphra_txt("Health Observation Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_health_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Health Observation Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_health_obs is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_health_obs <- shiny::renderTable({
      sel <- selected_health_obs()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_health_obs), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      health_obs_summary <- lapply(names(indicators_health_obs), function(sector) {
        inds <- indicators_health_obs[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_health_obs[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      health_obs_summary <- do.call(rbind, health_obs_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(health_obs_summary$Indicators),
        Minutes = sum(health_obs_summary$Minutes)
      )

      rbind(health_obs_summary, totals)
    })



    # ---- Indicators definition (lives inside module) ----
    indicators_nutrition_kii <- list(
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
    indicator_times_nutrition_kii <- setNames(
      rep(5, length(unlist(indicators_nutrition_kii))),
      unlist(indicators_nutrition_kii)
    )

    all_indicators_nutrition_kii_static <- unlist(indicators_nutrition_kii, use.names = FALSE)

    output$nutrition_kii_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_kii_nutrition_service_provider_iphra_v2", session)
    })
    shiny::outputOptions(output, "nutrition_kii_present", suspendWhenHidden = FALSE)

    all_indicators_nutrition_kii <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_nutrition_kii_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_nutrition_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_nutrition_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_nutrition_kii(), selected_nutrition_kii()),
        input_id = ns("available_nutrition_kii"),
        options = sortable::sortable_options(group = ns("indicators_nutrition_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_nutrition_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_nutrition_kii(),
        input_id = ns("selected_nutrition_kii"),
        options = sortable::sortable_options(group = ns("indicators_nutrition_kii"))
      )
    })

    # ---- Keep selected_nutrition_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_nutrition_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_nutrition_kii)) {
          iphra_message(
            iphra_txt("No Nutrition KII selection detected — skipping sync update."),
            origin = iphra_txt("Nutrition KII Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_health_server/observeEvent_selected_nutrition_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_nutrition_kii(input$selected_nutrition_kii)

          codes <- iphra_indicator_names_to_codes(input$selected_nutrition_kii, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_kii_nutrition_service_provider_iphra_v2",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("Nutrition KII selection synchronized with: "),
            paste(input$selected_nutrition_kii, collapse = ", ")
          ),
          origin = iphra_txt("Nutrition KII Tool: Selection Sync")
        )

        # --- Future: sync Nutrition KII selection with session/project state ---
        # session$userData$project$set_selection("nutrition_kii", input$selected_nutrition_kii)
        }, step = "mod_tools_health_server/observeEvent_selected_nutrition_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Nutrition KII selection synchronization completed successfully."),
          origin = iphra_txt("Nutrition KII Tool: Selection Sync")
        )
        }, step = "mod_tools_health_server/observeEvent_selected_nutrition_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Nutrition KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Nutrition KII Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_nutrition_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Nutrition KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_nutrition_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_nutrition_kii(c(
          indicators_nutrition_kii$Demographics,
          indicators_nutrition_kii$FSL_Core,
          indicators_nutrition_kii$WASH_Core,
          indicators_nutrition_kii$Health_Core,
          indicators_nutrition_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Nutrition KII objectives preset applied successfully."),
          origin = iphra_txt("Nutrition KII Tool: Preset Objectives")
        )

        # --- Future: store preset selection in session/project state ---
        # session$userData$project$set_selection("nutrition_kii_obj", selected_nutrition_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_obj_nutrition_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Nutrition KII objectives preset selection completed."),
          origin = iphra_txt("Nutrition KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_obj_nutrition_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Nutrition KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_nutrition_kii object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_nutrition_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Nutrition KII Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_nutrition_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_nutrition_kii(c(
          indicators_nutrition_kii$FSL_Core,
          indicators_nutrition_kii$WASH_Core,
          indicators_nutrition_kii$Health_Core,
          indicators_nutrition_kii$Nutrition_Core,
          indicators_nutrition_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Nutrition KII core preset applied successfully."),
          origin = iphra_txt("Nutrition KII Tool: Preset Core")
        )

        # --- Future: sync core preset with session/project state ---
        # session$userData$project$set_selection("nutrition_kii_core", selected_nutrition_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_core_nutrition_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Nutrition KII core preset selection completed."),
          origin = iphra_txt("Nutrition KII Tool: Preset Core")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_core_nutrition_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Nutrition KII Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Nutrition KII are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_nutrition_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Nutrition KII Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_nutrition_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_nutrition_kii(all_indicators_nutrition_kii())
        iphra_message(
          iphra_txt("Full Nutrition KII preset applied successfully."),
          origin = iphra_txt("Nutrition KII Tool: Preset Full")
        )

        # --- Future: sync full preset selection with session/project state ---
        # session$userData$project$set_selection("nutrition_kii_full", selected_nutrition_kii())
        }, step = "mod_tools_health_server/observeEvent_preset_full_nutrition_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Nutrition KII preset selection completed."),
          origin = iphra_txt("Nutrition KII Tool: Preset Full")
        )
        }, step = "mod_tools_health_server/observeEvent_preset_full_nutrition_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Nutrition KII Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_nutrition_kii is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_nutrition_kii <- shiny::renderTable({
      sel <- selected_nutrition_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_nutrition_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      nutrition_summary <- lapply(names(indicators_nutrition_kii), function(sector) {
        inds <- indicators_nutrition_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_nutrition_kii[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      nutrition_summary <- do.call(rbind, nutrition_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(nutrition_summary$Indicators),
        Minutes = sum(nutrition_summary$Minutes)
      )

      rbind(nutrition_summary, totals)
    })



  })
}

## To be copied in the UI
# mod_tools_health_ui("tools_health_1")

## To be copied in the server
# mod_tools_health_server("tools_health_1")
