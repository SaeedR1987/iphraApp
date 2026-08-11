#' tools_fsl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_fsl_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::conditionalPanel(
      condition = "output.fsl_kii_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "FSL Service Provider Key Informant Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_fsl_kii"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_fsl_kii"), "Core FSL Service Provider KII"),
          shiny::actionButton(ns("preset_full_fsl_kii"), "Full FSL Service Provider KII"),
          shiny::actionButton(ns("export_tool_kobo_fsl_kii"), "Export FSL Service Provider KII Kobo Tool", class = "btn-success"),
          shiny::actionButton(ns("export_tool_paper_fsl_kii"), "Export FSL Service Provider KII Paper Tool", class = "btn-success"),

        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_fsl_kii_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_fsl_kii_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_fsl_kii"))
          )
        )
      )
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.markets_kii_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Market Vendor KII Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_markets_kii"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_markets_kii"), "Core Market Vendor KII Tool"),
          shiny::actionButton(ns("preset_full_markets_kii"), "Full Market Vendor KII Tool"),
          shiny::actionButton(ns("export_tool_kobo_markets_kii"), "Export Market Vendor KII Kobo Tool", class = "btn-success"),
          shiny::actionButton(ns("export_tool_paper_markets_kii"), "Export Market Vendor KII Paper Tool", class = "btn-success")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_markets_kii_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_markets_kii_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_markets_kii"))
          )
        )
      )
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.livelihoods_obs_present == true",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Livelihoods Observation Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_livelihoods_obs"), "Match Objectives"),
          shiny::actionButton(ns("preset_core_livelihoods_obs"), "Core Livelihoods Observation Tool"),
          shiny::actionButton(ns("preset_full_livelihoods_obs"), "Full Livelihoods Observation Tool"),
          shiny::actionButton(ns("export_tool_kobo_livelihoods_obs"), "Export Livelihoods Observation Kobo Tool", class = "btn-success"),
          shiny::actionButton(ns("export_tool_paper_livelihoods_obs"), "Export Livelihoods Observation Paper Tool", class = "btn-success")


        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("available_livelihoods_obs_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("selected_livelihoods_obs_ui"))
        ),
        shiny::column(
          4,
          shinydashboard::box(
            title = "Summary of Selected Indicators",
            width = 12,
            shiny::tableOutput(ns("summary_table_livelihoods_obs"))
          )
        )
      )
    )

  )
}

#' tools_fsl Server Functions
#'
#' @noRd
mod_tools_fsl_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Indicators definition (lives inside module) ----
    indicators_fsl_kii <- list(
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
    indicator_times_fsl_kii <- setNames(
      rep(5, length(unlist(indicators_fsl_kii))),
      unlist(indicators_fsl_kii)
    )

    all_indicators_fsl_kii_static <- unlist(indicators_fsl_kii, use.names = FALSE)

    # ---- Tool presence flag ----
    output$fsl_kii_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_kii_fsl_service_provider_iphra_v2", session)
    })
    shiny::outputOptions(output, "fsl_kii_present", suspendWhenHidden = FALSE)

    # ---- Reactive available indicators sourced from master_indicator_bank ----
    all_indicators_fsl_kii <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_fsl_kii_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_fsl_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_fsl_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_fsl_kii(), selected_fsl_kii()),
        input_id = ns("available_fsl_kii"),
        options = sortable::sortable_options(group = ns("indicators_fsl_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_fsl_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_fsl_kii(),
        input_id = ns("selected_fsl_kii"),
        options = sortable::sortable_options(group = ns("indicators_fsl_kii"))
      )
    })

    # ---- Keep selected_fsl_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_fsl_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_fsl_kii)) {
          iphra_message(
            iphra_txt("No FSL KII selection detected — skipping sync update."),
            origin = iphra_txt("FSL KII Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_fsl_server/observeEvent_selected_fsl_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_fsl_kii(input$selected_fsl_kii)

          # ---- Sync selection to IPHRAProtocol ----
          codes <- iphra_indicator_names_to_codes(input$selected_fsl_kii, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_kii_fsl_service_provider_iphra_v2",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("FSL KII selection synchronized with: "),
            paste(input$selected_fsl_kii, collapse = ", ")
          ),
          origin = iphra_txt("FSL KII Tool: Selection Sync")
        )

        # --- Future: update project/session state with new FSL KII selection ---
        # session$userData$project$set_selection("fsl_kii", input$selected_fsl_kii)
        }, step = "mod_tools_fsl_server/observeEvent_selected_fsl_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("FSL KII selection synchronization completed successfully."),
          origin = iphra_txt("FSL KII Tool: Selection Sync")
        )
        }, step = "mod_tools_fsl_server/observeEvent_selected_fsl_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("FSL KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for FSL KII Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_fsl_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("FSL KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_fsl_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_fsl_kii(c(
          indicators_fsl_kii$Demographics,
          indicators_fsl_kii$FSL_Core,
          indicators_fsl_kii$WASH_Core,
          indicators_fsl_kii$Health_Core,
          indicators_fsl_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("FSL KII objectives preset applied successfully."),
          origin = iphra_txt("FSL KII Tool: Preset Objectives")
        )

        # --- Future: update session project with preset objectives selection ---
        # session$userData$project$set_selection("fsl_kii_obj", selected_fsl_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_fsl_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("FSL KII objectives preset selection completed."),
          origin = iphra_txt("FSL KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_fsl_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("FSL KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_fsl_kii object is correctly defined.")
      )
    })

    # ---- Preset: Core ----
    observeEvent(input$preset_core_fsl_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("FSL KII Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_fsl_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_fsl_kii(c(
          indicators_fsl_kii$FSL_Core,
          indicators_fsl_kii$WASH_Core,
          indicators_fsl_kii$Health_Core,
          indicators_fsl_kii$Nutrition_Core,
          indicators_fsl_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("FSL KII core preset applied successfully."),
          origin = iphra_txt("FSL KII Tool: Preset Core")
        )

        # --- Future: propagate FSL KII Core preset to project/session ---
        # session$userData$project$set_selection("fsl_kii_core", selected_fsl_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_fsl_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("FSL KII core preset selection completed."),
          origin = iphra_txt("FSL KII Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_fsl_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("FSL KII Tool: Preset Core"),
      hint = iphra_txt("Check that indicators_fsl_kii core components exist and are populated.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_fsl_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("FSL KII Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_fsl_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_fsl_kii(all_indicators_fsl_kii())
        iphra_message(
          iphra_txt("Full FSL KII preset applied successfully."),
          origin = iphra_txt("FSL KII Tool: Preset Full")
        )

        # --- Future: sync full FSL KII selection with session/project ---
        # session$userData$project$set_selection("fsl_kii_full", selected_fsl_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_fsl_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full FSL KII preset selection completed."),
          origin = iphra_txt("FSL KII Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_fsl_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("FSL KII Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_fsl_kii is defined and accessible.")
      )
    })

    # ---- Summary table ----
    output$summary_table_fsl_kii <- shiny::renderTable({
      sel <- selected_fsl_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_fsl_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      fsl_summary <- lapply(names(indicators_fsl_kii), function(sector) {
        inds <- indicators_fsl_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_fsl_kii[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      fsl_summary <- do.call(rbind, fsl_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(fsl_summary$Indicators),
        Minutes = sum(fsl_summary$Minutes)
      )

      rbind(fsl_summary, totals)
    })



    # ---- Indicators definition (lives inside module) ----
    indicators_markets_kii <- list(
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
    indicator_times_markets_kii <- setNames(
      rep(5, length(unlist(indicators_markets_kii))),
      unlist(indicators_markets_kii)
    )

    all_indicators_markets_kii_static <- unlist(indicators_markets_kii, use.names = FALSE)

    output$markets_kii_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_kii_markets_iphra_v2", session)
    })
    shiny::outputOptions(output, "markets_kii_present", suspendWhenHidden = FALSE)

    all_indicators_markets_kii <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_markets_kii_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_markets_kii <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_markets_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_markets_kii(), selected_markets_kii()),
        input_id = ns("available_markets_kii"),
        options = sortable::sortable_options(group = ns("indicators_markets_kii"))
      )
    })

    # ---- UI for selected list ----
    output$selected_markets_kii_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_markets_kii(),
        input_id = ns("selected_markets_kii"),
        options = sortable::sortable_options(group = ns("indicators_markets_kii"))
      )
    })

    # ---- Keep selected_markets_kii() in sync with drag-and-drop ----
    observeEvent(input$selected_markets_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_markets_kii)) {
          iphra_message(
            iphra_txt("No Markets KII selection detected — skipping sync update."),
            origin = iphra_txt("Markets KII Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_fsl_server/observeEvent_selected_markets_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_markets_kii(input$selected_markets_kii)

          codes <- iphra_indicator_names_to_codes(input$selected_markets_kii, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_kii_markets_iphra_v2",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("Markets KII selection synchronized with: "),
            paste(input$selected_markets_kii, collapse = ", ")
          ),
          origin = iphra_txt("Markets KII Tool: Selection Sync")
        )

        # --- Future: sync Markets KII selection with session/project state ---
        # session$userData$project$set_selection("markets_kii", input$selected_markets_kii)
        }, step = "mod_tools_fsl_server/observeEvent_selected_markets_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Markets KII selection synchronization completed successfully."),
          origin = iphra_txt("Markets KII Tool: Selection Sync")
        )
        }, step = "mod_tools_fsl_server/observeEvent_selected_markets_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Markets KII Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Markets KII Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_markets_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Markets KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_markets_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_markets_kii(c(
          indicators_markets_kii$Demographics,
          indicators_markets_kii$FSL_Core,
          indicators_markets_kii$WASH_Core,
          indicators_markets_kii$Health_Core,
          indicators_markets_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Markets KII objectives preset applied successfully."),
          origin = iphra_txt("Markets KII Tool: Preset Objectives")
        )

        # --- Future: save Markets KII objectives preset to session/project ---
        # session$userData$project$set_selection("markets_kii_obj", selected_markets_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_markets_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Markets KII objectives preset selection completed."),
          origin = iphra_txt("Markets KII Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_markets_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Markets KII Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_markets_kii object is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_markets_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Markets KII Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_markets_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_markets_kii(c(
          indicators_markets_kii$FSL_Core,
          indicators_markets_kii$WASH_Core,
          indicators_markets_kii$Health_Core,
          indicators_markets_kii$Nutrition_Core,
          indicators_markets_kii$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Markets KII core preset applied successfully."),
          origin = iphra_txt("Markets KII Tool: Preset Core")
        )

        # --- Future: store Core preset selection in session/project state ---
        # session$userData$project$set_selection("markets_kii_core", selected_markets_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_markets_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Markets KII core preset selection completed."),
          origin = iphra_txt("Markets KII Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_markets_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Markets KII Tool: Preset Core"),
      hint = iphra_txt("Check that indicators_markets_kii core categories exist.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_markets_kii, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Markets KII Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_markets_kii/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_markets_kii(all_indicators_markets_kii())
        iphra_message(
          iphra_txt("Full Markets KII preset applied successfully."),
          origin = iphra_txt("Markets KII Tool: Preset Full")
        )

        # --- Future: sync Full preset selection with session/project state ---
        # session$userData$project$set_selection("markets_kii_full", selected_markets_kii())
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_markets_kii/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Markets KII preset selection completed."),
          origin = iphra_txt("Markets KII Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_markets_kii/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Markets KII Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_markets_kii is accessible and defined.")
      )
    })

    # ---- Summary table ----
    output$summary_table_markets_kii <- shiny::renderTable({
      sel <- selected_markets_kii()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_markets_kii), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      markets_summary <- lapply(names(indicators_markets_kii), function(sector) {
        inds <- indicators_markets_kii[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_markets_kii[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      markets_summary <- do.call(rbind, markets_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(markets_summary$Indicators),
        Minutes = sum(markets_summary$Minutes)
      )

      rbind(markets_summary, totals)
    })


    # ---- Indicators definition (lives inside module) ----
    indicators_livelihoods_obs <- list(
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
    indicator_times_livelihoods_obs <- setNames(
      rep(5, length(unlist(indicators_livelihoods_obs))),
      unlist(indicators_livelihoods_obs)
    )

    all_indicators_livelihoods_obs_static <- unlist(indicators_livelihoods_obs, use.names = FALSE)

    output$livelihoods_obs_present <- shiny::reactive({
      iphra_has_protocol_tool("tool_obs_crop_livestock_iphra_v1", session)
    })
    shiny::outputOptions(output, "livelihoods_obs_present", suspendWhenHidden = FALSE)

    all_indicators_livelihoods_obs <- shiny::reactive({
      if (!is.null(session$userData$indicator_bank_version)) {
        session$userData$indicator_bank_version()
      }
      bank <- iphra_get_indicator_bank(session)
      if (nrow(bank) == 0) return(all_indicators_livelihoods_obs_static)
      bank$indicator_name
    })

    # ---- Reactive state ----
    selected_livelihoods_obs <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_livelihoods_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators_livelihoods_obs(), selected_livelihoods_obs()),
        input_id = ns("available_livelihoods_obs"),
        options = sortable::sortable_options(group = ns("indicators_livelihoods_obs"))
      )
    })

    # ---- UI for selected list ----
    output$selected_livelihoods_obs_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected_livelihoods_obs(),
        input_id = ns("selected_livelihoods_obs"),
        options = sortable::sortable_options(group = ns("indicators_livelihoods_obs"))
      )
    })

    # ---- Keep selected_livelihoods_obs() in sync with drag-and-drop ----
    observeEvent(input$selected_livelihoods_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$selected_livelihoods_obs)) {
          iphra_message(
            iphra_txt("No Livelihoods Observation selection detected — skipping sync update."),
            origin = iphra_txt("Livelihoods Observation Tool: Selection Sync")
          )
          return(NULL)
        }
        }, step = "mod_tools_fsl_server/observeEvent_selected_livelihoods_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_livelihoods_obs(input$selected_livelihoods_obs)

          codes <- iphra_indicator_names_to_codes(input$selected_livelihoods_obs, session)
          iphra_modify_indicator_bank(codes, session)
          iphra_filter_tool_survey(
            tool_name       = "tool_obs_crop_livestock_iphra_v1",
            indicator_codes = codes,
            session         = session
          )
        iphra_message(
          paste0(
            iphra_txt("Livelihoods Observation selection synchronized with: "),
            paste(input$selected_livelihoods_obs, collapse = ", ")
          ),
          origin = iphra_txt("Livelihoods Observation Tool: Selection Sync")
        )

        # --- Future: sync Livelihoods Observation selection with session/project state ---
        # session$userData$project$set_selection("livelihoods_obs", input$selected_livelihoods_obs)
        }, step = "mod_tools_fsl_server/observeEvent_selected_livelihoods_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Livelihoods Observation selection synchronization completed successfully."),
          origin = iphra_txt("Livelihoods Observation Tool: Selection Sync")
        )
        }, step = "mod_tools_fsl_server/observeEvent_selected_livelihoods_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Livelihoods Observation Tool: Selection Sync"),
      hint = iphra_txt("Ensure drag-and-drop input for Livelihoods Observation Tool is properly bound.")
      )
    })


    # ---- Preset: Objectives ----
    observeEvent(input$preset_obj_livelihoods_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_livelihoods_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_livelihoods_obs(c(
          indicators_livelihoods_obs$Demographics,
          indicators_livelihoods_obs$FSL_Core,
          indicators_livelihoods_obs$WASH_Core,
          indicators_livelihoods_obs$Health_Core,
          indicators_livelihoods_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Livelihoods Observation objectives preset applied successfully."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Objectives")
        )

        # --- Future: store preset selection in project/session object ---
        # session$userData$project$set_selection("livelihoods_obs_obj", selected_livelihoods_obs())
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_livelihoods_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Livelihoods Observation objectives preset selection completed."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Objectives")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_obj_livelihoods_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Livelihoods Observation Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_livelihoods_obs is properly defined.")
      )
    })


    # ---- Preset: Core ----
    observeEvent(input$preset_core_livelihoods_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_livelihoods_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_livelihoods_obs(c(
          indicators_livelihoods_obs$FSL_Core,
          indicators_livelihoods_obs$WASH_Core,
          indicators_livelihoods_obs$Health_Core,
          indicators_livelihoods_obs$Nutrition_Core,
          indicators_livelihoods_obs$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Livelihoods Observation core preset applied successfully."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Core")
        )

        # --- Future: sync Core preset with project/session object ---
        # session$userData$project$set_selection("livelihoods_obs_core", selected_livelihoods_obs())
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_livelihoods_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Livelihoods Observation core preset selection completed."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Core")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_core_livelihoods_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Livelihoods Observation Tool: Preset Core"),
      hint = iphra_txt("Ensure core indicator categories for Livelihoods Observation are defined.")
      )
    })


    # ---- Preset: Full ----
    observeEvent(input$preset_full_livelihoods_obs, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_livelihoods_obs/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_livelihoods_obs(all_indicators_livelihoods_obs())
        iphra_message(
          iphra_txt("Full Livelihoods Observation preset applied successfully."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Full")
        )

        # --- Future: sync Full preset with project/session object ---
        # session$userData$project$set_selection("livelihoods_obs_full", selected_livelihoods_obs())
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_livelihoods_obs/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Full Livelihoods Observation preset selection completed."),
          origin = iphra_txt("Livelihoods Observation Tool: Preset Full")
        )
        }, step = "mod_tools_fsl_server/observeEvent_preset_full_livelihoods_obs/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Livelihoods Observation Tool: Preset Full"),
      hint = iphra_txt("Confirm that all_indicators_livelihoods_obs is accessible and populated.")
      )
    })

    # ---- Summary table ----
    output$summary_table_livelihoods_obs <- shiny::renderTable({
      sel <- selected_livelihoods_obs()
      if (length(sel) == 0) {
        return(data.frame(
          Sector = c(names(indicators_livelihoods_obs), "Total"),
          Indicators = 0,
          Minutes = 0
        ))
      }

      livelihoods_obs_summary <- lapply(names(indicators_livelihoods_obs), function(sector) {
        inds <- indicators_livelihoods_obs[[sector]]
        count <- sum(sel %in% inds)
        time <- sum(indicator_times_livelihoods_obs[sel[sel %in% inds]])
        data.frame(Sector = sector, Indicators = count, Minutes = time)
      })
      livelihoods_obs_summary <- do.call(rbind, livelihoods_obs_summary)

      totals <- data.frame(
        Sector = "Total",
        Indicators = sum(livelihoods_obs_summary$Indicators),
        Minutes = sum(livelihoods_obs_summary$Minutes)
      )

      rbind(livelihoods_obs_summary, totals)
    })







  })
}

## To be copied in the UI
# mod_tools_fsl_ui("tools_fsl_1")

## To be copied in the server
# mod_tools_fsl_server("tools_fsl_1")
