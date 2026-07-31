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
    shiny::fluidRow(

      # --- Modern Checkbox: Household Tools Complete ---
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
            id = ns("household_complete"),
            name = ns("household_complete"),
            onchange = sprintf("Shiny.setInputValue('%s', this.checked);", ns("household_complete"))
          ),
          tags$span("Household Tools Complete")
        )
      ),

      # --- Existing Indicators Preset Box ---
      shinydashboard::box(
        title = "Household Tool - Presets",
        width = 12,
        shiny::actionButton(ns("preset_obj"), "Match Objectives"),
        shiny::actionButton(ns("preset_core"), "Core"),
        shiny::actionButton(ns("preset_full"), "Extended"),
        shiny::actionButton(ns("preset_outcomes"), "Outcome Focused"),
        shiny::actionButton(ns("preset_fsl"), "FSL Focused"),
        shiny::actionButton(ns("preset_wash"), "WASH Focused"),
        shiny::actionButton(ns("preset_health"), "Health Focused"),
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
  )
}

#' tools_household Server Functions
#'
#' @noRd
mod_tools_household_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Indicators definition (lives inside module) ----
    indicators <- list(
      Demographics = c("Household size", "Age distribution"),
      Health_Core       = c("Access to care", "Illness prevalence"),
      Outcomes    = c("MUAC", "Mortality"),
      FSL_Core          = c("Market access", "Food consumption score"),
      WASH_Core         = c("Water source", "Latrine access"),
      Shelter_Core      = c("Shelter type", "Overcrowding")
    )

    # Estimated times per indicator (mins)
    indicator_times <- setNames(
      rep(5, length(unlist(indicators))),
      unlist(indicators)
    )

    all_indicators <- unlist(indicators, use.names = FALSE)

    # ---- Reactive state ----
    selected <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----
    output$available_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Available Indicators",
        labels = setdiff(all_indicators, selected()),
        input_id = ns("available"),
        options = sortable::sortable_options(group = ns("indicators"))
      )
    })

    # ---- UI for selected list ----
    output$selected_ui <- shiny::renderUI({
      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = selected(),
        input_id = ns("selected"),
        options = sortable::sortable_options(group = ns("indicators"))
      )
    })

    # ---- Keep selected() in sync with drag-and-drop ----
    observeEvent(input$selected, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$selected)) {
          iphra_message(
            iphra_txt("No selection detected — skipping sync update."),
            origin = iphra_txt("Household Tool: Selection Sync")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_selected/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected(input$selected)
        iphra_message(
          paste0(
            iphra_txt("Selection synchronized with: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = iphra_txt("Household Tool: Selection Sync")
        )

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_selected/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        # Typically none for sync observers — they’re internal updates only.
        # Still, log completion for traceability.
        iphra_message(
          iphra_txt("Selection synchronization completed successfully."),
          origin = iphra_txt("Household Tool: Selection Sync")
        )
        }, step = "mod_tools_household_server/observeEvent_selected/Result Handling")
        if (iphra_failed(result)) return(result)

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


    # Preset: Core
    observeEvent(input$preset_core, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset Core")
        )

        selected(c(
          indicators$Demographics,
          indicators$FSL_Core,
          indicators$WASH_Core,
          indicators$Health_Core,
          indicators$Shelter_Core
        ))
        iphra_message(
          iphra_txt("Core preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset Core")
        )

        iphra_message(
          iphra_txt("Preset Core selection completed."),
          origin = iphra_txt("Household Tool: Preset Core")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Core"),
      hint = iphra_txt("Check indicator definitions for Core preset consistency.")
      )
    })


    # Preset: Full
    observeEvent(input$preset_full, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset Full")
        )

        selected(all_indicators)
        iphra_message(
          iphra_txt("Full preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset Full")
        )

        iphra_message(
          iphra_txt("Preset Full selection completed."),
          origin = iphra_txt("Household Tool: Preset Full")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Full"),
      hint = iphra_txt("Ensure all_indicators object is properly defined and accessible.")
      )
    })


    # Preset: Outcomes
    observeEvent(input$preset_outcomes, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset Outcomes")
        )

        selected(c(indicators$Demographics, indicators$Outcomes))
        iphra_message(
          iphra_txt("Outcomes preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset Outcomes")
        )

        iphra_message(
          iphra_txt("Preset Outcomes selection completed."),
          origin = iphra_txt("Household Tool: Preset Outcomes")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Outcomes"),
      hint = iphra_txt("Confirm that indicators$Outcomes exists and is populated.")
      )
    })


    # Preset: FSL
    observeEvent(input$preset_fsl, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset FSL")
        )

        selected(c(indicators$Demographics, indicators$FSL_Core))
        iphra_message(
          iphra_txt("FSL preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset FSL")
        )

        iphra_message(
          iphra_txt("Preset FSL selection completed."),
          origin = iphra_txt("Household Tool: Preset FSL")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset FSL"),
      hint = iphra_txt("Verify that indicators$FSL_Core exists in the indicators object.")
      )
    })


    # Preset: WASH
    observeEvent(input$preset_wash, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset WASH")
        )

        selected(c(indicators$Demographics, indicators$WASH_Core))
        iphra_message(
          iphra_txt("WASH preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset WASH")
        )

        iphra_message(
          iphra_txt("Preset WASH selection completed."),
          origin = iphra_txt("Household Tool: Preset WASH")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset WASH"),
      hint = iphra_txt("Verify that indicators$WASH_Core is defined and valid.")
      )
    })


    # Preset: Health
    observeEvent(input$preset_health, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Household Tool: Preset Health")
        )

        selected(c(indicators$Demographics, indicators$Health_Core))
        iphra_message(
          iphra_txt("Health preset applied successfully."),
          origin = iphra_txt("Household Tool: Preset Health")
        )

        iphra_message(
          iphra_txt("Preset Health selection completed."),
          origin = iphra_txt("Household Tool: Preset Health")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Health"),
      hint = iphra_txt("Check that indicators$Health_Core exists and contains expected variables.")
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

    # ---- Toggle: Household Tools Complete ----
    observeEvent(input$household_complete, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$household_complete)) {
          iphra_message(
            iphra_txt("Checkbox state is NULL — skipping update."),
            origin = iphra_txt("Household Tool: Completion Toggle")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_household_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (isTRUE(input$household_complete)) {
          iphra_message(
            iphra_txt("Household Tools marked as complete ✅"),
            origin = iphra_txt("Household Tool: Completion Toggle")
          )

          # --- Future logic (e.g., save completion status, update session/project) ---
          # session$userData$project$set_stage_completed("household_tools", TRUE)

        } else {
          iphra_message(
            iphra_txt("Household Tools marked as incomplete ❌"),
            origin = iphra_txt("Household Tool: Completion Toggle")
          )

          # --- Future logic (e.g., reset completion flag) ---
          # session$userData$project$set_stage_completed("household_tools", FALSE)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_household_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Household Tools completion status updated successfully."),
          origin = iphra_txt("Household Tool: Completion Toggle")
        )
        }, step = "mod_tools_household_server/observeEvent_household_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Household Tool: Completion Toggle"),
      hint = iphra_txt("Verify checkbox binding and completion state logic if this fails.")
      )
    })

    # ────────────────────────────────────────────────
    # ▶️ TOGGLE: Sample Size Complete
    # ────────────────────────────────────────────────
    observeEvent(input$sample_size_complete, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$sample_size_complete)) {
          iphra_message(
            iphra_txt("Checkbox state is NULL — skipping update."),
            origin = iphra_txt("Sample Size: Completion Toggle")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_sample_size_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (isTRUE(input$sample_size_complete)) {
          iphra_message(
            iphra_txt("Sample Size section marked as complete ✅"),
            origin = iphra_txt("Sample Size: Completion Toggle")
          )

          # --- Future logic (e.g., save completion status, update session/project) ---
          # session$userData$project$set_stage_completed("sample_size", TRUE)

        } else {
          iphra_message(
            iphra_txt("Sample Size section marked as incomplete ❌"),
            origin = iphra_txt("Sample Size: Completion Toggle")
          )

          # --- Future logic (e.g., reset completion flag) ---
          # session$userData$project$set_stage_completed("sample_size", FALSE)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_sample_size_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sample Size completion status updated successfully."),
          origin = iphra_txt("Sample Size: Completion Toggle")
        )
        }, step = "mod_tools_household_server/observeEvent_sample_size_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Size: Completion Toggle"),
      hint = iphra_txt("Verify checkbox binding and completion state logic if this fails.")
      )
    })



    # ────────────────────────────────────────────────
    # ▶️ TOGGLE: Survey Teams Complete
    # ────────────────────────────────────────────────
    observeEvent(input$survey_teams_complete, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$survey_teams_complete)) {
          iphra_message(
            iphra_txt("Checkbox state is NULL — skipping update."),
            origin = iphra_txt("Survey Teams: Completion Toggle")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_survey_teams_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (isTRUE(input$survey_teams_complete)) {
          iphra_message(
            iphra_txt("Survey Teams section marked as complete ✅"),
            origin = iphra_txt("Survey Teams: Completion Toggle")
          )

          # --- Future logic (e.g., save completion status, update session/project) ---
          # session$userData$project$set_stage_completed("survey_teams", TRUE)

        } else {
          iphra_message(
            iphra_txt("Survey Teams section marked as incomplete ❌"),
            origin = iphra_txt("Survey Teams: Completion Toggle")
          )

          # --- Future logic (e.g., reset completion flag) ---
          # session$userData$project$set_stage_completed("survey_teams", FALSE)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_survey_teams_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Survey Teams completion status updated successfully."),
          origin = iphra_txt("Survey Teams: Completion Toggle")
        )
        }, step = "mod_tools_household_server/observeEvent_survey_teams_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Survey Teams: Completion Toggle"),
      hint = iphra_txt("Verify checkbox binding and completion state logic if this fails.")
      )
    })



    # ────────────────────────────────────────────────
    # ▶️ TOGGLE: Sampling Complete
    # ────────────────────────────────────────────────
    observeEvent(input$sampling_complete, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$sampling_complete)) {
          iphra_message(
            iphra_txt("Checkbox state is NULL — skipping update."),
            origin = iphra_txt("Sampling: Completion Toggle")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_sampling_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (isTRUE(input$sampling_complete)) {
          iphra_message(
            iphra_txt("Sampling section marked as complete ✅"),
            origin = iphra_txt("Sampling: Completion Toggle")
          )

          # --- Future logic (e.g., save completion status, update session/project) ---
          # session$userData$project$set_stage_completed("sampling", TRUE)

        } else {
          iphra_message(
            iphra_txt("Sampling section marked as incomplete ❌"),
            origin = iphra_txt("Sampling: Completion Toggle")
          )

          # --- Future logic (e.g., reset completion flag) ---
          # session$userData$project$set_stage_completed("sampling", FALSE)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_tools_household_server/observeEvent_sampling_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sampling completion status updated successfully."),
          origin = iphra_txt("Sampling: Completion Toggle")
        )
        }, step = "mod_tools_household_server/observeEvent_sampling_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sampling: Completion Toggle"),
      hint = iphra_txt("Verify checkbox binding and completion state logic if this fails.")
      )
    })

  })
}

## To be copied in the UI
# mod_tools_household_ui("tools_household_1")

## To be copied in the server
# mod_tools_household_server("tools_household_1")
