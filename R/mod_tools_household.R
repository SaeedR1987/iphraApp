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
      condition = "output.tool_present == 'true'",
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
    protocol  <- session$userData$modules[["protocol"]]
    tool      <- protocol$tools$tool_household_iphra_v2

    #OUTPUTS ####

    # ---- Tool presence flag for conditional UI ----
    # `.tool_household_iphra` is an active binding on the IPHRAProtocol
    # class that returns TRUE/FALSE depending on whether the household tool
    # has been added to the protocol object.
    output$tool_present <- shiny::renderText({
      if (isTRUE(protocol$.tool_household_iphra)) "true" else "false"
    })


    shiny::outputOptions(output, "tool_present", suspendWhenHidden = FALSE)

    # REACTIVES ####

    all_indicators <- shiny::reactive({
      fw <- session$userData$modules[["protocol"]]$framework
      bank <- fw$modified_indicator_bank
      bank[bank$tool == "household", c("indicator_code", "indicator_name")]
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
    # Groups the household tool's `revised_survey` by `indicator_code` and
    # reports the total time (in minutes) each selected indicator
    # contributes to the questionnaire, based on the `time_seconds` field
    # on the revised survey rows.
    output$summary_table <- shiny::renderTable({

      sel_df <- selected_indicators()

      empty <- data.frame(
        Indicator = character(0),
        Minutes   = numeric(0),
        stringsAsFactors = FALSE
      )

      if (nrow(sel_df) == 0) return(empty)

      survey <- tool$revised_survey
      if (is.null(survey) || !is.data.frame(survey) || nrow(survey) == 0 ||
          !all(c("indicator_code", "time_seconds") %in% names(survey))) {
        return(empty)
      }

      seconds_by_code <- tapply(
        as.numeric(survey$time_seconds),
        survey$indicator_code,
        sum,
        na.rm = TRUE
      )

      per_indicator <- data.frame(
        Indicator = sel_df$indicator_name,
        Minutes   = as.numeric(seconds_by_code[sel_df$indicator_code]) / 60,
        stringsAsFactors = FALSE
      )
      per_indicator$Minutes[is.na(per_indicator$Minutes)] <- 0

      totals <- data.frame(
        Indicator = "Total",
        Minutes   = sum(per_indicator$Minutes),
        stringsAsFactors = FALSE
      )

      rbind(per_indicator, totals)
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

    # Preset: Core
    observeEvent(input$preset_core, {
      iphra_try({
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
      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Preset Core"),
      hint = iphra_txt("Verify that all indicator groups exist in the indicators object.")
      )
    })

    # ---- Export Tool ----
    #
    # The Export button opens a modal that lets the user save an Excel
    # workbook containing the household tool's `revised_survey`,
    # `revised_choices` and `revised_settings` data frames as three sheets.
    # The actual file writing happens through a `downloadHandler` (which
    # is what shows the browser's native "save as" dialog); the modal is
    # only used to surface that download link because the UI-side control
    # is an `actionButton`, not a `downloadButton`.
    observeEvent(input$export_tool, {
      iphra_try({
        if (!isTRUE(protocol$.tool_household_iphra)) {
          shiny::showModal(shiny::modalDialog(
            title = iphra_txt("Export Household Tool"),
            iphra_txt("The Household tool has not been added to the protocol yet. Please add it from the Tool Design page before exporting."),
            footer = shiny::modalButton(iphra_txt("Close")),
            easyClose = TRUE
          ))
          return(NULL)
        }

        shiny::showModal(shiny::modalDialog(
          title = iphra_txt("Export Household Tool"),
          shiny::tagList(
            shiny::p(iphra_txt("Click below to save the Household tool as an Excel workbook with three sheets: revised_survey, revised_choices, and revised_settings.")),
            shiny::downloadButton(ns("download_tool"),
                                  label = iphra_txt("Download Excel"),
                                  class = "btn-success")
          ),
          footer = shiny::modalButton(iphra_txt("Cancel")),
          easyClose = TRUE
        ))
      },
      on_error = "warn",
      origin = iphra_txt("Household Tool: Export"),
      hint = iphra_txt("Ensure the Household tool has been added to the protocol and exposes revised_survey / revised_choices / revised_settings.")
      )
    })

    output$download_tool <- shiny::downloadHandler(
      filename = function() {
        paste0("tool_household_iphra_v2_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        safe_df <- function(x) {
          tryCatch({
            if (is.null(x)) return(data.frame())
            if (is.data.frame(x)) return(x)
            as.data.frame(x)
          }, error = function(e) data.frame())
        }

        sheets <- list(
          revised_survey   = safe_df(tool$revised_survey),
          revised_choices  = safe_df(tool$revised_choices),
          revised_settings = safe_df(tool$revised_settings)
        )

        writexl::write_xlsx(sheets, path = file)

        iphra_message(
          iphra_txt("Household tool exported to Excel."),
          origin = iphra_txt("Household Tool: Export")
        )
      }
    )

  })
}

## To be copied in the UI
# mod_tools_household_ui("tools_household_1")

## To be copied in the server
# mod_tools_household_server("tools_household_1")
