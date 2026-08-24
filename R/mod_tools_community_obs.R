#' tools_community UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_community_obs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::conditionalPanel(
      condition = "output.tool_present == 'true'",
      ns = ns,
      shiny::fluidRow(
        shinydashboard::box(
          title = "Community Observation Tool - Presets",
          width = 12,
          shiny::actionButton(ns("preset_obj_match"), "Match Objectives"),
          shiny::actionButton(ns("preset_core"), "Core Tool"),
          shiny::actionButton(ns("preset_full"), "Full Tool"),
          shiny::actionButton(ns("export_tool"), "Export Community Observation Tool", class = "btn-success"),
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::uiOutput(ns("sector_filter_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("pillar_filter_ui"))
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("subpillar_filter_ui"))
        ),
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
  )
}

#' tools_community Server Functions
#'
#' @noRd
mod_tools_community_obs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    protocol_r <- phr_get_module_reactive("protocol", session)

    objective_filters_r <- reactive({

      protocol_r()$framework$master_objectives_schema[
        ,
        c(
          "objective_code",
          "sector",
          "pillar",
          "sub_pillar"
        )
      ]

    })

    filtered_pillars_r <- reactive({

      req(input$sector_filter)

      objective_filters_r() |>
        dplyr::filter(sector %in% input$sector_filter)

    })

    filtered_subpillars_r <- reactive({

      req(input$pillar_filter)

      filtered_pillars_r() |>
        dplyr::filter(pillar %in% input$pillar_filter)

    })

    filtered_available_indicators <- reactive({

      req(input$subpillar_filter)

      selected_objectives <-
        filtered_subpillars_r() |>
        dplyr::filter(
          sub_pillar %in% input$subpillar_filter
        ) |>
        dplyr::pull(objective_code) |>
        unique()

      all_indicators() |>
        dplyr::filter(
          objective_code %in% selected_objectives
        )

    })

    #OUTPUTS ####

    # ---- Tool presence flag for conditional UI ----

    output$tool_present <- shiny::renderText({
      if (isTRUE(protocol_r()$.tool_community_observation)) "true" else "false"
    })

    shiny::outputOptions(output, "tool_present", suspendWhenHidden = FALSE)

    all_indicators <- shiny::reactive({

      req(!is.null(protocol_r()$framework$modified_objectives_schema))
      req(!is.null(protocol_r()$framework$modified_indicator_bank))

      objs <- protocol_r()$framework$modified_objectives_schema[
        , c("sector", "pillar", "sub_pillar", "objective_code")
      ]

      bank <- protocol_r()$framework$modified_indicator_bank[
        protocol_r()$framework$modified_indicator_bank$tool == "obs_community",
        c("objective_code", "indicator_code", "indicator_name")
      ]

      dplyr::left_join(bank, objs, by = "objective_code")

    })

    selected <- shiny::reactiveVal(character(0))

    selected_indicators <- shiny::reactive({
      selected(as.character(input$selected))
      inds <- all_indicators()
      sel  <- selected()

      inds[inds$indicator_name %in% sel, ]
    })

    output$sector_filter_ui <- renderUI({

      selectInput(
        ns("sector_filter"),
        "Sector",
        choices = sort(unique(objective_filters_r()$sector)),
        selected = isolate(input$sector_filter),
        multiple = TRUE
      )

    })

    output$pillar_filter_ui <- renderUI({

      selectInput(
        ns("pillar_filter"),
        "Pillar",
        choices = sort(unique(filtered_pillars_r()$pillar)),
        selected = isolate(input$pillar_filter),
        multiple = TRUE
      )

    })

    output$subpillar_filter_ui <- renderUI({

      selectInput(
        ns("subpillar_filter"),
        "Sub-Pillar",
        choices = sort(unique(filtered_subpillars_r()$sub_pillar)),
        selected = isolate(input$subpillar_filter),
        multiple = TRUE
      )

    })


    # ---- UI for available list ----
    output$available_ui <- shiny::renderUI({

      labels <- filtered_available_indicators()

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

    output$summary_table <- shiny::renderTable({

      sel_df <- selected_indicators()

      empty <- data.frame(
        Indicator = character(0),
        Minutes   = numeric(0),
        stringsAsFactors = FALSE
      )

      if (nrow(sel_df) == 0) return(empty)

      survey <- protocol_r()$tools$tool_obs_community_iphra_v2$revised_survey
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

      sel_df <- sel_df[!duplicated(sel_df$indicator_code), ]

      per_indicator <- data.frame(
        Indicator = sel_df$indicator_name,
        Minutes = as.numeric(
          seconds_by_code[as.character(sel_df$indicator_code)]
        ) / 60,
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

    # ---- Restore selected indicators when a project file is loaded ----
    observeEvent(session$userData$flags$project_loaded, {
      req(isolate(session$userData$flags$project_loaded) > 0)

      proto <- isolate(protocol_r())
      tool  <- proto$tools[["tool_obs_community_iphra_v2"]]
      if (is.null(tool)) return()

      codes <- tryCatch(
        as.character(tool$get_indicator_codes(prefer_revised = TRUE)),
        error = function(e) character(0)
      )
      codes <- codes[nchar(codes) > 0 & codes != "10000" & !grepl("00$", codes)]
      if (length(codes) == 0) return()

      bank <- isolate(all_indicators())
      selected(as.character(bank$indicator_name[bank$indicator_code %in% codes]))
    }, ignoreInit = TRUE)

    # ---- Keep Tool in sync with selected
    observeEvent(input$selected, {
      iphra_try({

        selected(as.character(input$selected))

        indicators_selected <- selected_indicators()

        if (is.null(indicators_selected) || nrow(indicators_selected) == 0) {
          return(NULL)
        }

        protocol_r()$tools$tool_obs_community_iphra_v2$filter_survey_by_indicator(indicator_codes = indicators_selected$indicator_code)

        phr_touch_module("protocol")

        iphra_message(
          paste0(
            iphra_txt("Selection synchronized with: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = iphra_txt("Community Obs Tool: Selection Sync")
        )
      },
      on_error = "warn",
      origin = iphra_txt("Community Obs Tool: Selection Sync"),
      hint = iphra_txt("Ensure the drag-and-drop or selection input is properly bound.")
      )
    })
    # ---- Presets (KII)

    # Preset: Objectives
    observeEvent(input$preset_obj_match, {
      iphra_try({

        result <- iphra_try_step({

          selected_kii(c(
            indicators_kii$Demographics,
            indicators_kii$FSL_Core,
            indicators_kii$WASH_Core,
            indicators_kii$Health_Core,
            indicators_kii$Shelter_Core
          ))
          iphra_message(
            iphra_txt("KII objectives preset applied successfully."),
            origin = iphra_txt("Community Obs Tool: Preset Objectives")
          )

          # --- Future: update project session state for KII objectives ---
          # session$userData$project$set_selection("kii", selected_kii())


        }, step = "mod_tools_community_server/observeEvent_preset_obj_kii/Core Logic")
        if (iphra_failed(result)) return(result)


        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS

        result <- iphra_try_step({

          iphra_message(
            iphra_txt("Community Obs objectives preset selection completed."),
            origin = iphra_txt("Community Obs Tool: Preset Objectives")
          )
        }, step = "mod_tools_community_obs_server/observeEvent_preset_obj_kii/Result Handling")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Community Obs Tool: Preset Objectives"),
      hint = iphra_txt("Verify that indicators_kii object is correctly defined and accessible.")
      )
    })

    # Preset: Core
    observeEvent(input$preset_core, {
      iphra_try({

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
        }, step = "mod_tools_community_obs_server/observeEvent_preset_core/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Community Observation Tool: Preset Core"),
      hint = iphra_txt("Check that indicators_kii core components exist and are populated.")
      )
    })


    # Preset: Full
    observeEvent(input$preset_full, {
      iphra_try({

        result <- iphra_try_step({
          selected_kii(all_indicators_kii())
          iphra_message(
            iphra_txt("Full Community Obs preset applied successfully."),
            origin = iphra_txt("Community Obs Tool: Preset Full")
          )

          # --- Future: sync with project session-level KII data ---
          # session$userData$project$set_selection("kii_full", selected_kii())
        }, step = "mod_tools_community_obs_server/observeEvent_preset_full_community_obs/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Community Obs Tool: Preset Full"),
      hint = iphra_txt("Ensure all_indicators_kii object is properly defined in the environment.")
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
        if (!isTRUE(protocol_r()$.tool_community_observation)) {
          shiny::showModal(shiny::modalDialog(
            title = iphra_txt("Export Community Obs Tool"),
            iphra_txt("The Community Observation tool has not been added to the protocol yet. Please add it from the Tool Design page before exporting."),
            footer = shiny::modalButton(iphra_txt("Close")),
            easyClose = TRUE
          ))
          return(NULL)
        }

        shiny::showModal(shiny::modalDialog(
          title = iphra_txt("Export Community Obs Tool"),
          shiny::tagList(
            shiny::p(iphra_txt("Click below to save the Community Observation tool as an Excel workbook with three sheets: revised_survey, revised_choices, and revised_settings.")),
            shiny::downloadButton(ns("download_tool"),
                                  label = iphra_txt("Download Excel"),
                                  class = "btn-success")
          ),
          footer = shiny::modalButton(iphra_txt("Cancel")),
          easyClose = TRUE
        ))
      },
      on_error = "warn",
      origin = iphra_txt("Community Obs Tool: Export"),
      hint = iphra_txt("Ensure the Community Observation tool has been added to the protocol and exposes revised_survey / revised_choices / revised_settings.")
      )
    })

    output$download_tool <- shiny::downloadHandler(
      filename = function() {
        paste0("tool_obs_community_iphra_v2",
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
          survey   = safe_df(protocol_r()$tools$tool_obs_community_iphra_v2$revised_survey),
          choices  = safe_df(protocol_r()$tools$tool_obs_community_iphra_v2$revised_choices),
          settings = safe_df(protocol_r()$tools$tool_obs_community_iphra_v2$revised_settings)
        )

        writexl::write_xlsx(sheets, path = file)

        iphra_message(
          iphra_txt("Community Key Informant tool exported to Excel."),
          origin = iphra_txt("Community KII Tool: Export")
        )
      }
    )

  })

}


## To be copied in the UI
# mod_tools_community_ui("tools_community_1")

## To be copied in the server
# mod_tools_community_server("tools_community_1")
