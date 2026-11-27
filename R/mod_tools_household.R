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
        shiny::column(4, shiny::uiOutput(ns("sector_filter_ui"))),
        shiny::column(4, shiny::uiOutput(ns("pillar_filter_ui"))),
        shiny::column(4, shiny::uiOutput(ns("subpillar_filter_ui")))
      ),
      shiny::fluidRow(
        shiny::column(
          2,
          shinydashboard::box(
            title = "Presets",
            width = 12,
            shiny::actionButton(ns("preset_obj"), "Match Objectives", class = "iphra-preset-btn"),
            shiny::actionButton(ns("preset_core"), "Core", class = "iphra-preset-btn")
          )
        ),
        shiny::column(5, shiny::uiOutput(ns("available_ui"))),
        shiny::column(5, shiny::uiOutput(ns("selected_ui")))
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

    observe({
      cat("HOUSEHOLD MODULE INITIALIZED\n")
    })

    # SETUP ####

    protocol_r <- phr_get_module_reactive("protocol", session)

    objective_filters_r <- reactive({
      protocol_r()$get(
        field = "framework",
        member = "master_objectives_schema"
      )[
        ,
        c(
          "objective_code",
          "sector",
          "pillar",
          "sub_pillar"
        )
      ]
    })

    filters <- iphra_tool_filters_server(
      input, output, session,
      protocol_r = protocol_r,
      role = "tool_household_iphra_v2",
      all_indicators_fn = function() all_indicators()
    )
    filtered_available_indicators <- filters$available

    # tool_r     <- shiny::reactive({ protocol_r()$tools$tool_household_iphra_v2 })

    #OUTPUTS ####

    # ---- Tool presence flag for conditional UI ----
    # Reads reactively via `iphra_has_protocol_tool()`, which depends on the
    # protocol module's version signal (bumped by mod_tools_master_server
    # via `phr_touch_module()` whenever a tool is added or removed), so
    # this output re-evaluates and the conditionalPanel shows/hides correctly.
    output$tool_present <- shiny::renderText({
      if (isTRUE(protocol_r()$.tool_household_iphra)) "true" else "false"
    })

    shiny::outputOptions(output, "tool_present", suspendWhenHidden = FALSE)

    # REACTIVES ####

    all_indicators <- shiny::reactive({

      req(!is.null(protocol_r()$get(field = "framework", member = "modified_objectives_schema")))
      req(!is.null(protocol_r()$get(field = "framework", member = "modified_indicator_bank")))

      objs <- protocol_r()$get(
        field = "framework",
        member = "modified_objectives_schema"
      )[
        , c("sector", "pillar", "sub_pillar", "objective_code")
      ]

      indicator_bank <- protocol_r()$get(
        field = "framework",
        member = "modified_indicator_bank"
      )

      bank <- indicator_bank[
        indicator_bank$tool == "household",
        c("objective_code", "indicator_code", "indicator_name")
      ]

      dplyr::left_join(bank, objs, by = "objective_code")

    })

    selected <- shiny::reactiveVal(character(0))

    selected_indicators <- shiny::reactive({
      inds <- all_indicators()
      sel  <- selected()

      inds[inds$indicator_name %in% sel, ]
    })

    sector_selected <- reactiveVal(character())
    pillar_selected <- reactiveVal(character())
    subpillar_selected <- reactiveVal(character())

    observeEvent(input$sector_filter, {
      sector_selected(input$sector_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "selected_sectors",
                       value = input$sector_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "available_sectors",
                       value = sort(unique(objective_filters_r()$sector)))

    })

    observeEvent(input$pillar_filter, {
      pillar_selected(input$pillar_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "selected_pillars",
                       value = input$pillar_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "available_pillars",
                       value = sort(unique(objective_filters_r()$pillar)))
    })

    observeEvent(input$subpillar_filter, {
      subpillar_selected(input$subpillar_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "selected_subpillars",
                       value = input$subpillar_filter)

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "available_subpillars",
                       value = sort(unique(objective_filters_r()$sub_pillar)))
    })

    # OUTPUTS ####



    # ---- UI for available list ----
    output$available_ui <- shiny::renderUI({

      labels <- filtered_available_indicators()

      sortable::rank_list(
        text = "Available Indicators",
        labels = unique(labels$indicator_name[
          !labels$indicator_code %in% selected()
        ]),
        input_id = ns("available"),
        options = sortable::sortable_options(group = ns("indicators"))
      )
    })

    # ---- UI for selected list
    output$selected_ui <- shiny::renderUI({
      inds <- all_indicators()

      labels <- inds$indicator_name[
        match(selected(), inds$indicator_code)
      ]

      sortable::rank_list(
        text = "Selected Indicators (drag to reorder)",
        labels = labels,
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

      tool <- protocol_r()$get(field = "tools", role = "tool_household_iphra_v2")

      survey <- tool$get(field = "revised_survey")

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

    # ---- Restore filters + selected indicators when a project file is loaded ----
    observeEvent(session$userData$flags$project_loaded, {
      req(isolate(session$userData$flags$project_loaded) > 0)

      proto <- isolate(protocol_r())
      tool  <- proto$get(field = "tools", role = "tool_household_iphra_v2")
      if (is.null(tool)) return()

      # iphra_restore_tool_filters(session, tool, isolate(objective_filters_r()))

      filters$restore()

      selected(as.character(tool$get(field = "selected_indicator_codes") %||% character(0)))


    }, ignoreInit = TRUE)

    # ---- Keep Tool in sync with the filters ----
    observeEvent(input$sector_filter, {
      iphra_save_tool_field(
        protocol_r()$get(field = "tools", role = "tool_household_iphra_v2"),
        "selected_sectors", input$sector_filter
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$pillar_filter, {
      iphra_save_tool_field(
        protocol_r()$get(field = "tools", role = "tool_household_iphra_v2"),
        "selected_pillars", input$pillar_filter
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$subpillar_filter, {
      iphra_save_tool_field(
        protocol_r()$get(field = "tools", role = "tool_household_iphra_v2"),
        "selected_subpillars", input$subpillar_filter
      )
    }, ignoreNULL = FALSE)

    # ---- Keep Tool in sync with the currently available indicators ----
    observeEvent(filtered_available_indicators(), {

      protocol_r()$set(field = "tools",
                       role = "tool_household_iphra_v2",
                       member = "available_indicator_codes",
                       value = unique(filtered_available_indicators()$indicator_code)
                       )

    }, ignoreNULL = FALSE)

    # ---- Keep Tool in sync with selected
    observeEvent(input$selected, {
      phrutils::phr_try({

        inds <- all_indicators()

        codes <- inds$indicator_code[
          match(as.character(input$selected), inds$indicator_name)
        ]

        codes <- as.character(codes[!is.na(codes)])

        selected(codes)

        tool <- protocol_r()$get(field = "tools", role = "tool_household_iphra_v2")

        tool$set(field = "selected_indicator_codes", value = codes)

        indicators_selected <- selected_indicators()

        if (is.null(indicators_selected) || nrow(indicators_selected) == 0) {
          return(NULL)
        }

        tool$call(field = "filter_survey_by_indicator", indicator_codes = indicators_selected$indicator_code)

        phr_touch_module("protocol")

      },
      on_error = "warn",
      origin = phrutils::phr_txt("Household Tool: Selection Sync"),
      hint = phrutils::phr_txt("Ensure the drag-and-drop or selection input is properly bound.")
      )
    })

    # ---- Presets ----

    # Preset: Objectives
    observeEvent(input$preset_obj, {
      phrutils::phr_try({

        result <- phrutils::phr_try_step({
          selected(c(
          indicators$Demographics,
          indicators$FSL_Core,
          indicators$WASH_Core,
          indicators$Health_Core,
          indicators$Shelter_Core
        ))

        }, step = "mod_tools_household_server/observeEvent_preset_obj/Core Logic")
        if (phrutils::phr_failed(result)) return(result)

      },
      on_error = "warn",
      origin = phrutils::phr_txt("Household Tool: Preset Objectives"),
      hint = phrutils::phr_txt("Verify that all indicator groups exist in the indicators object.")
      )
    })

    # Preset: Core
    observeEvent(input$preset_core, {
      phrutils::phr_try({
        selected(c(
          indicators$Demographics,
          indicators$FSL_Core,
          indicators$WASH_Core,
          indicators$Health_Core,
          indicators$Shelter_Core
        ))
        phrutils::phr_message(
          phrutils::phr_txt("Core preset applied successfully."),
          origin = phrutils::phr_txt("Household Tool: Preset Core")
        )
      },
      on_error = "warn",
      origin = phrutils::phr_txt("Household Tool: Preset Core"),
      hint = phrutils::phr_txt("Verify that all indicator groups exist in the indicators object.")
      )
    })

  })
}

## To be copied in the UI
# mod_tools_household_ui("tools_household_1")

## To be copied in the server
# mod_tools_household_server("tools_household_1")
