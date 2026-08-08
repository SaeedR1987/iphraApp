#' goals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_goals_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      # Left side (controls in tabset)
      shiny::column(
        width = 6,
        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Primary Tab ----
          shiny::tabPanel(
            title = iphra_txt("Primary"),
            shinydashboard::box(
              title = iphra_txt("Objectives Presets"),
              width = 12,
              shiny::actionButton(ns("preset_core"), iphra_txt("Core")),
              shiny::actionButton(ns("preset_full"), iphra_txt("Extended")),
              shiny::actionButton(ns("preset_outcomes"), iphra_txt("Outcome Focused")),
              shiny::actionButton(ns("preset_fsl"), iphra_txt("FSL Focused")),
              shiny::actionButton(ns("preset_wash"), iphra_txt("WASH Focused")),
              shiny::actionButton(ns("preset_health"), iphra_txt("Health Focused")),
              shiny::actionButton(
                ns("clear_objectives"),
                iphra_txt("Clear Objectives"),
                style = "margin-left:10px; background-color:#f88; color:white;"
              )
            ),
            shiny::uiOutput(ns("dynamic_select_ui")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::uiOutput(ns("available_ui"))),
              shiny::column(width = 6, shiny::uiOutput(ns("selected_ui")))
            )
          ),

          # ---- Secondary Tab ----
          shiny::tabPanel(
            title = iphra_txt("Secondary"),
            shinydashboard::box(
              title = iphra_txt("Objectives Presets"),
              width = 12,
              shiny::actionButton(ns("preset_sdr_core"), iphra_txt("Core")),
              shiny::actionButton(ns("preset_sdr_full"), iphra_txt("Extended")),
              shiny::actionButton(ns("preset_sdr_outcomes"), iphra_txt("Outcome Focused")),
              shiny::actionButton(ns("preset_sdr_fsl"), iphra_txt("FSL Focused")),
              shiny::actionButton(ns("preset_sdr_wash"), iphra_txt("WASH Focused")),
              shiny::actionButton(ns("preset_sdr_health"), iphra_txt("Health Focused")),
              shiny::actionButton(
                ns("clear_sdr_objectives"),
                iphra_txt("Clear Objectives"),
                style = "margin-left:10px; background-color:#f88; color:white;"
              )
            ),
            shiny::uiOutput(ns("dynamic_select_sdr_ui")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::uiOutput(ns("available_sdr_ui"))),
              shiny::column(width = 6, shiny::uiOutput(ns("selected_sdr_ui")))
            )
          ),

          # ---- Full Objectives Tab ----
          shiny::tabPanel(
            title = iphra_txt("Full Objectives"),
            shiny::h4(iphra_txt("Goal Statements")),
            shiny::p(iphra_txt("1. To understand the severity of public health needs in the target population.")),
            shiny::p(iphra_txt("2. To identify initial public health priorities and service gaps for response.")),
            shiny::hr(),

            # Added modern-styled checkbox
            shiny::div(
              style = "
    display: flex;
    align-items: center;
    justify-content: left;
    padding: 10px 14px;             /* ↓ reduced vertical padding */
    border: 1px solid #ccc;
    border-radius: 6px;
    background-color: #f8f9fa;
    margin-bottom: 15px;
    width: fit-content;
    line-height: 1.2em;            /* ↓ tighter text alignment */
  ",
              tags$label(
                class = "checkbox-inline",
                style = "margin: 0; font-weight: 600; font-size: 14px; display: flex; align-items: center;",
                shiny::tags$input(
                  type = "checkbox",
                  id = ns("goals_complete"),
                  name = ns("goals_complete"),
                  onchange = sprintf("Shiny.setInputValue('%s', this.checked);", ns("goals_complete"))
                ),
                tags$span(paste0("  ", iphra_txt("Goals and Objectives Complete")))  # small non-breaking space before text
              )
            ),

            shiny::h4(iphra_txt("Selected Objectives")),
            shiny::uiOutput(ns("full_objectives_ui"))
          )
        )
      ),

      # Right side (SVG diagram)
      shiny::column(
        width = 6,
        # >>> Color Key <<<
        tags$div(
          style = "margin-bottom:10px;",
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:white; border:1px solid #ccc; margin-right:5px;"
          ),
          iphra_txt("Unselected"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:lightgreen; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          iphra_txt("Primary Sources"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:lightblue; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          iphra_txt("Secondary Sources"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:#D8BFD8; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          iphra_txt("Both")
        ),
        tags$div(
          id = ns("framework_container"),
          style = "border:1px solid #ccc; height:800px; overflow:auto;",
          uiOutput(ns("framework_svg"))
        )
      )
    ),

    # Custom JS handler for SVG highlighting
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateFramework', function(message){
        console.log('framework incoming:', message.code);
        try {
          eval(message.code);
        } catch(e) {
          console.error('framework eval error:', e, message.code);
        }
      });
    "))
  )
}

#' goals Server Functions
#'
#' @noRd
mod_goals_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    reference_objectives <- tibble::tribble(
      ~pillar, ~short_objective, ~text_objective, ~sub_pillar, ~core, ~extended, ~outcomes, ~fsl, ~wash, ~health,

      "FSL", "Food Security",  "Improve food security",  "FoodSecurity",
      "Core", "Extended", "Outcome", "FSL", NA, NA,

      "WASH", "Water Security", "Improve water security", "WaterSecurity",
      "Core", "Extended", "Outcome", NA, "WASH", NA,

      "HEALTH", "Health Status", "Improve health status", "HealthStatus",
      "Core", "Extended", "Outcome", NA, NA, "HEALTH"
    )

    output$dynamic_select_ui <- renderUI({

      # choices <- filtered_available_objectives()

      default_selection <- c("FSL", "WASH")

      selectInput(
        ns("dynamic_select"),
        label = iphra_txt("Select Dimensions"),
        choices = unique(reference_objectives$pillar),  # Replace with your reactive or static vector
        selected = default_selection,
        multiple = TRUE
      )
    })

    output$dynamic_select_sdr_ui <- renderUI({

      # choices <- filtered_available_objectives()

      default_selection <- c("FSL", "WASH")

      selectInput(
        ns("dynamic_select_sdr"),
        label = iphra_txt("Select Dimensions"),
        choices = unique(reference_objectives$pillar),  # Replace with your reactive or static vector
        selected = default_selection,
        multiple = TRUE
      )
    })

    # ---- Indicators definition (lives inside module) ----
    all_objectives <- reference_objectives$short_objective

    filtered_available_objectives <- reactive({
      req(input$dynamic_select)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select, ]
      filtered$short_objective  # Return character vector of indicator names

    })

    filtered_available_sdr_objectives <- reactive({
      req(input$dynamic_select_sdr)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select_sdr, ]
      filtered$short_objective  # Return character vector of indicator names

    })



    # ---- Reactive state ----
    selected <- shiny::reactiveVal(character(0))
    selected_sdr <- shiny::reactiveVal(character(0))

    # ---- UI for available list ----

    output$full_objectives_ui <- renderUI({

      sel <- selected()  # `selected()` should return a character vector of objectives
      sel_sdr <- selected_sdr()  # `selected()` should return a character vector of objectives

      prim_obj <- reference_objectives %>%
                 filter(short_objective %in% sel) %>%
                 dplyr::pull(text_objective)

      sdr_obj <- reference_objectives %>%
                     filter(short_objective %in% sel_sdr) %>%
                     dplyr::pull(text_objective)

      objs <- c(prim_obj, sdr_obj)


      if (length(objs) == 0) {
        shiny::em(iphra_txt("No objectives selected."))
      } else {
        tags$ul(
          lapply(objs, tags$li)
        )
      }
    })

    output$available_ui <- shiny::renderUI({
      sortable::rank_list(
        text = iphra_txt("Available Objectives"),
        labels = setdiff(filtered_available_objectives(), selected()),
        input_id = ns("available"),
        options = sortable::sortable_options(group = ns("all_objectives"))
      )
    })

    # ---- UI for selected list ----
    output$selected_ui <- shiny::renderUI({
      sortable::rank_list(
        text = iphra_txt("Selected Objectives"),
        labels = selected(),
        input_id = ns("selected"),
        options = sortable::sortable_options(group = ns("all_objectives"))
      )
    })

    output$available_sdr_ui <- shiny::renderUI({
      sortable::rank_list(
        text = iphra_txt("Available Objectives"),
        labels = setdiff(filtered_available_sdr_objectives(), selected_sdr()),
        input_id = ns("available_sdr"),
        options = sortable::sortable_options(group = ns("all_objectives"))
      )
    })

    # ---- UI for selected list ----
    output$selected_sdr_ui <- shiny::renderUI({
      sortable::rank_list(
        text = iphra_txt("Selected Objectives"),
        labels = selected_sdr(),
        input_id = ns("selected_sdr"),
        options = sortable::sortable_options(group = ns("all_objectives"))
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
        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Selection Update")
        )
        # (Optional future: validate input$selected not NULL or empty)

        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_selected/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected(input$selected)
        iphra_message(
          paste0(
            iphra_txt("Selected item(s) updated to: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = iphra_txt("Selection Update")
        )

        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_selected/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Selection update processed successfully."),
          origin = iphra_txt("Selection Update")
        )
        }, step = "mod_goals_server/observeEvent_selected/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Selection Update"),
      hint = iphra_txt("Check input binding or reactive assignment if this fails.")
      )
    })

    observeEvent(input$selected_sdr, {
      iphra_try({

        # ────────────────────────────────────────────────

        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("SDR Selection Update")
        )
        # (Optional future: validate input$selected_sdr not NULL or empty)


        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_selected_sdr/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        selected_sdr(input$selected_sdr)
        iphra_message(
          paste0(
            iphra_txt("SDR selection updated to: "),
            paste(input$selected_sdr, collapse = ", ")
          ),
          origin = iphra_txt("SDR selection Update")
        )


        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_selected_sdr/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("SDR selection update processed successfully."),
          origin = iphra_txt("SDR Selection Update")
        )
        }, step = "mod_goals_server/observeEvent_selected_sdr/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("SDR Selection Update"),
      hint = iphra_txt("Check input binding or reactive assignment if this fails.")
      )
    })

    # ---- Presets ----

    # Core preset
    observeEvent(input$preset_core, {
      iphra_try({


        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_core/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected(
          reference_objectives %>%
            dplyr::filter(core %in% c("Core")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("Core objectives preset applied."),
          origin = iphra_txt("Preset: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_core/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Core preset selection processed successfully."),
          origin = iphra_txt("Preset: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_core/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Preset: Core Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })


    # Full preset
    observeEvent(input$preset_full, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: Full Objectives")
        )

        selected(
          reference_objectives %>%
            dplyr::filter(extended %in% c("Extended")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("Full objectives preset applied."),
          origin = iphra_txt("Preset: Full Objectives")
        )

        iphra_message(
          iphra_txt("Full preset selection processed successfully."),
          origin = iphra_txt("Preset: Full Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset: Full Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })


    # Outcomes preset
    observeEvent(input$preset_outcomes, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: Outcomes Objectives")
        )

        selected(
          reference_objectives %>%
            dplyr::filter(outcomes %in% c("Outcome")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("Outcome objectives preset applied."),
          origin = iphra_txt("Preset: Outcomes Objectives")
        )

        iphra_message(
          iphra_txt("Outcome preset selection processed successfully."),
          origin = iphra_txt("Preset: Outcomes Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset: Outcomes Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })


    # FSL preset
    observeEvent(input$preset_fsl, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: FSL Objectives")
        )

        selected(
          reference_objectives %>%
            dplyr::filter(fsl %in% c("FSL")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("FSL objectives preset applied."),
          origin = iphra_txt("Preset: FSL Objectives")
        )

        iphra_message(
          iphra_txt("FSL preset selection processed successfully."),
          origin = iphra_txt("Preset: FSL Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset: FSL Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })


    # WASH preset
    observeEvent(input$preset_wash, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: WASH Objectives")
        )

        selected(
          reference_objectives %>%
            dplyr::filter(wash %in% c("WASH")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("WASH objectives preset applied."),
          origin = iphra_txt("Preset: WASH Objectives")
        )

        iphra_message(
          iphra_txt("WASH preset selection processed successfully."),
          origin = iphra_txt("Preset: WASH Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset: WASH Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })


    # Health preset
    observeEvent(input$preset_health, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset: Health Objectives")
        )

        selected(
          reference_objectives %>%
            dplyr::filter(health %in% c("HEALTH")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("Health objectives preset applied."),
          origin = iphra_txt("Preset: Health Objectives")
        )

        iphra_message(
          iphra_txt("Health preset selection processed successfully."),
          origin = iphra_txt("Preset: Health Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset: Health Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })

    # ---- Presets SDR ----

    # SDR Core preset
    observeEvent(input$preset_sdr_core, {
      iphra_try({


        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_sdr_core/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          selected_sdr(
          reference_objectives %>%
            dplyr::filter(core %in% c("Core")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR Core objectives preset applied."),
          origin = iphra_txt("Preset SDR: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_sdr_core/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("SDR Core preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: Core Objectives")
        )
        }, step = "mod_goals_server/observeEvent_preset_sdr_core/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Core Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })


    # SDR Full preset
    observeEvent(input$preset_sdr_full, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: Full Objectives")
        )

        selected_sdr(
          reference_objectives %>%
            dplyr::filter(extended %in% c("Extended")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR Full objectives preset applied."),
          origin = iphra_txt("Preset SDR: Full Objectives")
        )

        iphra_message(
          iphra_txt("SDR Full preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: Full Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Full Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })


    # SDR Outcomes preset
    observeEvent(input$preset_sdr_outcomes, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: Outcomes Objectives")
        )

        selected_sdr(
          reference_objectives %>%
            dplyr::filter(outcomes %in% c("Outcome")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR Outcome objectives preset applied."),
          origin = iphra_txt("Preset SDR: Outcomes Objectives")
        )

        iphra_message(
          iphra_txt("SDR Outcome preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: Outcomes Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Outcomes Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })


    # SDR FSL preset
    observeEvent(input$preset_sdr_fsl, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: FSL Objectives")
        )

        selected_sdr(
          reference_objectives %>%
            dplyr::filter(fsl %in% c("FSL")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR FSL objectives preset applied."),
          origin = iphra_txt("Preset SDR: FSL Objectives")
        )

        iphra_message(
          iphra_txt("SDR FSL preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: FSL Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: FSL Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })


    # SDR WASH preset
    observeEvent(input$preset_sdr_wash, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: WASH Objectives")
        )

        selected_sdr(
          reference_objectives %>%
            dplyr::filter(wash %in% c("WASH")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR WASH objectives preset applied."),
          origin = iphra_txt("Preset SDR: WASH Objectives")
        )

        iphra_message(
          iphra_txt("SDR WASH preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: WASH Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: WASH Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })


    # SDR Health preset
    observeEvent(input$preset_sdr_health, {
      iphra_try({

        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Preset SDR: Health Objectives")
        )

        selected_sdr(
          reference_objectives %>%
            dplyr::filter(health %in% c("HEALTH")) %>%
            dplyr::pull(short_objective)
        )
        iphra_message(
          iphra_txt("SDR Health objectives preset applied."),
          origin = iphra_txt("Preset SDR: Health Objectives")
        )

        iphra_message(
          iphra_txt("SDR Health preset selection processed successfully."),
          origin = iphra_txt("Preset SDR: Health Objectives")
        )

      },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Health Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })

    # --- SVG with individual block IDs ---
    base_svg <- paste0('
<div id="%s">
<svg width="950" height="800" xmlns="http://www.w3.org/2000/svg">


  <!-- Community Level Factors -->
  <g id="CommLevelFactors">
    <rect x="10" y="8" width="860" height="570" fill="white" stroke="black"/>
    <text x="55" y="25" text-anchor="middle" font-size="7"">Community Level Factors</text>
  </g>
  <!-- Household Level Factors -->
  <g id="HHLevelFactors">
    <rect x="100" y="8" width="690" height="400" fill="white" stroke="black"/>
    <text x="145" y="25" text-anchor="middle" font-size="7"">HH Level Factors</text>
  </g>
  <!-- Individual Level Factors -->
  <g id="IndLevelFactors">
    <rect x="190" y="8" width="500" height="240" fill="white" stroke="black"/>
    <text x="235" y="25" text-anchor="middle" font-size="7">Ind. Level Factors</text>
  </g>
  <!-- Individual Health Outcomes -->
  <g id="Outcomes">
    <rect x="300" y="10" width="320" height="130" fill="white" stroke="black"/>
    <text x="345" y="25" text-anchor="middle" font-size="7">Ind. Health Outcomes</text>
  </g>

  <!-- Mortality -->
  <g id="Mortality">
    <rect x="400" y="20" width="100" height="35" fill="white" stroke="black"/>
    <text x="450" y="40" text-anchor="middle" font-size="9">Mortality</text>
  </g>

  <!-- Nutrition Status -->
  <g id="NutritionStatus">
    <rect x="330" y="90" width="100" height="35" fill="white" stroke="black"/>
    <text x="380" y="110" text-anchor="middle" font-size="9">Nutrition status</text>
  </g>

  <!-- Health Status -->
  <g id="HealthStatus">
    <rect x="490" y="90" width="100" height="35" fill="white" stroke="black"/>
    <text x="540" y="110" text-anchor="middle" font-size="9">Health status</text>
  </g>

  <!-- Food Consumption -->
  <g id="FoodConsumption">
    <rect x="230" y="160" width="100" height="35" fill="white" stroke="black"/>
    <text x="280" y="180" text-anchor="middle" font-size="9">Food consumption</text>
  </g>
        <g id="FoodQuantity">
          <rect x="240" y="202.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="280" y="210" text-anchor="middle" font-size="7">Quantity</text>
        </g>
        <g id="FoodQuality">
          <rect x="240" y="222.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="280" y="230" text-anchor="middle" font-size="7">Quality</text>
        </g>

  <!-- Water Consumption -->
  <g id="WaterConsumption">
    <rect x="400" y="160" width="100" height="35" fill="white" stroke="black"/>
    <text x="450" y="180" text-anchor="middle" font-size="9">Water consumption</text>
        </g>
        <g id="WaterQuantity">
          <rect x="410" y="202.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="450" y="210" text-anchor="middle" font-size="7">Quantity</text>
        </g>
        <g id="WaterQuality">
          <rect x="410" y="222.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="450" y="230" text-anchor="middle" font-size="7">Quality</text>
        </g>

  <!-- Exposure to Disease -->
  <g id="ExposureDisease">
    <rect x="560" y="160" width="100" height="35" fill="white" stroke="black"/>
    <text x="610" y="180" text-anchor="middle" font-size="9">Exposure to disease</text>
  </g>
        <g id="ReproductiveNumber">
          <rect x="570" y="202.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="610" y="210" text-anchor="middle" font-size="7">Reproductive Number</text>
        </g>
        <g id="RiskFactors">
          <rect x="570" y="222.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="610" y="230" text-anchor="middle" font-size="7">Risk Factors</text>
        </g>
        <g id="Hygiene">
          <rect x="570" y="222.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="610" y="230" text-anchor="middle" font-size="7">Hygiene</text>
        </g>

    <!-- Food Security -->
  <g id="FoodSecurity">
    <rect x="150" y="260" width="100" height="35" fill="white" stroke="black"/>
    <text x="200" y="280" text-anchor="middle" font-size="9">Food Security</text>
  </g>
        <g id="HHFoodConsumption">
          <rect x="160" y="302.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="200" y="310" text-anchor="middle" font-size="7">HH Consumption</text>
        </g>
        <g id="FoodAvailability">
          <rect x="160" y="322.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="200" y="330" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="FoodAccessiblity">
          <rect x="160" y="342.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="200" y="350" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="FoodUtilization">
          <rect x="160" y="362.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="200" y="370" text-anchor="middle" font-size="7">Utilization</text>
        </g>
        <g id="FoodStability">
          <rect x="160" y="382.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="200" y="390" text-anchor="middle" font-size="7">Stability</text>
        </g>

    <!-- Water Security -->
  <g id="WaterSecurity">
    <rect x="310" y="260" width="100" height="35" fill="white" stroke="black"/>
    <text x="360" y="280" text-anchor="middle" font-size="9">Water Security</text>
  </g>
        <g id="HHWaterConsumption">
          <rect x="320" y="302.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="360" y="310" text-anchor="middle" font-size="7">HH Consumption</text>
        </g>
        <g id="WaterAvailability">
          <rect x="320" y="322.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="360" y="330" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="WaterAccessibility">
          <rect x="320" y="342.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="360" y="350" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="WaterUtilization">
          <rect x="320" y="362.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="360" y="370" text-anchor="middle" font-size="7">Utilization</text>
        </g>
        <g id="WaterStability">
          <rect x="320" y="382.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="360" y="390" text-anchor="middle" font-size="7">Stability</text>
        </g>

    <!-- Income and Coping Capacities  -->
  <g id="IncomeCoping">
    <rect x="470" y="260" width="100" height="35" fill="white" stroke="black"/>
    <text x="520" y="280" text-anchor="middle" font-size="9">Income and Coping</text>
  </g>
        <g id="Income">
          <rect x="480" y="302.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="520" y="310" text-anchor="middle" font-size="7">Income</text>
        </g>
        <g id="CopingCapacity">
          <rect x="480" y="322.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="520" y="330" text-anchor="middle" font-size="7">Coping Capacity</text>
        </g>
        <g id="Assets">
          <rect x="480" y="342.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="520" y="350" text-anchor="middle" font-size="7">Assets</text>
        </g>

  <!-- Living Conditions -->
  <g id="LivingConditions">
    <rect x="630" y="260" width="100" height="35" fill="white" stroke="black"/>
    <text x="680" y="280" text-anchor="middle" font-size="9">Living conditions</text>
  </g>
        <g id="ProtElements">
          <rect x="640" y="302.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="680" y="310" text-anchor="middle" font-size="7">Protection Elements</text>
        </g>
        <g id="ProtVectors">
          <rect x="640" y="322.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="680" y="330" text-anchor="middle" font-size="7">Protection Vectors</text>
        </g>
        <g id="Crowdedness">
          <rect x="640" y="342.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="680" y="350" text-anchor="middle" font-size="7">Crowdedness</text>
        </g>

  <!-- Livelihoods -->
  <g id="Livelihoods">
    <rect x="60" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="110" y="440" text-anchor="middle" font-size="9">Livelihoods</text>
  </g>
        <g id="LivAvailability">
          <rect x="70" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="110" y="470" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="LivAccessibility">
          <rect x="70" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="110" y="490" text-anchor="middle" font-size="7">Accessibility</text>
        </g>

    <!-- Markets -->
  <g id="Markets">
    <rect x="200" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="250" y="440" text-anchor="middle" font-size="9">Markets</text>
  </g>
        <g id="MarketAvailability">
          <rect x="210" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="260" y="470" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="MarketAccessibility">
          <rect x="210" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="260" y="490" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="MarketFunctionality">
          <rect x="210" y="502.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="260" y="510" text-anchor="middle" font-size="7">Functionality</text>
        </g>

      <!-- WASH Services -->
  <g id="WASHServices">
    <rect x="330" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="380" y="440" text-anchor="middle" font-size="9">WASH Services</text>
  </g>
        <g id="WASHAvailability">
          <rect x="340" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="380" y="470" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="WASHAccessibility">
          <rect x="340" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="380" y="490" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="WASHQuality">
          <rect x="340" y="502.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="380" y="510" text-anchor="middle" font-size="7">Quality</text>
        </g>
        <g id="WASHUseBehaviours">
          <rect x="340" y="522.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="380" y="530" text-anchor="middle" font-size="7">Use Behaviours</text>
        </g>

      <!-- Nutrition Services -->
  <g id="NutritionServices">
    <rect x="460" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="520" y="440" text-anchor="middle" font-size="9">Nutrition Services</text>
  </g>
        <g id="NutAvailability">
          <rect x="470" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="510" y="470" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="NutAccessibility">
          <rect x="470" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="510" y="490" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="NutQuality">
          <rect x="470" y="502.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="510" y="510" text-anchor="middle" font-size="7">Quality</text>
        </g>
        <g id="NutUseBehaviours">
          <rect x="470" y="522.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="510" y="530" text-anchor="middle" font-size="7">Use Behaviours</text>
        </g>
        <g id="NutPreventative">
          <rect x="470" y="542.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="510" y="550" text-anchor="middle" font-size="7">Prevention</text>
        </g>

      <!-- Health Services -->
  <g id="HealthServices">
    <rect x="590" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="640" y="440" text-anchor="middle" font-size="9">Health Services</text>
  </g>
        <g id="HealthAvailability">
          <rect x="600" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="640" y="470" text-anchor="middle" font-size="7">Availability</text>
        </g>
        <g id="HealthAccessibility">
          <rect x="600" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="640" y="490" text-anchor="middle" font-size="7">Accessibility</text>
        </g>
        <g id="HealthQuality">
          <rect x="600" y="502.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="640" y="510" text-anchor="middle" font-size="7">Quality</text>
        </g>
        <g id="HealthUseBehaviours">
          <rect x="600" y="522.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="640" y="530" text-anchor="middle" font-size="7">Use Behaviours</text>
        </g>
        <g id="HealthPreventative">
          <rect x="600" y="542.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="640" y="550" text-anchor="middle" font-size="7">Prevention</text>
        </g>

        <!-- Environment -->
  <g id="Environment">
    <rect x="720" y="420" width="100" height="35" fill="white" stroke="black"/>
    <text x="770" y="440" text-anchor="middle" font-size="9">Environment</text>
  </g>
        <g id="EnvHygiene">
          <rect x="730" y="462.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="770" y="470" text-anchor="middle" font-size="7">Env Hygiene</text>
        </g>
        <g id="Sanitation">
          <rect x="730" y="482.5" width="80" height="15" fill="white" stroke="lightgrey"/>
          <text x="770" y="490" text-anchor="middle" font-size="7">Sanitation</text>
        </g>

<!-- Shocks and Hazards -->

  <g id="Hazards">
    <rect x="250" y="600" width="400" height="60" fill="white" stroke="black"/>
    <text x="450" y="630" text-anchor="middle" font-size="9">Shocks and Hazards</text>
  </g>

  <g id="Governance">
    <rect x="150" y="700" width="100" height="60" fill="white" stroke="black"/>
    <text x="200" y="725" text-anchor="middle" font-size="9">
      <tspan x="200" dy="0">Governance, policies</tspan>
      <tspan x="200" dy="1.2em">institutions</tspan>
    </text>
  </g>


  <g id="Norms">
    <rect x="300" y="700" width="120" height="60" fill="white" stroke="black"/>
    <text x="360" y="725" text-anchor="middle" font-size="9">
      <tspan x="360" dy="0">Cultural beliefs, practices,</tspan>
      <tspan x="360" dy="1.2em">social norms</tspan>
    </text>
  </g>

    <g id="Macroeconomy">
    <rect x="470" y="700" width="100" height="60" fill="white" stroke="black"/>
    <text x="520" y="730" text-anchor="middle" font-size="9">Macroeconomy</text>
  </g>

  <g id="Demographics">
    <rect x="630" y="700" width="100" height="60" fill="white" stroke="black"/>
    <text x="680" y="725" text-anchor="middle" font-size="9">
      <tspan x="680" dy="0">Population</tspan>
      <tspan x="680" dy="1.2em">demographics</tspan>
    </text>
  </g>

</svg>
</div>', ns("framework_container"))

    # --- Render the SVG ---
    output$framework_svg <- renderUI({ HTML(base_svg) })


    # -------------------------
    # Highlight SVG blocks
    # -------------------------
    observe({
      iphra_try({

        # ────────────────────────────────────────────────

        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Reactive update triggered for framework visualization."),
          origin = iphra_txt("Framework SVG Highlighter")
        )
        # (Optional future: validate selected() and selected_sdr() not NULL)


        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observe/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        sel <- selected()
        sel_sdr <- selected_sdr()

        all_block_ids <- reference_objectives$sub_pillar

        block_ids <- reference_objectives %>%
          dplyr::filter(short_objective %in% sel) %>%
          dplyr::pull(sub_pillar)

        block_ids_sdr <- reference_objectives %>%
          dplyr::filter(short_objective %in% sel_sdr) %>%
          dplyr::pull(sub_pillar)

        both_block_ids <- intersect(block_ids, block_ids_sdr)

        js <- sprintf("
      var svg = document.querySelector('#%s svg');
      if(svg){
        var greenIds = %s;
        var blueIds = %s;
        var purpleIds = %s;
        %s
      }",
                      ns('framework_container'),
                      jsonlite::toJSON(block_ids, auto_unbox = TRUE),
                      jsonlite::toJSON(block_ids_sdr, auto_unbox = TRUE),
                      jsonlite::toJSON(both_block_ids, auto_unbox = TRUE),
                      paste0(
                        sapply(all_block_ids, function(bid) {
                          sprintf("
            var g = svg.getElementById('%s');
            if(g){
              var r = g.querySelector('rect');
              if(purpleIds.includes('%s')){
                r.setAttribute('fill','#D8BFD8');
              } else if(greenIds.includes('%s')){
                r.setAttribute('fill','lightgreen');
              } else if(blueIds.includes('%s')){
                r.setAttribute('fill','lightblue');
              } else {
                r.setAttribute('fill','white');
              }
            }", bid, bid, bid, bid)
                        }),
                        collapse = '\n'
                      )
        )

        session$sendCustomMessage("updateFramework", list(code = js))

        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observe/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Framework visualization updated successfully (dummy mode)."),
          origin = iphra_txt("Framework SVG Highlighter")
        )
        }, step = "mod_goals_server/observe/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Framework SVG Highlighter"),
      hint = iphra_txt("Check reactive dependencies or JavaScript message binding if this fails.")
      )
    })


    observeEvent(input$goals_complete, {
      iphra_try({

        # ────────────────────────────────────────────────

        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Validation checks passed (dummy mode)."),
          origin = iphra_txt("Goals and Objectives Completion Toggle")
        )

        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_goals_complete/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (isTRUE(input$goals_complete)) {
          # --- Future: trigger save, update status table, etc. ---
          iphra_message(
            iphra_txt("Goals and Objectives marked as complete ✅"),
            origin = iphra_txt("Goals and Objectives Completion Toggle")
          )
        } else {
          # --- Future: handle incomplete state ---
          iphra_message(
            iphra_txt("Goals and Objectives marked as incomplete ❌"),
            origin = iphra_txt("Goals and Objectives Completion Toggle")
          )
        }

        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observeEvent_goals_complete/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Completion status change simulated successfully."),
          origin = iphra_txt("Goals and Objectives Completion Toggle")
        )
        }, step = "mod_goals_server/observeEvent_goals_complete/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Goals and Objectives Completion Toggle"),
      hint = iphra_txt("Check event binding or session state if this fails.")
      )
    })


  })
}

## To be copied in the UI
# mod_goals_ui("goals_1")

## To be copied in the server
# mod_goals_server("goals_1")
