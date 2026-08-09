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

    library(dplyr)

    # Access the protocol object stored in session$userData
    protocol <- session$userData$modules[["protocol"]]

    # Get the nested ANAFramework object (R6 reference)
    framework <- protocol$access_nested("framework")

    # Master objectives schema (static full list of possible objectives)
    reference_objectives <- framework$master_objectives_schema

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

    # ---- Update modified objectives schema in protocol when selections change ----
    observe({
      sel <- selected()
      sel_sdr <- selected_sdr()

      selected_objectives <- reference_objectives %>%
        dplyr::filter(short_objective %in% c(sel, sel_sdr))

      framework$modify_adjusted_schema(selected_objectives)
    })

    # ---- UI for available list ----

    output$full_objectives_ui <- renderUI({

      sel <- selected()  # `selected()` should return a character vector of objectives
      sel_sdr <- selected_sdr()  # `selected()` should return a character vector of objectives

      prim_obj <- reference_objectives %>%
                 filter(short_objective %in% sel) %>%
                 dplyr::pull(text_objective)

      sdr_obj <- reference_objectives %>%
                     filter(short_objective %in% sel_sdr) %>%
                     dplyr::pull(text_objective) %>% unique()

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
    base_svg <- framework$master_svg

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

        framework$set_primary_objectives(objective_codes = sel)
        framewor$set_secondary_objectives(objective_codes = sel_sdr)
        framework$modify_adjusted_svg(primary_objective_codes = sel, secondary_objective_codes = sel_sdr)



        # ────────────────────────────────────────────────
        }, step = "mod_goals_server/observe/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────

        framework$adjusted_svg

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

  })
}

## To be copied in the UI
# mod_goals_ui("goals_1")

## To be copied in the server
# mod_goals_server("goals_1")
