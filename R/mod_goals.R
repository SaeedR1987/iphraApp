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
    )
  )
}

#' goals Server Functions
#'
#' @noRd
mod_goals_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    library(dplyr)

    # Setup Reference Objectives and Lookups ####
    protocol <- session$userData$modules[["protocol"]]
    framework <- protocol$access_nested("framework")
    reference_objectives <- framework$master_objectives_schema

    # ---- Lookup maps between objective_code and short_objective ----
    unique_objectives <- reference_objectives[
      !duplicated(reference_objectives$objective_code),
    ]

    code_to_short <- setNames(
      unique_objectives$short_objective,
      unique_objectives$objective_code
    )

    short_to_code <- setNames(
      unique_objectives$objective_code,
      unique_objectives$short_objective
    )

    # --- SVG with individual block IDs ---
    base_svg <- framework$adjusted_svg

    # Initializing Reactive Values ####
    svg_to_display <- reactiveVal(framework$adjusted_svg)
    selected <- shiny::reactiveVal(character(0))
    selected_sdr <- shiny::reactiveVal(character(0))

    # Primary Objective Selection

    all_objectives <- unique(reference_objectives$objective_code)

    filtered_available_objectives <- reactive({
      req(input$dynamic_select)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select, ]
      unique(filtered$objective_code)  # Return character vector of objective codes

    })

    filtered_available_sdr_objectives <- reactive({
      req(input$dynamic_select_sdr)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select_sdr, ]
      unique(filtered$objective_code)  # Return character vector of objective codes

    })


    # Outputs ####

    output$dynamic_select_ui <- renderUI({

      default_selection <- c("Demographics", "HealthStatus")

      selectInput(
        ns("dynamic_select"),
        label = iphra_txt("Select Pillars"),
        choices = unique(reference_objectives$pillar),  # Replace with your reactive or static vector
        selected = default_selection,
        multiple = TRUE
      )
    })

    output$dynamic_select_sdr_ui <- renderUI({

      default_selection <- c("Demographics", "HealthStatus")

      selectInput(
        ns("dynamic_select_sdr"),
        label = iphra_txt("Select Dimensions"),
        choices = unique(reference_objectives$pillar),  # Replace with your reactive or static vector
        selected = default_selection,
        multiple = TRUE
      )
    })

    # output$available_ui <- renderUI({
    #
    #   tags$div(
    #     style = "border:1px solid red;",
    #     paste(labels, collapse = ", ")
    #   )
    #
    # })

    output$available_ui <- shiny::renderUI({

      cat("Rendering available_ui\n")

      available_codes <- setdiff(filtered_available_objectives(), selected())

      print(available_codes)

      print(available_codes[1])

      print(code_to_short["101"])

      print(code_to_short[101])

      labels <- unname(code_to_short[as.character(available_codes)])
      labels <- labels[!is.na(labels)]

      cat("available codes:", length(available_codes), "\n")
      cat("labels:", length(labels), "\n")


      sortable::rank_list(
        text = iphra_txt("Available Objectives"),
        labels = labels,
        input_id = "available",
        options = sortable::sortable_options(group = "all_objectives")
      )



    })

    # ---- UI for selected list
    output$selected_ui <- shiny::renderUI({
      labels <- unname(code_to_short[as.character(selected())])
      labels <- labels[!is.na(labels)]
      sortable::rank_list(
        text = iphra_txt("Selected Objectives"),
        labels = labels,
        input_id = "selected",
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    output$available_sdr_ui <- shiny::renderUI({
      available_sdr_codes <- setdiff(filtered_available_sdr_objectives(), selected_sdr())
      labels_sdr <- unname(code_to_short[as.character(available_sdr_codes)])
      labels_sdr <- labels_sdr[!is.na(labels_sdr)]
      sortable::rank_list(
        text = iphra_txt("Available Objectives"),
        labels = labels_sdr,
        input_id = "available_sdr",
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    # ---- UI for selected list
    output$selected_sdr_ui <- shiny::renderUI({
      labels_sdr <- unname(code_to_short[as.character(selected_sdr())])
      labels_sdr <- labels_sdr[!is.na(labels_sdr)]
      sortable::rank_list(
        text = iphra_txt("Selected Objectives"),
        labels = labels_sdr,
        input_id = "selected_sdr",
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    # for full text objectives preview
    output$full_objectives_ui <- renderUI({

      sel <- selected()  # `selected()` should return a character vector of objectives
      sel_sdr <- selected_sdr()  # `selected()` should return a character vector of objectives

      prim_obj <- reference_objectives %>%
        filter(objective_code %in% sel) %>%
        dplyr::pull(text_objective) %>% unique()

      sdr_obj <- reference_objectives %>%
        filter(objective_code %in% sel_sdr) %>%
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

    # --- Render the initial SVG ---
    output$framework_svg <- renderUI({
      cat("Rendering SVG\n")
      HTML(svg_to_display())
    })

    # Observes ####

    observe({
      cat("dynamic_select:\n")
      print(input$dynamic_select)
    })

    # ---- Update modified objectives schema in protocol when selections change
    observe({
      sel <- selected()
      sel_sdr <- selected_sdr()

      selected_objectives <- reference_objectives %>%
        dplyr::filter(objective_code %in% c(sel, sel_sdr)) %>%
        dplyr::pull(objective_code) %>%
        unique()

      framework$modify_adjusted_schema(selected_objectives)
    })

    # ---- Keep selected() in sync with drag-and-drop
    observeEvent(input$selected, {
      iphra_try({

        codes <- unname(short_to_code[as.character(input$selected)])
        selected(codes[!is.na(codes)])

        iphra_message(
          paste0(
            iphra_txt("Selected item(s) updated to: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = iphra_txt("Selection Update")
        )
        },
      on_error = "warn",
      origin = iphra_txt("Selection Update"),
      hint = iphra_txt("Check input binding or reactive assignment if this fails.")
      )
    })

    observeEvent(input$selected_sdr, {
      iphra_try({

        sdr_codes <- unname(short_to_code[input$selected_sdr])
        selected_sdr(sdr_codes[!is.na(sdr_codes)])
        iphra_message(
          paste0(
            iphra_txt("SDR selection updated to: "),
            paste(input$selected_sdr, collapse = ", ")
          ),
          origin = iphra_txt("SDR selection Update")
        )
      },
      on_error = "warn",
      origin = iphra_txt("SDR Selection Update"),
      hint = iphra_txt("Check input binding or reactive assignment if this fails.")
      )
    })

    # Core preset
    observeEvent(input$preset_core, {
      iphra_try({


          selected(
          reference_objectives %>%
            dplyr::filter(core %in% c("Core")) %>%
            dplyr::pull(objective_code)
        )
        },
      on_error = "warn",
      origin = iphra_txt("Preset: Core Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })

    # SDR Core preset
    observeEvent(input$preset_sdr_core, {
      iphra_try({


          selected_sdr(
          reference_objectives %>%
            dplyr::filter(core %in% c("Core")) %>%
            dplyr::pull(objective_code)
        )
        },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Core Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })

    # Highlight SVG blocks
    observeEvent(list(selected(), selected_sdr()),{
      iphra_try({

        # 1️⃣ VALIDATION & PRECONDITIONS

        result <- iphra_try_step({

        iphra_message(
          iphra_txt("Reactive update triggered for framework visualization."),
          origin = iphra_txt("Framework SVG Highlighter")
        )
        # (Optional future: validate selected() and selected_sdr() not NULL)

        }, step = "mod_goals_server/observe/Validation")
        if (iphra_failed(result)) return(result)

        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY

        result <- iphra_try_step({

          cat("\n====================\n")
          cat("SVG observer fired\n")

          print(selected())
          print(selected_sdr())

        sel <- selected()
        sel_sdr <- selected_sdr()

        framework$set_primary_objectives(objective_codes = sel)
        framework$set_secondary_objectives(objective_codes = sel_sdr)
        framework$modify_adjusted_svg(primary_objective_codes = sel, secondary_objective_codes = sel_sdr)

        cat(
          "Assigning SVG length:",
          nchar(framework$adjusted_svg),
          "\n"
        )

        svg_to_display(framework$adjusted_svg)

        cat("svg_to_display updated\n")



        }, step = "mod_goals_server/observe/Core Logic")
        if (iphra_failed(result)) return(result)


        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS

        result <- iphra_try_step({




        iphra_message(
          iphra_txt("Framework visualization updated successfully."),
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
