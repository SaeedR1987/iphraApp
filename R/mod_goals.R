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

          # ---- Assessment Info Tab ----
          shiny::tabPanel(
            title = iphra_txt("Assessment Info"),

            # Group 1
            shinydashboard::box(
              title = iphra_txt("General Information"),
              width = 12,
              collapsible = TRUE,
              shiny::textInput(ns("country_name"),   iphra_txt("Country Name")),
              shiny::textInput(ns("country"),        iphra_txt("Country Code")),
              shiny::textInput(ns("month_year"),     iphra_txt("Month/Year")),
              shiny::textInput(ns("research_cycle_id"), iphra_txt("Research Cycle ID")),
              shiny::textInput(ns("assessment_title"),  iphra_txt("Assessment Title")),
              shiny::textInput(ns("type_emergency"), iphra_txt("Type of Emergency")),
              shiny::textInput(ns("type_crisis"),    iphra_txt("Type of Crisis")),
              shiny::textInput(ns("population"),     iphra_txt("Population")),
              shiny::textInput(ns("rationale"),      iphra_txt("Rationale")),
              shiny::textInput(ns("geographic_coverage"), iphra_txt("Geographic Coverage")),
              shiny::textInput(ns("stratification"), iphra_txt("Stratification")),
              shiny::dateInput(ns("release_date"),   iphra_txt("Release Date"), value = NA),
              shiny::numericInput(ns("version_number"), iphra_txt("Version Number"), value = NA, min = 0, step = 1),
              shiny::textInput(ns("mandating_body"), iphra_txt("Mandating Body")),
              shiny::textInput(ns("project_code"),   iphra_txt("Project Code"))
            ),

            # Group 2
            shinydashboard::box(
              title = iphra_txt("Timelines & Milestones"),
              width = 12,
              collapsible = TRUE,
              shiny::dateInput(ns("overall_timeframe"),              iphra_txt("Overall Timeframe"), value = NA),
              shiny::dateInput(ns("date_pilot_training"),            iphra_txt("Date: Pilot Training"), value = NA),
              shiny::dateInput(ns("date_data_collection_start"),     iphra_txt("Date: Data Collection Start"), value = NA),
              shiny::dateInput(ns("date_data_collection_end"),       iphra_txt("Date: Data Collection End"), value = NA),
              shiny::dateInput(ns("date_data_analysis"),             iphra_txt("Date: Data Analysis"), value = NA),
              shiny::dateInput(ns("date_data_validation"),           iphra_txt("Date: Data Validation"), value = NA),
              shiny::dateInput(ns("date_preliminary_presentation"),  iphra_txt("Date: Preliminary Presentation"), value = NA),
              shiny::dateInput(ns("date_outputs_validation"),        iphra_txt("Date: Outputs Validation"), value = NA),
              shiny::dateInput(ns("date_outputs_publication"),       iphra_txt("Date: Outputs Publication"), value = NA),
              shiny::dateInput(ns("date_final_presentation"),        iphra_txt("Date: Final Presentation"), value = NA),
              shiny::dateInput(ns("date_milestone_donor"),           iphra_txt("Date: Milestone Donor"), value = NA),
              shiny::dateInput(ns("date_milestone_intercluster"),    iphra_txt("Date: Milestone Intercluster"), value = NA),
              shiny::dateInput(ns("date_milestone_cluster"),         iphra_txt("Date: Milestone Cluster"), value = NA),
              shiny::dateInput(ns("date_milestone_ngo_platform"),    iphra_txt("Date: Milestone NGO Platform"), value = NA),
              shiny::dateInput(ns("date_milestone_other"),           iphra_txt("Date: Milestone Other"), value = NA)
            ),

            # Group 3
            shinydashboard::box(
              title = iphra_txt("Audiences & Expected Outputs"),
              width = 12,
              collapsible = TRUE,
              shiny::textInput(ns("audience_type_cluster"), iphra_txt("Audience Type (Cluster)")),
              shiny::selectInput(ns("expected_output_cluster"),
                label = iphra_txt("Expected Output (Cluster)"),
                choices = c("Preliminary Presentation", "Technical Report", "Brief", "Factsheet", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("expected_output_donor"),
                label = iphra_txt("Expected Output (Donor)"),
                choices = c("Preliminary Presentation", "Technical Report", "Brief", "Factsheet", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("expected_output_operational_actor"),
                label = iphra_txt("Expected Output (Operational Actor)"),
                choices = c("Preliminary Presentation", "Technical Report", "Brief", "Factsheet", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("expected_output_other"),
                label = iphra_txt("Expected Output (Other)"),
                choices = c("Preliminary Presentation", "Technical Report", "Brief", "Factsheet", "Not Applicable"),
                multiple = TRUE, selected = NULL)
            ),

            # Group 4
            shinydashboard::box(
              title = iphra_txt("Dissemination Strategy"),
              width = 12,
              collapsible = TRUE,
              shiny::selectInput(ns("dissemination_strategy_cluster"),
                label = iphra_txt("Dissemination Strategy (Cluster)"),
                choices = c("In-Person", "Email", "Remote Presentation", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("dissemination_strategy_donor"),
                label = iphra_txt("Dissemination Strategy (Donor)"),
                choices = c("In-Person", "Email", "Remote Presentation", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("dissemination_strategy_operational_actor"),
                label = iphra_txt("Dissemination Strategy (Operational Actor)"),
                choices = c("In-Person", "Email", "Remote Presentation", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("dissemination_strategy_other"),
                label = iphra_txt("Dissemination Strategy (Other)"),
                choices = c("In-Person", "Email", "Remote Presentation", "Not Applicable"),
                multiple = TRUE, selected = NULL)
            ),

            # Group 5
            shinydashboard::box(
              title = iphra_txt("Access"),
              width = 12,
              collapsible = TRUE,
              shiny::selectInput(ns("access_cluster"),
                label = iphra_txt("Access (Cluster)"),
                choices = c("Public", "Bilateral", "Restricted"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("access_donor"),
                label = iphra_txt("Access (Donor)"),
                choices = c("Public", "Bilateral", "Restricted"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("access_operational_actor"),
                label = iphra_txt("Access (Operational Actor)"),
                choices = c("Public", "Bilateral", "Restricted"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("access_other"),
                label = iphra_txt("Access (Other)"),
                choices = c("Public", "Bilateral", "Restricted"),
                multiple = TRUE, selected = NULL)
            ),

            # Group 6
            shinydashboard::box(
              title = iphra_txt("Visibility"),
              width = 12,
              collapsible = TRUE,
              shiny::selectInput(ns("visibility_cluster"),
                label = iphra_txt("Visibility (Cluster)"),
                choices = c("Public", "Restricted", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("visibility_donor"),
                label = iphra_txt("Visibility (Donor)"),
                choices = c("Public", "Restricted", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("visibility_operational_actor"),
                label = iphra_txt("Visibility (Operational Actor)"),
                choices = c("Public", "Restricted", "Not Applicable"),
                multiple = TRUE, selected = NULL),
              shiny::selectInput(ns("visibility_other"),
                label = iphra_txt("Visibility (Other)"),
                choices = c("Public", "Restricted", "Not Applicable"),
                multiple = TRUE, selected = NULL)
            ),

            # Group 7
            shinydashboard::box(
              title = iphra_txt("Output Counts"),
              width = 12,
              collapsible = TRUE,
              shiny::numericInput(ns("num_report"),               iphra_txt("# Reports"),               value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_profile"),              iphra_txt("# Profiles"),              value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_prelim_presentation"),  iphra_txt("# Preliminary Presentations"), value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_final_presentation"),   iphra_txt("# Final Presentations"),   value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_factsheet"),            iphra_txt("# Factsheets"),            value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_dashboard"),            iphra_txt("# Dashboards"),            value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_webmap"),               iphra_txt("# Webmaps"),               value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_map"),                  iphra_txt("# Maps"),                  value = NA, min = 0, step = 1),
              shiny::numericInput(ns("num_output_other"),         iphra_txt("# Other Outputs"),         value = NA, min = 0, step = 1)
            )
          ),

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

    # Setup Reference Objectives and Lookups ####
    protocol <- session$userData$modules[["protocol"]]
    framework <- protocol$access_nested("framework")
    reference_objectives <- framework$master_objectives_schema

    # Normalize the objective_code column to character up-front so every
    # downstream operation (setdiff, %in%, named-vector lookups, and the
    # calls into `framework$*` which key the SVG by character element ids)
    # sees a consistent type. The three-digit numeric codes (e.g. 101)
    # are treated as strings ("101") throughout the module.
    reference_objectives$objective_code <- as.character(
      reference_objectives$objective_code
    )

    # ---- Lookup maps between objective_code and short_objective ----
    unique_objectives <- reference_objectives[
      !duplicated(reference_objectives$objective_code),
    ]

    code_to_short <- setNames(
      as.character(unique_objectives$short_objective),
      as.character(unique_objectives$objective_code)
    )

    short_to_code <- setNames(
      as.character(unique_objectives$objective_code),
      as.character(unique_objectives$short_objective)
    )

    # --- SVG with individual block IDs ---
    base_svg <- framework$adjusted_svg

    # Initializing Reactive Values ####
    svg_to_display <- reactiveVal(framework$adjusted_svg)
    selected <- shiny::reactiveVal(character(0))
    selected_sdr <- shiny::reactiveVal(character(0))

    # Primary Objective Selection

    all_objectives <- as.character(unique(reference_objectives$objective_code))

    filtered_available_objectives <- reactive({
      req(input$dynamic_select)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select, ]
      as.character(unique(filtered$objective_code))

    })

    filtered_available_sdr_objectives <- reactive({
      req(input$dynamic_select_sdr)  # Ensure input is available

      filtered <- reference_objectives[reference_objectives$pillar %in% input$dynamic_select_sdr, ]
      as.character(unique(filtered$objective_code))

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

      available_codes <- setdiff(filtered_available_objectives(), selected())

      labels <- unname(code_to_short[as.character(available_codes)])
      labels <- labels[!is.na(labels)]

      sortable::rank_list(
        text = iphra_txt("Available Objectives"),
        labels = labels,
        input_id = ns("available"),
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
        input_id = ns("selected"),
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
        input_id = ns("available_sdr"),
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
        input_id = ns("selected_sdr"),
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    # for full text objectives preview
    output$full_objectives_ui <- renderUI({

      sel <- as.character(selected())
      sel_sdr <- as.character(selected_sdr())

      prim_obj <- reference_objectives %>%
        dplyr::filter(objective_code %in% sel) %>%
        dplyr::pull(text_objective) %>% unique()

      sdr_obj <- reference_objectives %>%
        dplyr::filter(objective_code %in% sel_sdr) %>%
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
      HTML(svg_to_display())
    })

    # Observes ####

    # ---- Assessment Info metadata observers ----
    # Character fields (group 1)
    local({
      char_fields <- c(
        "country_name", "country", "month_year", "research_cycle_id",
        "assessment_title", "type_emergency", "type_crisis", "population",
        "rationale", "geographic_coverage", "stratification", "mandating_body",
        "project_code"
      )
      for (fld in char_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol$metadata[[f]] <- input[[f]]
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # Numeric field (group 1)
    observeEvent(input$version_number, {
      protocol$metadata$version_number <- input$version_number
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Date fields (group 1)
    observeEvent(input$release_date, {
      protocol$metadata$release_date <- input$release_date
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Date fields (group 2)
    local({
      date_fields <- c(
        "overall_timeframe", "date_pilot_training", "date_data_collection_start",
        "date_data_collection_end", "date_data_analysis", "date_data_validation",
        "date_preliminary_presentation", "date_outputs_validation",
        "date_outputs_publication", "date_final_presentation",
        "date_milestone_donor", "date_milestone_intercluster",
        "date_milestone_cluster", "date_milestone_ngo_platform",
        "date_milestone_other"
      )
      for (fld in date_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol$metadata[[f]] <- input[[f]]
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # Character field (group 3)
    observeEvent(input$audience_type_cluster, {
      protocol$metadata$audience_type_cluster <- input$audience_type_cluster
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Select multiple fields (groups 3-6)
    local({
      multi_fields <- c(
        "expected_output_cluster", "expected_output_donor",
        "expected_output_operational_actor", "expected_output_other",
        "dissemination_strategy_cluster", "dissemination_strategy_donor",
        "dissemination_strategy_operational_actor", "dissemination_strategy_other",
        "access_cluster", "access_donor", "access_operational_actor", "access_other",
        "visibility_cluster", "visibility_donor", "visibility_operational_actor",
        "visibility_other"
      )
      for (fld in multi_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol$metadata[[f]] <- input[[f]]
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # Numeric integer fields (group 7)
    local({
      num_fields <- c(
        "num_report", "num_profile", "num_prelim_presentation",
        "num_final_presentation", "num_factsheet", "num_dashboard",
        "num_webmap", "num_map", "num_output_other"
      )
      for (fld in num_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol$metadata[[f]] <- as.integer(input[[f]])
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # ---- Keep selected() in sync with drag-and-drop
    observeEvent(input$selected, {
      iphra_try({

        codes <- unname(short_to_code[as.character(input$selected)])
        codes <- as.character(codes[!is.na(codes)])
        # Only update if the set actually changed to avoid feedback loops
        # with the renderUI that rebuilds the rank_list.
        if (!setequal(codes, selected())) {
          selected(codes)
        }

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
    }, ignoreNULL = FALSE)

    observeEvent(input$selected_sdr, {
      iphra_try({

        sdr_codes <- unname(short_to_code[as.character(input$selected_sdr)])
        sdr_codes <- as.character(sdr_codes[!is.na(sdr_codes)])
        if (!setequal(sdr_codes, selected_sdr())) {
          selected_sdr(sdr_codes)
        }
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
    }, ignoreNULL = FALSE)

    # Core preset
    observeEvent(input$preset_core, {
      iphra_try({

          selected(as.character(
            reference_objectives %>%
              dplyr::filter(core %in% c("Core")) %>%
              dplyr::pull(objective_code) %>%
              unique()
          ))
        },
      on_error = "warn",
      origin = iphra_txt("Preset: Core Objectives"),
      hint = iphra_txt("Check objective data structure or input binding if this fails.")
      )
    })

    # SDR Core preset
    observeEvent(input$preset_sdr_core, {
      iphra_try({

          selected_sdr(as.character(
            reference_objectives %>%
              dplyr::filter(core %in% c("Core")) %>%
              dplyr::pull(objective_code) %>%
              unique()
          ))
        },
      on_error = "warn",
      origin = iphra_txt("Preset SDR: Core Objectives"),
      hint = iphra_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })

    # Clear buttons
    observeEvent(input$clear_objectives, {
      selected(character(0))
    })

    observeEvent(input$clear_sdr_objectives, {
      selected_sdr(character(0))
    })

    # ---- Single observer: update the schema, then the SVG, then the display.
    # Both operations depend on selected()/selected_sdr(); keeping them in one
    # observer guarantees the SVG is rebuilt against the freshly-modified
    # schema instead of racing a separate observer.
    observeEvent(list(selected(), selected_sdr()), {
      iphra_try({

        # 1️⃣ VALIDATION & PRECONDITIONS

        result <- iphra_try_step({

          iphra_message(
            iphra_txt("Reactive update triggered for framework visualization."),
            origin = iphra_txt("Framework SVG Highlighter")
          )

        }, step = "mod_goals_server/observe/Validation")
        if (iphra_failed(result)) return(result)

        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY

        result <- iphra_try_step({

          sel     <- as.character(selected())
          sel_sdr <- as.character(selected_sdr())

          combined <- unique(c(sel, sel_sdr))

          # a) schema first, so the framework's internal state matches the
          #    codes we are about to render.
          framework$modify_adjusted_schema(combined)

          # b) push primary/secondary lists onto the framework.
          framework$set_primary_objectives(objective_codes = sel)
          framework$set_secondary_objectives(objective_codes = sel_sdr)

          # c) rebuild the SVG using the same character codes.
          framework$modify_adjusted_svg(
            primary_objective_codes   = sel,
            secondary_objective_codes = sel_sdr
          )

          # d) always re-read adjusted_svg from the framework so the UI
          #    reflects whatever the framework produced (including the
          #    unmodified base SVG when both selection sets are empty).
          svg_to_display(framework$adjusted_svg)

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
    }, ignoreNULL = FALSE)

  })
}
