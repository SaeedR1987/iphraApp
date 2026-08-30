# x_col_row <- function(...) {
#   inputs <- list(...)
#
#   shiny::fluidRow(
#     lapply(inputs, function(x) {
#       shiny::column(width = 12 / length(inputs), x)
#     })
#   )
# }

# Shared choices for select-multiple inputs
.type_emergency_choices <- c("Natural hazard", "Conflict", "Other")
.type_crisis_choices <- c("Sudden onset", "Slow onset", "Protracted")
.output_choices        <- c("Preliminary Presentation", "Technical Report", "Brief", "Factsheet", "Not Applicable")
.dissemination_choices <- c("In-Person", "Email", "Remote Presentation", "Not Applicable")
.access_choices        <- c("Public", "Bilateral", "Restricted")
.visibility_choices    <- c("Public", "Restricted", "Not Applicable")

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
            title = phrutils::phr_txt("Assessment Info"),
            br(),

            # Group 1
            shinyBS::bsCollapse(
              id = ns("assessment_info_collapse"),
              multiple = TRUE,
              open = NULL,

              shinyBS::bsCollapsePanel(
                title = phrutils::phr_txt("General Information"),
                style = "primary",

                div(
                  style = "overflow-x:auto;",

                  tags$table(
                    class = "table table-bordered",
                    style = "width:100%;",

                    tags$tr(
                      tags$th(colspan = 4,
                              style = "background-color:#f5f5f5;",
                              phrutils::phr_txt("Assessment Metadata")
                      )
                    ),

                    tags$tr(
                      tags$th(style = "width:20%;", phrutils::phr_txt("Assessment Title")),
                      tags$td(style = "width:30%;",
                              textInput(
                                ns("assessment_title"),
                                label = NULL,
                                width = "100%"
                              )
                      ),

                      tags$th(style = "width:20%;", phrutils::phr_txt("Country Name")),
                      tags$td(style = "width:30%;",
                              textInput(
                                ns("country_name"),
                                label = NULL,
                                width = "100%"
                              )
                      )
                    ),

                    tags$tr(
                      tags$th(phrutils::phr_txt("Month/Year")),
                      tags$td(
                        textInput(
                          ns("month_year"),
                          label = NULL,
                          width = "100%"
                        )
                      ),

                      tags$th(phrutils::phr_txt("Research Cycle ID")),
                      tags$td(
                        textInput(
                          ns("research_cycle_id"),
                          label = NULL,
                          width = "100%"
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(phrutils::phr_txt("Project Code")),
                      tags$td(
                        textInput(
                          ns("project_code"),
                          label = NULL,
                          width = "100%"
                        )
                      ),

                      tags$th(phrutils::phr_txt("Mandating Body")),
                      tags$td(
                        textInput(
                          ns("mandating_body"),
                          label = NULL,
                          width = "100%"
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(phrutils::phr_txt("Version Number")),
                      tags$td(
                        numericInput(
                          ns("version_number"),
                          label = NULL,
                          value = NA,
                          min = 0,
                          step = 1,
                          width = "100%"
                        )
                      ),

                      tags$th(phrutils::phr_txt("Release Date")),
                      tags$td(
                        dateInput(
                          ns("release_date"),
                          label = NULL,
                          value = NA,
                          width = "100%"
                        )
                      )
                    )
                  )
                )





              ),
              shinyBS::bsCollapsePanel(
                title = phrutils::phr_txt("Rationale, Goals, Population and Geographic Scope"),
                style = "primary",

                div(
                  style = "overflow-x:auto;",

                  tags$table(
                    class = "table table-bordered",
                    style = "width:100%;",

                    tags$tr(
                      tags$th(
                        colspan = 2,
                        style = "background-color:#f5f5f5;",
                        phrutils::phr_txt("Rationale for Assessment")
                      )
                    ),

                    tags$tr(
                      tags$td(
                        colspan = 2,
                        shiny::tagAppendAttributes(
                          textAreaInput(
                            ns("rationale"),
                            label = NULL,
                            rows = 3,
                            width = "100%"
                          ),
                          maxlength = 200
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(
                        colspan = 2,
                        style = "background-color:#f5f5f5;",
                        phrutils::phr_txt("Assessment Goals")
                      )
                    ),

                    tags$tr(
                      tags$td(
                        colspan = 2,

                        tags$h5(phrutils::phr_txt("Primary Goals")),

                        shiny::checkboxInput(
                          ns("goal_primary_1"),
                          label = phrutils::phr_txt(
                            "1. To understand the severity of public health needs in the target population."
                          ),
                          value = TRUE
                        ),

                        shiny::checkboxInput(
                          ns("goal_primary_2"),
                          label = phrutils::phr_txt(
                            "2. To identify initial public health priorities and service gaps for response."
                          ),
                          value = TRUE
                        ),
                        tags$script(
                          HTML(
                            sprintf(
                              "$('#%s').prop('disabled', true);
       $('#%s').prop('disabled', true);",
                              ns("goal_primary_1"),
                              ns("goal_primary_2")
                            )
                          )
                        ),

                        tags$hr(),

                        tags$h5(phrutils::phr_txt("Secondary Goals")),

                        shiny::checkboxInput(
                          ns("goal_secondary_impact"),
                          label = phrutils::phr_txt(
                            "3. To inform the IMPACT acute needs analysis."
                          ),
                          value = FALSE
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(
                        colspan = 2,
                        style = "background-color:#f5f5f5;",
                        phrutils::phr_txt("Geographic Scope")
                      )
                    ),

                    tags$tr(
                      tags$td(
                        colspan = 2,
                        shiny::tagAppendAttributes(
                          textAreaInput(
                            ns("geographic_coverage"),
                            label = NULL,
                            rows = 3,
                            width = "100%"
                          ),
                          maxlength = 200
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(
                        colspan = 2,
                        style = "background-color:#f5f5f5;",
                        phrutils::phr_txt("Population and Stratification Plan")
                      )
                    ),

                    tags$tr(
                      tags$th(
                        style = "width:25%;",
                        phrutils::phr_txt("Type of Emergency")
                      ),

                      tags$th(
                        style = "width:25%;",
                        phrutils::phr_txt("Type of Crisis")
                      )
                    ),

                    tags$tr(
                      tags$td(
                        selectInput(
                          ns("type_emergency"),
                          label = NULL,
                          choices = .type_emergency_choices,
                          multiple = TRUE,
                          width = "100%"
                        )
                      ),
                      tags$td(
                        selectInput(
                          ns("type_crisis"),
                          label = NULL,
                          choices = .type_crisis_choices,
                          multiple = TRUE,
                          width = "100%"
                        )
                      )
                    ),

                    tags$tr(
                      tags$th(
                        phrutils::phr_txt("Population")
                      ),

                      tags$th(
                        phrutils::phr_txt("Stratification")
                      )
                    ),

                    tags$tr(
                      tags$td(
                        textInput(
                          ns("population"),
                          label = NULL,
                          width = "100%"
                        )
                      ),

                      tags$td(
                        textInput(
                          ns("stratification"),
                          label = NULL,
                          width = "100%"
                        )
                      )
                    )
                  )
                )


              ),
              shinyBS::bsCollapsePanel(
                title = phrutils::phr_txt("Timelines and Milestones"),
                style = "primary",

                div(
                  style = "width:100%;",

                  tags$table(
                    class = "table table-bordered table-striped",
                    style = "width:100%;",

                    tags$thead(
                      tags$tr(
                        tags$th(
                          style = "width:20%;",
                          phrutils::phr_txt("Phase")
                        ),
                        tags$th(
                          style = "width:35%;",
                          phrutils::phr_txt("Milestone")
                        ),
                        tags$th(
                          style = "width:45%;",
                          phrutils::phr_txt("Date")
                        )
                      )
                    ),

                    tags$tbody(

                      # ------------------------------
                      # Data Collection
                      # ------------------------------
                      tags$tr(
                        tags$td(
                          rowspan = 3,
                          strong(phrutils::phr_txt("Data Collection"))
                        ),
                        tags$td(phrutils::phr_txt("Pilot Training")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_pilot_training"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Data Collection Start")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_data_collection_start"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Data Collection End")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_data_collection_end"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      # ------------------------------
                      # Data Analysis
                      # ------------------------------
                      tags$tr(
                        tags$td(
                          rowspan = 2,
                          strong(phrutils::phr_txt("Data Analysis"))
                        ),
                        tags$td(phrutils::phr_txt("Data Analysis")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_data_analysis"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Data Validation")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_data_validation"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      # ------------------------------
                      # Output Production
                      # ------------------------------
                      tags$tr(
                        tags$td(
                          rowspan = 4,
                          strong(phrutils::phr_txt("Output Production"))
                        ),
                        tags$td(phrutils::phr_txt("Preliminary Presentation")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_preliminary_presentation"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Outputs Validation")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_outputs_validation"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Outputs Publication")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_outputs_publication"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      ),

                      tags$tr(
                        tags$td(phrutils::phr_txt("Final Presentation")),
                        tags$td(
                          shiny::dateInput(
                            ns("date_final_presentation"),
                            label = NULL,
                            value = NA,
                            width = "100%"
                          )
                        )
                      )

                    )
                  )
                )
              ),
              shinyBS::bsCollapsePanel(
                title = phrutils::phr_txt("Audience, Outputs, Dissemination Plan"),
                style = "primary",

                fluidRow(
                  column(
                    6,
                    actionButton(
                      ns("add_audience"),
                      phrutils::phr_txt("Add Audience")
                    )
                  ),
                  column(
                    6,
                    actionButton(
                      ns("remove_audience"),
                      phrutils::phr_txt("Remove Audience")
                    )
                  )
                ),

                br(),

                rhandsontable::rHandsontableOutput(
                  ns("audience_table")
                )

              )
            )

          ),

          # ---- Primary Tab ----
          shiny::tabPanel(
            title = phrutils::phr_txt("Primary"),
            shinydashboard::box(
              title = phrutils::phr_txt("Objectives Presets"),
              width = 12,
              shiny::actionButton(ns("preset_core"), phrutils::phr_txt("Core")),
              shiny::actionButton(
                ns("clear_objectives"),
                phrutils::phr_txt("Clear Objectives"),
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
            title = phrutils::phr_txt("Secondary"),
            shinyBS::bsCollapse(
              id = ns("secondary_data_sources_collapse"),
              shinyBS::bsCollapsePanel(
                title = phrutils::phr_txt("Secondary Data Sources"),
                style = "primary",

                shiny::actionButton(
                  ns("refresh_secondary_sources"),
                  phrutils::phr_txt("Refresh")
                ),

                shiny::br(),
                shiny::br(),

                rhandsontable::rHandsontableOutput(ns("secondary_sources_table"))
              )
            ),
            shinydashboard::box(
              title = phrutils::phr_txt("Objectives Presets"),
              width = 12,
              shiny::actionButton(ns("preset_sdr_core"), phrutils::phr_txt("Core")),
              shiny::actionButton(
                ns("clear_sdr_objectives"),
                phrutils::phr_txt("Clear Objectives"),
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
            title = phrutils::phr_txt("Full Objectives"),
            shiny::h4(phrutils::phr_txt("Goal Statements")),
            shiny::p(phrutils::phr_txt("1. To understand the severity of public health needs in the target population.")),
            shiny::p(phrutils::phr_txt("2. To identify initial public health priorities and service gaps for response.")),
            shiny::hr(),
            shiny::h4(phrutils::phr_txt("Selected Objectives")),
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
          phrutils::phr_txt("Unselected"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:lightgreen; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          phrutils::phr_txt("Primary Sources"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:lightblue; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          phrutils::phr_txt("Secondary Sources"),
          tags$span(
            style = "display:inline-block; width:15px; height:15px; background:#D8BFD8; border:1px solid #ccc; margin-left:15px; margin-right:5px;"
          ),
          phrutils::phr_txt("Both")
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

    protocol_r <- phr_get_module_reactive("protocol", session)

    # Setup Reference Objectives and Lookups
    # protocol <- protocol_r()
    # framework <- protocol$access_nested("framework")
    # reference_objectives <- framework$master_objectives_schema

    # Reactive reference objectives
    reference_objectives_r <- reactive({

      protocol_r()$framework$master_objectives_schema

      # reference_objectives$objective_code

    })

    # ---- Lookup maps between objective_code and short_objective

    unique_objectives_r <- reactive({
      objs <- reference_objectives_r()
      objs[!duplicated(objs$objective_code), ]
    })

    code_to_short_r <- reactive({
      uo <- unique_objectives_r()

      setNames(
        as.character(uo$short_objective),
        as.character(uo$objective_code)
      )
    })

    short_to_code_r <- reactive({
      uo <- unique_objectives_r()

      setNames(
        as.character(uo$objective_code),
        as.character(uo$short_objective)
      )
    })

    code_to_text_r <- reactive({
      uo <- unique_objectives_r()

      setNames(
        as.character(uo$text_objective),
        as.character(uo$objective_code)
      )
    })

    # --- SVG with individual block IDs ---

    # Initializing Reactive Values ####

    audience_table_data <- reactiveVal(
      data.frame(
        AudienceType = NA_character_,
        Audience = NA_character_,
        ExpectedOutputs = NA_character_,
        OutputCounts = NA_real_,
        Dissemination = NA_character_,
        Access = NA_character_,
        Visibility = NA_character_,
        stringsAsFactors = FALSE
      )
    )

    selected <- shiny::reactiveVal(character(0))
    selected_sdr <- shiny::reactiveVal(character(0))

    secondary_sources_data <- shiny::reactiveVal(
      data.frame(
        Objective = character(0),
        Source = character(0),
        Purpose = character(0),
        stringsAsFactors = FALSE
      )
    )

    # Primary Objective Selection

    all_objectives <- reactive({
      as.character(unique(reference_objectives_r()$objective_code))
    })

    filtered_available_objectives <- reactive({
      req(input$dynamic_select)  # Ensure input is available

      filtered <- reference_objectives_r()[reference_objectives_r()$pillar %in% input$dynamic_select, ]
      as.character(unique(filtered$objective_code))

    })

    filtered_available_sdr_objectives <- reactive({
      req(input$dynamic_select_sdr)  # Ensure input is available

      filtered <- reference_objectives_r()[reference_objectives_r()$pillar %in% input$dynamic_select_sdr, ]
      as.character(unique(filtered$objective_code))

    })


    # Outputs ####

    output$dynamic_select_ui <- renderUI({

      selectInput(
        ns("dynamic_select"),
        label = phrutils::phr_txt("Select Pillars"),
        choices = unique(reference_objectives_r()$pillar),  # Replace with your reactive or static vector
        selected = isolate(input$dynamic_select %||%
                           c("Demographics", "HealthStatus")),
        multiple = TRUE
      )
    })

    output$dynamic_select_sdr_ui <- renderUI({

      selectInput(
        ns("dynamic_select_sdr"),
        label = phrutils::phr_txt("Select Dimensions"),
        choices = unique(reference_objectives_r()$pillar),  # Replace with your reactive or static vector
        selected = isolate(input$dynamic_select_sdr %||%
                             c("Demographics", "HealthStatus")),
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

      labels <- unname(code_to_short_r()[as.character(available_codes)])
      labels <- labels[!is.na(labels)]

      sortable::rank_list(
        text = phrutils::phr_txt("Available Objectives"),
        labels = labels,
        input_id = ns("available"),
        options = sortable::sortable_options(group = "all_objectives")
      )

    })

    # ---- UI for selected list
    output$selected_ui <- shiny::renderUI({
      labels <- unname(code_to_short_r()[as.character(selected())])
      labels <- labels[!is.na(labels)]
      sortable::rank_list(
        text = phrutils::phr_txt("Selected Objectives"),
        labels = labels,
        input_id = ns("selected"),
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    output$available_sdr_ui <- shiny::renderUI({
      available_sdr_codes <- setdiff(filtered_available_sdr_objectives(), selected_sdr())
      labels_sdr <- unname(code_to_short_r()[as.character(available_sdr_codes)])
      labels_sdr <- labels_sdr[!is.na(labels_sdr)]
      sortable::rank_list(
        text = phrutils::phr_txt("Available Objectives"),
        labels = labels_sdr,
        input_id = ns("available_sdr"),
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    # ---- UI for selected list
    output$selected_sdr_ui <- shiny::renderUI({
      labels_sdr <- unname(code_to_short_r()[as.character(selected_sdr())])
      labels_sdr <- labels_sdr[!is.na(labels_sdr)]
      sortable::rank_list(
        text = phrutils::phr_txt("Selected Objectives"),
        labels = labels_sdr,
        input_id = ns("selected_sdr"),
        options = sortable::sortable_options(group = "all_objectives")
      )
    })

    # ---- Table of secondary data sources
    output$secondary_sources_table <- rhandsontable::renderRHandsontable({

      df <- secondary_sources_data()

      rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        stretchH = "all"
      ) |>
        rhandsontable::hot_col("Objective", readOnly = TRUE) |>
        rhandsontable::hot_col("Source", readOnly = FALSE) |>
        rhandsontable::hot_col("Purpose", readOnly = FALSE)

    })

    # for full text objectives preview
    output$full_objectives_ui <- renderUI({

      sel <- as.character(selected())
      sel_sdr <- as.character(selected_sdr())

      prim_obj <- reference_objectives_r() |>
        dplyr::filter(objective_code %in% sel) |>
        dplyr::pull(text_objective) |> unique()

      sdr_obj <- reference_objectives_r() |>
        dplyr::filter(objective_code %in% sel_sdr) |>
        dplyr::pull(text_objective) |> unique()

      objs <- c(prim_obj, sdr_obj)


      if (length(objs) == 0) {
        shiny::em(phrutils::phr_txt("No objectives selected."))
      } else {
        tags$ul(
          lapply(objs, tags$li)
        )
      }
    })

    output$audience_table <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(
        audience_table_data(),
        stretchH = "all"
      ) |>

        rhandsontable::hot_col(
          "AudienceType",
          type = "dropdown",
          source = c(
            "Strategic",
            "Coordination/Cluster",
            "Government Agency",
            "Donor",
            "Operational Actor",
            "Community",
            "Other"
          )
        ) |>

        rhandsontable::hot_col(
          "ExpectedOutputs",
          type = "dropdown",
          source = .output_choices
        ) |>

        rhandsontable::hot_col(
          "OutputCounts",
          type = "numeric"
        ) |>

        rhandsontable::hot_col(
          "Dissemination",
          type = "dropdown",
          source = .dissemination_choices
        ) |>

        rhandsontable::hot_col(
          "Access",
          type = "dropdown",
          source = .access_choices
        ) |>

        rhandsontable::hot_col(
          "Visibility",
          type = "dropdown",
          source = .visibility_choices
        )

    })


    # --- Render the initial SVG ---
    output$framework_svg <- renderUI({
      HTML(protocol_r()$framework$adjusted_svg)
    })

    # Observes ####

    observeEvent(input$goal_secondary_impact, {

      protocol_r()$set(
        field = "framework",
        role = "secondary_ana_goal",
        value = isTRUE(
          input$goal_secondary_impact
        )
      )

      phr_touch_module("protocol", session)

    }, ignoreInit = FALSE)

    observeEvent(input$add_audience, {

      df <- audience_table_data()

      df <- rbind(
        df,
        data.frame(
          AudienceType = NA_character_,
          Audience = NA_character_,
          ExpectedOutputs = NA_character_,
          OutputCounts = NA_real_,
          Dissemination = NA_character_,
          Access = NA_character_,
          Visibility = NA_character_,
          stringsAsFactors = FALSE
        )
      )

      audience_table_data(df)

    })

    observeEvent(input$remove_audience, {

      df <- audience_table_data()

      if (nrow(df) > 1) {
        audience_table_data(
          df[-nrow(df), , drop = FALSE]
        )
      }

    })

    observe({

      protocol_r()$set(
        field = "metadata",
        role = "audience_matrix",
        value = audience_table_data()
      )

    })

    # ---- Assessment Info metadata observers
    # Character fields (groups 1 & 3)
    local({
      char_fields <- c(
        "country_name", "country", "month_year", "research_cycle_id",
        "assessment_title", "type_emergency", "type_crisis", "population",
        "rationale", "geographic_coverage", "stratification", "mandating_body",
        "project_code", "audience_type_cluster"
      )
      for (fld in char_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol_r()$set(field = "metadata", role = f, value = input[[f]])
            phr_touch_module("protocol")
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # Date fields (groups 1 & 2)
    local({
      date_fields <- c(
        "release_date",
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
            protocol_r()$set(field = "metadata", role = f, value = input[[f]])
            phr_touch_module("protocol")
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

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
            protocol_r()$set(field = "metadata", role = f, value = input[[f]])
            phr_touch_module("protocol")
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # Numeric fields (groups 1 & 7)
    local({
      num_fields <- c(
        "version_number",
        "num_report", "num_profile", "num_prelim_presentation",
        "num_final_presentation", "num_factsheet", "num_dashboard",
        "num_webmap", "num_map", "num_output_other"
      )
      for (fld in num_fields) {
        local({
          f <- fld
          observeEvent(input[[f]], {
            protocol_r()$set(field = "metadata", role = f, value = input[[f]])
            phr_touch_module("protocol")
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
        })
      }
    })

    # ---- Restore UI state when a project file is loaded ----
    # Fires each time `flags$project_loaded` is incremented by
    # `iphra_load_project_file()`. All reactive reads inside are wrapped in
    # `isolate()` so this observer only re-runs on explicit loads, not on
    # every protocol mutation during normal use.
    observeEvent(session$userData$flags$project_loaded, {
      req(isolate(session$userData$flags$project_loaded) > 0)

      proto <- isolate(protocol_r())
      if (is.null(proto)) return()

      meta <- proto$metadata

      # --- Restore metadata text inputs ---
      if (!is.null(meta)) {
        char_fields_restore <- c(
          "country_name", "country", "month_year", "research_cycle_id",
          "assessment_title", "type_emergency", "type_crisis", "population",
          "rationale", "geographic_coverage", "stratification", "mandating_body",
          "project_code", "audience_type_cluster"
        )
        for (f in char_fields_restore) {
          val <- meta[[f]]
          if (!is.null(val) && is.character(val))
            updateTextInput(session, f, value = val)
        }

        date_fields_restore <- c(
          "release_date",
          "overall_timeframe", "date_pilot_training", "date_data_collection_start",
          "date_data_collection_end", "date_data_analysis", "date_data_validation",
          "date_preliminary_presentation", "date_outputs_validation",
          "date_outputs_publication", "date_final_presentation",
          "date_milestone_donor", "date_milestone_intercluster",
          "date_milestone_cluster", "date_milestone_ngo_platform",
          "date_milestone_other"
        )
        for (f in date_fields_restore) {
          val <- meta[[f]]
          if (!is.null(val))
            updateDateInput(session, f, value = val)
        }

        multi_fields_restore <- c(
          "expected_output_cluster", "expected_output_donor",
          "expected_output_operational_actor", "expected_output_other",
          "dissemination_strategy_cluster", "dissemination_strategy_donor",
          "dissemination_strategy_operational_actor", "dissemination_strategy_other",
          "access_cluster", "access_donor", "access_operational_actor", "access_other",
          "visibility_cluster", "visibility_donor", "visibility_operational_actor",
          "visibility_other"
        )
        for (f in multi_fields_restore) {
          val <- meta[[f]]
          if (!is.null(val))
            updateSelectInput(session, f, selected = val)
        }

        num_fields_restore <- c(
          "version_number",
          "num_report", "num_profile", "num_prelim_presentation",
          "num_final_presentation", "num_factsheet", "num_dashboard",
          "num_webmap", "num_map", "num_output_other"
        )
        for (f in num_fields_restore) {
          val <- meta[[f]]
          if (!is.null(val) && is.numeric(val))
            updateNumericInput(session, f, value = val)
        }
      }

      # --- Restore primary and secondary objectives ---
      # `proto` is a plain R6 snapshot obtained above via isolate(); its
      # fields (including framework$primary_objectives) are not reactive, so
      # these accesses do not create any reactive dependencies.
      prim_codes <- as.character(
        proto$framework$primary_objectives %||% integer(0)
      )
      selected(prim_codes)

      sec_codes <- as.character(
        proto$framework$secondary_objectives %||% integer(0)
      )
      selected_sdr(sec_codes)

      # --- Restore pillar filter selects so the available pool matches ---
      # (dynamic_select_ui re-renders before this observer fires, so
      #  updateSelectInput patches the already-recreated DOM element.)
      ref <- isolate(reference_objectives_r())
      if (!is.null(ref) && nrow(ref) > 0) {
        prim_pillars <- unique(ref$pillar[ref$objective_code %in% prim_codes])
        sec_pillars  <- unique(ref$pillar[ref$objective_code %in% sec_codes])
        if (length(prim_pillars) > 0)
          updateSelectInput(session, "dynamic_select", selected = prim_pillars)
        if (length(sec_pillars) > 0)
          updateSelectInput(session, "dynamic_select_sdr", selected = sec_pillars)
      }

      updateCheckboxInput(
        session,
        "goal_secondary_impact",
        value = isTRUE(proto$framework$secondary_ana_goal)
      )

      if (!is.null(proto$metadata$audience_matrix)) {
        audience_table_data(
          proto$metadata$audience_matrix
        )
      }

      if (!is.null(proto$framework$secondary_data_sources)) {

        restored <- proto$framework$secondary_data_sources

        secondary_sources_data(
          data.frame(
            Objective = restored$objective,
            Source = restored$source,
            Purpose = restored$purpose,
            stringsAsFactors = FALSE
          )
        )

      }

    }, ignoreInit = TRUE)

    # ---- Keep selected() in sync with drag-and-drop
    observeEvent(input$selected, {
      iphra_try({

        codes <- unname(short_to_code_r()[as.character(input$selected)])
        codes <- as.character(codes[!is.na(codes)])
        # Only update if the set actually changed to avoid feedback loops
        # with the renderUI that rebuilds the rank_list.
        if (!setequal(codes, selected())) {
          selected(codes)
        }

        protocol_r()$framework$set_primary_objectives(objective_codes = codes)
        phr_touch_module("protocol", session)

        iphra_message(
          paste0(
            phrutils::phr_txt("Selected item(s) updated to: "),
            paste(input$selected, collapse = ", ")
          ),
          origin = phrutils::phr_txt("Selection Update")
        )
        },
      on_error = "warn",
      origin = phrutils::phr_txt("Selection Update"),
      hint = phrutils::phr_txt("Check input binding or reactive assignment if this fails.")
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$selected_sdr, {
      iphra_try({

        sdr_codes <- unname(short_to_code_r()[as.character(input$selected_sdr)])
        sdr_codes <- as.character(sdr_codes[!is.na(sdr_codes)])
        if (!setequal(sdr_codes, selected_sdr())) {
          selected_sdr(sdr_codes)
        }

        protocol_r()$framework$set_secondary_objectives(objective_codes = sdr_codes)
        phr_touch_module("protocol", session)

        iphra_message(
          paste0(
            phrutils::phr_txt("SDR selection updated to: "),
            paste(input$selected_sdr, collapse = ", ")
          ),
          origin = phrutils::phr_txt("SDR selection Update")
        )
      },
      on_error = "warn",
      origin = phrutils::phr_txt("SDR Selection Update"),
      hint = phrutils::phr_txt("Check input binding or reactive assignment if this fails.")
      )
    }, ignoreNULL = FALSE)

    # Core preset
    observeEvent(input$preset_core, {
      iphra_try({

          # selected(as.character(
          #   reference_objectives |>
          #     dplyr::filter(core %in% c("Core")) |>
          #     dplyr::pull(objective_code) |>
          #     unique()
          # ))

        selected(c("101", "102", "103", "104"))

        protocol_r()$framework$set_primary_objectives(objective_codes = c("101", "102", "103", "104"))
        phr_touch_module("protocol", session)


        },
      on_error = "warn",
      origin = phrutils::phr_txt("Preset: Core Objectives"),
      hint = phrutils::phr_txt("Check objective data structure or input binding if this fails.")
      )
    })

    # SDR Core preset
    observeEvent(input$preset_sdr_core, {
      iphra_try({

          # selected_sdr(as.character(
          #   reference_objectives |>
          #     dplyr::filter(core %in% c("Core")) |>
          #     dplyr::pull(objective_code) |>
          #     unique()
          # ))

        selected_sdr(c("101", "102", "103", "104"))

        protocol_r()$framework$set_secondary_objectives(objective_codes = c("101", "102", "103", "104"))
        phr_touch_module("protocol", session)

        },
      on_error = "warn",
      origin = phrutils::phr_txt("Preset SDR: Core Objectives"),
      hint = phrutils::phr_txt("Check objective data structure or SDR input binding if this fails.")
      )
    })

    # Clear buttons
    observeEvent(input$clear_objectives, {
      selected(character(0))
    })

    observeEvent(input$clear_sdr_objectives, {
      selected_sdr(character(0))
    })

    # ---- Sync the secondary data sources table onto the stored protocol
    # object. Because Framework only exposes row-level add/remove methods,
    # every existing row is removed and every current row (with a non-blank
    # Source) is re-added, one access_nested() call per row.
    sync_secondary_sources_to_protocol <- function(new_df) {
      iphra_try({

        existing_df <- protocol_r()$framework$secondary_data_sources

        if (!is.null(existing_df) && nrow(existing_df) > 0) {
          for (i in seq_len(nrow(existing_df))) {
            protocol_r()$access_nested(
              field = "framework",
              member = "remove_secondary_data_source",
              objective = existing_df$objective[i],
              source = existing_df$source[i]
            )
          }
        }

        if (!is.null(new_df) && nrow(new_df) > 0) {
          for (i in seq_len(nrow(new_df))) {
            source_val <- trimws(new_df$Source[i] %||% "")
            if (source_val == "") next

            protocol_r()$access_nested(
              field = "framework",
              member = "add_secondary_data_source",
              objective = new_df$Objective[i],
              source = new_df$Source[i],
              purpose = new_df$Purpose[i] %||% NA_character_
            )
          }
        }

        phr_touch_module("protocol", session)

      },
      on_error = "warn",
      origin = phrutils::phr_txt("Secondary Data Sources: Sync"),
      hint = phrutils::phr_txt("Check Framework$secondary_data_sources access_nested calls if this fails.")
      )
    }

    # ---- Refresh button: rebuild the Objective column from the selected
    # secondary objectives, preserving any Source/Purpose already entered
    # for objectives that remain selected.
    observeEvent(input$refresh_secondary_sources, {
      iphra_try({

        target_labels <- unname(code_to_text_r()[as.character(selected_sdr())])
        target_labels <- target_labels[!is.na(target_labels)]

        old_df <- secondary_sources_data()

        new_df <- data.frame(
          Objective = target_labels,
          Source = NA_character_,
          Purpose = NA_character_,
          stringsAsFactors = FALSE
        )

        if (nrow(new_df) > 0 && nrow(old_df) > 0) {
          match_idx <- match(new_df$Objective, old_df$Objective)
          keep <- !is.na(match_idx)
          new_df$Source[keep] <- old_df$Source[match_idx[keep]]
          new_df$Purpose[keep] <- old_df$Purpose[match_idx[keep]]
        }

        secondary_sources_data(new_df)
        sync_secondary_sources_to_protocol(new_df)

      },
      on_error = "warn",
      origin = phrutils::phr_txt("Secondary Data Sources: Refresh"),
      hint = phrutils::phr_txt("Check selected secondary objectives if this fails.")
      )
    })

    # ---- Keep secondary_sources_data() and the protocol object in sync with
    # manual edits made directly in the rhandsontable widget.
    observeEvent(input$secondary_sources_table, {
      iphra_try({

        new_df <- rhandsontable::hot_to_r(input$secondary_sources_table)

        secondary_sources_data(new_df)
        sync_secondary_sources_to_protocol(new_df)

      },
      on_error = "warn",
      origin = phrutils::phr_txt("Secondary Data Sources: Edit"),
      hint = phrutils::phr_txt("Ensure rhandsontable input is correctly bound if this fails.")
      )
    }, ignoreInit = TRUE)

    # ---- Single observer: update the schema, then the SVG, then the display.
    # Both operations depend on selected()/selected_sdr(); keeping them in one
    # observer guarantees the SVG is rebuilt against the freshly-modified
    # schema instead of racing a separate observer.
    observeEvent(list(selected(), selected_sdr()), {
      iphra_try({

        # 1️⃣ VALIDATION & PRECONDITIONS

        result <- iphra_try_step({

          iphra_message(
            phrutils::phr_txt("Reactive update triggered for framework visualization."),
            origin = phrutils::phr_txt("Framework SVG Highlighter")
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

          protocol_r()$framework$modify_adjusted_schema(combined)
          phr_touch_module("protocol")

          # b) push primary/secondary lists onto the framework.
          protocol_r()$framework$set_primary_objectives(objective_codes = sel)
          protocol_r()$framework$set_secondary_objectives(objective_codes = sel_sdr)
          phr_touch_module("protocol")

          # c) rebuild the SVG using the same character codes.
          protocol_r()$framework$modify_adjusted_svg(
            primary_objective_codes   = sel,
            secondary_objective_codes = sel_sdr
          )

          phr_touch_module("protocol")

        }, step = "mod_goals_server/observe/Core Logic")
        if (iphra_failed(result)) return(result)

        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS

        result <- iphra_try_step({

          iphra_message(
            phrutils::phr_txt("Framework visualization updated successfully."),
            origin = phrutils::phr_txt("Framework SVG Highlighter")
          )
        }, step = "mod_goals_server/observe/Result Handling")
        if (iphra_failed(result)) return(result)

        },
      on_error = "warn",
      origin = phrutils::phr_txt("Framework SVG Highlighter"),
      hint = phrutils::phr_txt("Check reactive dependencies or JavaScript message binding if this fails.")
      )
    }, ignoreNULL = FALSE)

  })
}
