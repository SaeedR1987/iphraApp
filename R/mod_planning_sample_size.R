#' planning_sample_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_planning_sample_size_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shinyjs::useShinyjs(),
    br(),


    # MAIN SPLIT LAYOUT

    fluidRow(


      # LEFT SIDE (3/4 WIDTH): Tabset

      column(width = 9,

             tabsetPanel(
               id = ns("sample_tabs"),
               type = "tabs",

               # TAB 1: SAMPLE SIZE

               tabPanel(
                 title = "Sample Size",
                 br(),

                 # Top-level controls
                 fluidRow(
                   column(2,
                          textInput(ns("population_name"), "Population Name:",
                                    value = "Population A", width = "100%")
                   ),
                   column(2,
                          numericInput(ns("total_households"), "Total Households:",
                                       value = 1000, min = 1, width = "100%"),
                          numericInput(ns("total_population"), "Total Population:",
                                       value = 5000, min = 1, width = "100%")
                   ),
                   column(2,
                          numericInput(ns("n_sites"), "Number of Sites to Sample (Non-Cluster sampling):",
                                       value = 100, min = 1, width = "100%")

                   ),
                   column(2,
                          br(), br(),
                          radioButtons(
                            ns("sampling_method_site"),
                            "Site/Cluster Sampling Method:",
                            choices = c(
                              "Simple Random Sampling (SRS)" = "simple_random",
                              "Proportional" = "proportional",
                              "Cluster (Probability Proportional to Size (PPS) w/replacement)" = "cluster",
                              "Systematic" = "systematic"
                            ),
                            inline = FALSE
                          ),
                          radioButtons(
                            ns("sampling_method_hh"),
                            "Household Sampling Method:",
                            choices = c(
                              "Simple Random Sampling (SRS)" = "simple_random",
                              "Systematic" = "systematic",
                              "Random Location Clusters (RLC)" = "rlc"
                            ),
                            inline = FALSE
                          ),
                   ),
                   column(
                     4,
                     div(
                       fluidRow(
                         column(
                           width = 12,
                           checkboxGroupInput(
                             ns("sample_to_add"),
                             "Select Calculation(s) to Add:",
                             choices = c(
                               "Household" = "household",
                               "Individual" = "individual",
                               "Rate" = "rate",
                               "Plan" = "plan"
                             ),
                             selected = "household",
                             inline = TRUE
                           )
                         )
                       ),
                       fluidRow(
                         column(
                           6,
                           actionButton(
                             ns("add_sample"),
                             "Add Sample",
                             class = "btn-primary btn-sm",
                             width = "100%"
                           )
                         )
                       )
                     )
                   )
                 ),

                 hr(),

                 # Four planning calculators
                 fluidRow(

                   # Household-level
                   column(width = 3, style = "min-width: 300px;",
                          wellPanel(
                            style = "padding: 10px; font-size: 12px;",
                            h4("Household-level Sample Size", style = "font-size:14px;"),
                            selectInput(ns("pop_indicator"), "Select Indicator:",
                                        choices = c("General", "HHS")),
                            numericInput(ns("pop_expected_prevalence"),
                                         "Expected Prevalence (%):", value = 10, min = 0, max = 100),
                            numericInput(ns("pop_precision"), "Desired Precision (%):",
                                         value = 5, min = 0.1, max = 50),
                            numericInput(ns("pop_nonresponse"), "Non-response Rate (%):",
                                         value = 10, min = 0, max = 100),
                            numericInput(ns("pop_design_effect"), "Design Effect:",
                                         value = 1, min = 1),
                            checkboxInput(ns("pop_fpc"), "Apply FPC?", value = FALSE),
                            actionButton(ns("pop_calculate"), "Calculate"),
                            h5("Result", style = "margin-top:10px;"),
                            verbatimTextOutput(ns("pop_result"))

                          )
                   ),

                   # Individual-level
                   column(width = 3, style = "min-width: 300px;",
                          wellPanel(
                            style = "padding: 10px; font-size: 12px;",
                            h4("Individual-level Sample Size", style = "font-size:14px;"),
                            selectInput(ns("ind_indicator"), "Select Indicator:",
                                        choices = c("General", "GAM by MUAC")),
                            numericInput(ns("ind_expected_prevalence"),
                                         "Expected Prevalence (%):", value = 10, min = 0, max = 100),
                            numericInput(ns("ind_precision"), "Desired Precision (%):",
                                         value = 5, min = 0.1, max = 50),
                            numericInput(ns("ind_nonresponse"), "Non-response Rate (%):",
                                         value = 10, min = 0, max = 100),
                            numericInput(ns("ind_design_effect"), "Design Effect:",
                                         value = 1, min = 1),
                            numericInput(ns("ind_avg_hh_size"), "Average Household Size:",
                                         value = 5, min = 1),
                            numericInput(ns("ind_subpop_prop"),
                                         "Proportion of Sub-population (%):", value = 100, min = 0, max = 100),
                            checkboxInput(ns("ind_fpc"), "Apply FPC?", value = FALSE),
                            actionButton(ns("ind_calculate"), "Calculate"),
                            h5("Result", style = "margin-top:10px;"),

                            tags$b("Total People"),
                            verbatimTextOutput(ns("ind_total_people")),

                            tags$b("Total Households"),
                            verbatimTextOutput(ns("ind_total_households"))
                          )
                   ),

                   # Rate-level
                   column(width = 3, style = "min-width: 300px;",
                          wellPanel(
                            style = "padding: 10px; font-size: 12px;",
                            h4("Rate Sample Size", style = "font-size:14px;"),
                            selectInput(ns("rate_indicator"), "Select Indicator:",
                                        choices = c("Crude Death Rate")),
                            numericInput(ns("rate_expected_rate"),
                                         "Expected Death Rate:", value = 0.5, min = 0, max = 100),
                            numericInput(ns("rate_precision"),
                                         "Desired Precision:", value = 0.1, min = 0.01, max = 50),
                            numericInput(ns("rate_nonresponse"),
                                         "Non-response Rate (%):", value = 10, min = 0, max = 100),
                            numericInput(ns("rate_design_effect"),
                                         "Design Effect:", value = 1, min = 1),
                            numericInput(ns("rate_recall_days"),
                                         "Number of Recall Days:", value = 30, min = 1),
                            numericInput(ns("rate_avg_hh_size"),
                                         "Average Household Size:", value = 5, min = 1),
                            checkboxInput(ns("rate_fpc"), "Apply FPC?", value = FALSE),
                            actionButton(ns("rate_calculate"), "Calculate"),
                            h5("Result", style = "margin-top:10px;"),

                            tags$b("Total Households"),
                            verbatimTextOutput(ns("rate_total_households")),

                            tags$b("Total People"),
                            verbatimTextOutput(ns("rate_total_people")),

                            tags$b("Total Person-Time"),
                            verbatimTextOutput(ns("rate_total_person_time"))
                          )
                   ),
                   # Plan level
                   column(
                     width = 3,
                     style = "min-width: 300px;",

                     wellPanel(
                       style = "padding: 10px; font-size: 12px;",

                       h4(
                         "Data Collection Planning",
                         style = "font-size:14px;"
                       ),

                       numericInput(
                         ns("sample_size_plan"),
                         "Total Sample Size",
                         value = 1,
                         min = 1
                       ),

                       numericInput(
                         ns("n_teams"),
                         "Number of Teams",
                         value = 2,
                         min = 1
                       ),

                       numericInput(
                         ns("n_enum"),
                         "Enumerators per Team",
                         value = 3,
                         min = 1
                       ),

                       numericInput(
                         ns("clusters_per_day"),
                         "Clusters per Day",
                         value = 2,
                         min = 1
                       ),

                       numericInput(
                         ns("interview_time"),
                         "Interview Time (minutes)",
                         value = 30,
                         min = 1
                       ),

                       numericInput(
                         ns("travel_time"),
                         "Travel Time (minutes)",
                         value = 30,
                         min = 0
                       ),

                       numericInput(
                         ns("rest_time"),
                         "Rest Time (minutes)",
                         value = 60,
                         min = 0
                       ),

                       textInput(
                         ns("start_time"),
                         "Start Time",
                         value = "08:00"
                       ),

                       textInput(
                         ns("end_time"),
                         "End Time",
                         value = "17:00"
                       ),

                       actionButton(
                         ns("calc_plan"),
                         "Calculate"
                       ),

                       h5(
                         "Result",
                         style = "margin-top:10px;"
                       ),

                       tags$b("Interviews / Enumerator / Day"),
                       verbatimTextOutput(ns("num_interview_per_enumday_result")),

                       tags$b("Estimated Days"),
                       verbatimTextOutput(ns("num_days_dc_result")),

                       tags$b("Recommended Cluster Size"),
                       verbatimTextOutput(ns("psu_size_result")),

                       tags$b("Recommended Clusters"),
                       verbatimTextOutput(ns("num_psu_needed_result"))
                     )
                   )

                 )
               ),


               # TAB 3: SAMPLING

               tabPanel(
                 title = "Sampling",
                 br(), br(), br(),

                 # ---- Main Flex Layout: Three-column structure ----
                 div(
                   style = "
      display: flex;
      flex-direction: row;
      justify-content: space-between;
      gap: 20px;
      align-items: flex-start;
      width: 100%;
      max-width: 100%;
      overflow-x: hidden;    /* prevents bleed into right section */
      box-sizing: border-box;
      flex-wrap: nowrap;
    ",

                   # Left column (≈25%)
                   div(
                     style = "flex: 0 0 25%; min-width: 260px; box-sizing: border-box;",
                     h4("Sampling Controls", style = "font-size:14px; font-weight:600; margin-bottom:10px;"),
                     fileInput(
                       ns("import_frame"),
                       "Import Sampling Frame",
                       accept = c(
                         ".csv",
                         ".xlsx",
                         ".xls"
                       ),
                       width = "100%"
                     ),
                     wellPanel(
                       style = "padding:10px; margin-top:10px;",
                       h5("Reserve Clusters", style = "font-weight:600;"),
                       checkboxInput(ns("include_reserves"), "Include reserve clusters", value = FALSE),
                       numericInput(ns("n_reserves"), "Number per strata", value = 0, min = 0)
                     ),
                     br(),
                     actionButton(ns("draw_sample"), "Draw Sample", class = "btn-success", width = "100%")
                   ),

                   # Middle column (≈37.5%)
                   div(
                     style = "flex: 0 0 37.5%; min-width: 380px; box-sizing: border-box;",
                     h4("Imported Sampling Frame", style = "font-size:14px; font-weight:600; margin-bottom:10px;"),
                     div(
                       style = "
          max-height: 500px;
          overflow-y: auto;
          overflow-x: auto;
          border: 1px solid #ddd;
          border-radius: 6px;
          padding: 5px;
          background: #fff;
        ",
                       rhandsontable::rHandsontableOutput(ns('sampling_frame'))
                     )
                   ),

                   # Right column (≈37.5%)
                   div(
                     style = "flex: 0 0 37.5%; min-width: 380px; box-sizing: border-box;",
                     h4("Sample Results", style = "font-size:14px; font-weight:600; margin-bottom:10px;"),
                     div(
                       style = "
          max-height: 500px;
          overflow-y: auto;
          overflow-x: auto;
          border: 1px solid #ddd;
          border-radius: 6px;
          padding: 5px;
          background: #fff;
        ",
                       rhandsontable::rHandsontableOutput(ns('sample_results'))
                     )
                   )
                 )
               )
             )
      ),


      # RIGHT SIDE (1/4 WIDTH): Always Visible Table

      column(width = 3, style = "min-width: 300px;",
             h4("Sample Table", style = "font-size:14px;"),

             fluidRow(
               column(
                 6,
                 selectInput(
                   ns("remove_target"),
                   "Select Population:",
                   choices = NULL,
                   width = "100%"
                 )
               ),
               column(
                 6,
                 br(),
                 actionButton(
                   ns("remove_sample"),
                   "Remove Row",
                   class = "btn-danger btn-sm",
                   width = "100%"
                 )
               )
             ),

             div(
               style = "max-height: 800px; overflow-y: auto;",
               rhandsontable::rHandsontableOutput(ns("samples_table"))
             )
      )
    )
  )
}

#' planning_sample_size Server Functions
#'
#' @noRd
mod_planning_sample_size_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    protocol_r <- phr_get_module_reactive("protocol", session)





    # INITIALIZATION

    updating_dropdown <- reactiveVal(FALSE)  # new flag to prevent re-entry loops

    # Initialize master data frame with all fields

    samples <- reactive({
      protocol_r()$sample_object$sample_table
    })

    sampling_frame_data <- reactive({
      protocol_r()$sampling_frame$get(field = "log_df")
    })

    observe({
      print("sampling_frame_data updated")
      print(dim(sampling_frame_data()))
    })

    sample_results_data <- reactive({
      protocol_r()$sampling_frame$drawn_sample_full
    })

    observe({

      x <- sample_results_data()

      cat("\n*** sample_results_data invalidated ***\n")

      if (is.null(x)) {
        cat("Current value: NULL\n")
      } else {
        cat("Rows:", nrow(x), "\n")
        cat("Cols:", ncol(x), "\n")
      }

    })

    pop_sample_size <- reactiveVal(NULL)

    ind_sample_size <- reactiveVal(NULL)

    rate_sample_size <- reactiveVal(NULL)

    num_interview_per_enumday <- reactiveVal(NULL)
    num_days_dc <- reactiveVal(NULL)
    num_psu_needed <- reactiveVal(NULL)
    psu_size <- reactiveVal(NULL)

    # RENDER filtered table for display

    output$samples_table <- rhandsontable::renderRHandsontable({

      tbl <- samples()

      req(tbl, nrow(tbl) > 0)

      # Use stratum_id as column names
      stratum_names <- tbl$stratum_id

      # Remove stratum_id from data
      tbl_t <- tbl[, setdiff(names(tbl), "stratum_id"), drop = FALSE]

      # Transpose
      tbl_t <- as.data.frame(t(tbl_t), stringsAsFactors = FALSE)

      # Set column names from stratum_id
      colnames(tbl_t) <- stratum_names

      rhandsontable::rhandsontable(
        tbl_t,
        readOnly = TRUE,
        rowHeaders = rownames(tbl_t), rowHeaderWidth = 120
      )

    })

    output$pop_result <- renderText({
      req(pop_sample_size())
      format(pop_sample_size(), big.mark = ",")
    })

    # output$ind_result <- renderText({
    #   req(ind_sample_size())
    #   format(ind_sample_size(), big.mark = ",")
    # })
    #
    # output$rate_result <- renderText({
    #   req(rate_sample_size())
    #   format(rate_sample_size(), big.mark = ",")
    # })

    output$ind_total_people <- renderText({
      req(ind_sample_size())

      format(ind_sample_size()[1], big.mark = ",")
    })

    output$ind_total_households <- renderText({
      req(ind_sample_size())

      format(ind_sample_size()[2], big.mark = ",")
    })

    output$rate_total_households <- renderText({
      req(rate_sample_size())

      format(rate_sample_size()[1], big.mark = ",")
    })

    output$rate_total_people <- renderText({
      req(rate_sample_size())

      format(rate_sample_size()[2], big.mark = ",")
    })

    output$rate_total_person_time <- renderText({
      req(rate_sample_size())

      format(rate_sample_size()[3], big.mark = ",")
    })

    output$num_interview_per_enumday_result <- renderText({
      req(num_interview_per_enumday())
      format(num_interview_per_enumday(), big.mark = ",")
    })
    output$num_days_dc_result <- renderText({
      req(num_days_dc())
      format(num_days_dc(), big.mark = ",")
    })
    output$num_psu_needed_result <- renderText({
      req(num_psu_needed())
      format(num_psu_needed(), big.mark = ",")
    })
    output$psu_size_result <- renderText({
      req(psu_size())
      format(psu_size(), big.mark = ",")
    })

    # ▶️ OUTPUT - SAMPLING FRAME ####
    output$sampling_frame <- rhandsontable::renderRHandsontable({

      df <- sampling_frame_data()
      if (is.null(df)) return(NULL)

      rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 500
      ) |>
        rhandsontable::hot_cols(
          columnSorting = TRUE,
          stretchH = "all"
        )
    })

    # ▶️ OUTPUT - SAMPLE STRATUM TABLE ####
    output$sample_results <- rhandsontable::renderRHandsontable({

      cat("\nRendering sample_results\n")

      df <- sample_results_data()
      if (is.null(df)) {
        cat("sample_results received NULL\n")
        return(NULL)
      }

      print(dim(df))

      rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 500,
        readOnly = TRUE   # ← prevents direct editing by user
      ) |>
        rhandsontable::hot_cols(
          columnSorting = TRUE,
          stretchH = "all"
        )
    })

    observe({
      cat("log_df changed\n")
      print(dim(protocol_r()$sampling_frame$get("log_df")))
    })

    observe({
      cat("drawn_sample_full changed\n")
      print(dim(protocol_r()$sampling_frame$drawn_sample_full))
    })


    # ▶️ OBSERVE - ADD SAMPLE BUTTON ####

    observeEvent(input$add_sample, {
      iphra_try({

        protocol_r()$sample_object$add_stratum(
          # field = "sample_object",
          # member = "add_stratum",

          # Core stratum information
          stratum_id = input$population_name,
          stratum_name = input$population_name,
          population_size = input$total_population,
          total_households = input$total_households,

          # Sampling design
          sampling_method_site = input$sampling_method_site,
          sampling_method_hh = input$sampling_method_hh,

          # n_psu = NA_real_,
          # cluster_size = NA_real_,
          n_sites = input$n_sites,

          # Survey planning
          teams = if ("plan" %in% input$sample_to_add) input$n_teams else NA_real_,
          avg_interview_time = if ("plan" %in% input$sample_to_add) input$interview_time else NA_real_,
          clusters_per_day = if ("plan" %in% input$sample_to_add) input$clusters_per_day else NA_real_,
          enumerators_per_team = if ("plan" %in% input$sample_to_add) input$n_enum else NA_real_,
          avg_rest_time = if ("plan" %in% input$sample_to_add) input$rest_time else NA_real_,
          avg_travel_time = if ("plan" %in% input$sample_to_add) input$travel_time else NA_real_,
          start_time = if ("plan" %in% input$sample_to_add) input$start_time else NA_character_,
          end_time = if ("plan" %in% input$sample_to_add) input$end_time else NA_character_,

          # # Household sample size inputs
          pop_indicator = if ("household" %in% input$sample_to_add) input$pop_indicator else "General",
          pop_expected_prevalence = if ("household" %in% input$sample_to_add) input$pop_expected_prevalence else NA_real_,
          pop_precision = if ("household" %in% input$sample_to_add) input$pop_precision else NA_real_,
          pop_nonresponse = if ("household" %in% input$sample_to_add) input$pop_nonresponse else NA_real_,
          pop_design_effect = if ("household" %in% input$sample_to_add) input$pop_design_effect else NA_real_,
          pop_fpc = if ("household" %in% input$sample_to_add) input$pop_fpc else FALSE,

          # Individual sample size inputs
          ind_indicator = if ("individual" %in% input$sample_to_add) input$ind_indicator else NA_character_,
          ind_expected_prevalence = if ("individual" %in% input$sample_to_add) input$ind_expected_prevalence else NA_real_,
          ind_precision = if ("individual" %in% input$sample_to_add) input$ind_precision else NA_real_,
          ind_nonresponse = if ("individual" %in% input$sample_to_add) input$ind_nonresponse else NA_real_,
          ind_design_effect = if ("individual" %in% input$sample_to_add) input$ind_design_effect else NA_real_,
          ind_avg_hh_size = if ("individual" %in% input$sample_to_add) input$ind_avg_hh_size else NA_real_,
          ind_subpop_prop = if ("individual" %in% input$sample_to_add) input$ind_subpop_prop else NA_real_,
          ind_fpc = if ("individual" %in% input$sample_to_add) input$ind_fpc else FALSE,

          # # Rate sample size inputs
          rate_indicator = if ("rate" %in% input$sample_to_add) input$rate_indicator else NA_character_,
          rate_expected_rate = if ("rate" %in% input$sample_to_add) input$rate_expected_rate else NA_real_,
          rate_precision = if ("rate" %in% input$sample_to_add) input$rate_precision else NA_real_,
          rate_nonresponse = if ("rate" %in% input$sample_to_add) input$rate_nonresponse else NA_real_,
          rate_design_effect = if ("rate" %in% input$sample_to_add) input$rate_design_effect else NA_real_,
          rate_recall_days = if ("rate" %in% input$sample_to_add) input$rate_recall_days else NA_real_,
          rate_avg_hh_size = if ("rate" %in% input$sample_to_add) input$rate_avg_hh_size else NA_real_,
          rate_fpc = if ("rate" %in% input$sample_to_add) input$rate_fpc else FALSE
        )

        protocol_r()$access_nested(
          field = "sample_object",
          member = "calculate_sample_sizes"
        )

        phr_touch_module(module_name = "protocol", session = session)

        iphra_message("New sample added successfully.", origin = "Sample Module: Add Sample")

      }, on_error = "warn")
    })


    # ▶️ OBSERVE - UPDATE POPULATION DROPDOWN ####

    # ---- Sync remove_target dropdown with samples()
    observeEvent(samples(), {
      iphra_try({



        # 1️⃣ VALIDATION

        result <- iphra_try_step({
          if (isTRUE(updating_dropdown())) return()
        updating_dropdown(TRUE)
        on.exit(updating_dropdown(FALSE), add = TRUE)

        tbl <- samples()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        }, step = "mod_planning_sample_size_server/unknown/Validation")
        if (iphra_failed(result)) return(result)


        # 2️⃣ CORE LOGIC

        result <- iphra_try_step({
          choices <- unique(tbl$stratum_id)
        sel <- input$remove_target
        if (!sel %in% choices) sel <- tail(choices, 1)

        updateSelectInput(
          session,
          "remove_target",
          choices = choices,
          selected = sel
        )
        }, step = "mod_planning_sample_size_server/unknown/Core Logic")
        if (iphra_failed(result)) return(result)


        # 3️⃣ RESULT HANDLING

        result <- iphra_try_step({
          iphra_message(
          paste0(
            iphra_txt("Dropdown updated with"),
            " ",
            length(choices),
            " ",
            iphra_txt("available population(s).")
          ),
          origin = iphra_txt("Remove Target Dropdown Sync")
        )
        }, step = "mod_planning_sample_size_server/unknown/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Remove Target Dropdown Sync"),
      hint = iphra_txt("Check reactive samples() or input bindings if this fails.")
      )
    },
    ignoreInit = FALSE)


    # ▶️ OBSERVE - REMOVE SAMPLE BUTTON ####

    observeEvent(input$remove_sample, {
      iphra_try({

        # 1️⃣ VALIDATION ----

        result <- iphra_try_step({
          current <- samples()
        if (is.null(current) || !nrow(current)) {
          iphra_warning("No rows to remove.",
                        origin = "Sample Module: Remove Sample",
                        hint = "Add a population first.")
          return(NULL)
        }

        target <- trimws(input$remove_target %||% "")
        if (target == "") {
          iphra_warning("No population selected to remove.",
                        origin = "Sample Module: Remove Sample",
                        hint = "Choose a population name from the dropdown.")
          return(NULL)
        }
        }, step = "mod_planning_sample_size_server/observeEvent_remove_sample/Validation")
        if (iphra_failed(result)) return(result)

        # 2️⃣ CORE LOGIC####

        result <- iphra_try_step({

          protocol_r()$access_nested(
            field = "sample_object",
            member = "remove_stratum",
            strata_name = input$remove_target
          )

          phr_touch_module(module_name = "protocol", session = session)

        }, step = "mod_planning_sample_size_server/observeEvent_remove_sample/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Sample Module: Remove Sample"),
      hint   = iphra_txt("Check reactive sample frame or dropdown value if this fails.")
      )
    })


    # ▶️ OBSERVE - DESIGN EFFECT BLOCK ####

    # ---- Update Design Effect Field State based on Sampling Method
    observeEvent(input$sampling_method_site, {
      iphra_try({

        # 2️⃣ CORE LOGIC

        result <- iphra_try_step({
          if (input$sampling_method_site == "cluster") {
          shinyjs::enable(ns("pop_design_effect"))
          shinyjs::enable(ns("ind_design_effect"))
          shinyjs::enable(ns("rate_design_effect"))
          shinyjs::runjs(sprintf(
            "$('#%s, #%s, #%s').prop('readonly', false);",
            ns('pop_design_effect'),
            ns('ind_design_effect'),
            ns('rate_design_effect')
          ))

        } else if (input$sampling_method_site == "simple_random" |
                   input$sampling_method_site == "systematic" |
                   input$sampling_method_site == "proportional" |
                   input$sampling_method_site == "purposive") {
          shinyjs::disable(ns("pop_design_effect"))
          shinyjs::disable(ns("ind_design_effect"))
          shinyjs::disable(ns("rate_design_effect"))
          shinyjs::runjs(sprintf(
            "$('#%s, #%s, #%s').prop('readonly', true);",
            ns('pop_design_effect'),
            ns('ind_design_effect'),
            ns('rate_design_effect')
          ))
        }
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = iphra_txt("Sample Module: Sampling Method Change"),
      hint = iphra_txt("Verify shinyjs bindings or ns() IDs if this fails.")
      )
    })

    observeEvent(input$calculate_all, {

      protocol_r()$access_nested(
        field = "sample_object",
        member = "calculate_sample_sizes"
      )

      phr_touch_module(module_name = "protocol", session = session)

    })

    # ▶️ OBSERVE - CALCULATE HOUSEHOLD SAMPLE ####
    observeEvent(input$pop_calculate, {
      iphra_try({

        # 2️⃣ CORE LOGIC

        result <- iphra_try_step({

          if(input$sampling_method_site == "simple_random" |
             input$sampling_method_site == "systematic" |
             input$sampling_method_site == "purposive" |
             input$sampling_method_site == "proportional" ) {
            sample_design <- "simple_random"
          } else if(input$sampling_method_site == "cluster") {
            sample_design <- "cluster"
          } else {
            phrutils::phr_warning(message = "Inappropriate sampling design chosen. Stopping operation")
          }

          sample_n <- phr::calculate_sample_size_general(
            expected_proportion = input$pop_expected_prevalence,
            desired_precision = input$pop_precision,
            non_response_rate = input$pop_nonresponse,
            design = sample_design,
            design_effect = input$pop_design_effect,
            fpc = input$pop_fpc,
            total_population = input$total_population,
            number_clusters = NULL,
            confidence_level = 0.95
          )

          pop_sample_size(sample_n)

        # --- Future: Store result in session state ---
        # session$userData$project$sample$household <- sample
        }, step = "mod_planning_sample_size_server/observeEvent_pop_calculate/Core Logic")
        if (iphra_failed(result)) return(result)


        # 3️⃣ RESULT HANDLING

        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Household sample size calculation completed successfully."),
          origin = iphra_txt("Planning: Household Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_pop_calculate/Result Handling")
        if (iphra_failed(result)) return(result)

      }, on_error = "warn",
      origin = iphra_txt("Planning: Household Sample"),
      hint = iphra_txt("Check numeric inputs or missing data fields if this fails.")
      )
    })


    # ▶️ OBSERVE - CALCULATE INDIVIDUAL SAMPLE ####
    observeEvent(input$ind_calculate, {
      iphra_try({

        if(input$sampling_method_site == "simple_random" |
           input$sampling_method_site == "systematic" |
           input$sampling_method_site == "purposive" |
           input$sampling_method_site == "proportional" ) {
          sample_design <- "simple_random"
        } else if(input$sampling_method_site == "cluster") {
          sample_design <- "cluster"
        } else {
          phrutils::phr_warning(message = "Inappropriate sampling design chosen. Stopping operation")
        }

        sample_n <- phr::calculate_sample_size_individual(
          expected_proportion = input$ind_expected_prevalence,
          desired_precision = input$ind_precision,
          non_response_rate = input$ind_nonresponse,
          design = sample_design,
          design_effect = input$ind_design_effect,
          fpc = input$ind_fpc,
          average_household_size = input$ind_avg_hh_size,
          sub_population_percent = input$ind_subpop_prop,
          total_population = input$total_population,
          num_clusters = NULL,
          confidence_level = 0.95
        )

        ind_sample_size(sample_n)

      }, on_error = "warn",
      origin = iphra_txt("Planning: Individual Sample"),
      hint = iphra_txt("Check numeric inputs or missing data fields if this fails.")
      )
    })


    # ▶️ OBSERVE - CALCULATE RATE SAMPLE ####
    observeEvent(input$rate_calculate, {
      iphra_try({

        if(input$sampling_method_site == "simple_random" |
           input$sampling_method_site == "systematic" |
           input$sampling_method_site == "purposive" |
           input$sampling_method_site == "proportional" ) {
          sample_design <- "simple_random"
        } else if(input$sampling_method_site == "cluster") {
          sample_design <- "cluster"
        } else {
          phrutils::phr_warning(message = "Inappropriate sampling design chosen. Stopping operation")
        }

        sample_n <- phr::calculate_sample_size_rate(
          expected_rate = input$rate_expected_rate,
          desired_precision = input$rate_precision,
          non_response_rate = input$rate_nonresponse,
          design = sample_design,
          design_effect = input$rate_design_effect,
          number_clusters = NULL,
          recall_days = input$rate_recall_days,
          average_household_size = input$rate_avg_hh_size,
          fpc = input$rate_fpc,
          total_population = input$total_population,
          confidence_level = 0.95,
          multiplier = 10000
        )

        rate_sample_size(sample_n)

}, on_error = "warn",
      origin = iphra_txt("Planning: Mortality Sample"),
      hint = iphra_txt("Check numeric inputs or missing data fields if this fails.")
      )
    })


    # ▶️ OBSERVE - CALCULATE PLAN ####
    observeEvent(input$calc_plan, {
      iphra_try({

        if(input$sampling_method_site == "simple_random" |
           input$sampling_method_site == "systematic" |
           input$sampling_method_site == "purposive" |
           input$sampling_method_site == "proportional" ) {
          sample_design <- "simple_random"
        } else if(input$sampling_method_site == "cluster") {
          sample_design <- "cluster"
        } else {
          phrutils::phr_warning(message = "Inappropriate sampling design chosen. Stopping operation")
        }

        plan_result <- phr::estimate_field_plan(
          sample_design = sample_design,
          number_of_teams = input$n_teams,
          enumerators_per_team = input$n_enum,
          number_of_psu_per_team_per_day = input$clusters_per_day,
          start_time = input$start_time,
          end_time = input$end_time,
          average_interview_time = input$interview_time,
          average_travel_time = input$travel_time,
          average_rest_time = input$rest_time,
          total_sample_size = NULL
        )

        num_interview_per_enumday(plan_result[["num_interview_per_enum_per_day"]])
        num_days_dc(plan_result[["num_days"]])

        if(sample_design == "cluster") {
          num_psu_needed(plan_result[["num_psu_needed"]])
          psu_size(plan_result[["psu_size"]])
        }

}, on_error = "warn",
      origin = iphra_txt("Planning: Survey Days"),
      hint = iphra_txt("Check time fields or numeric entries if this fails.")
      )
    })


    # ---- Update Selected Population
    observeEvent(input$update_population, {
      iphra_try({


      }, on_error = "warn",
      origin = iphra_txt("Planning: Update Population"),
      hint = iphra_txt("Check if population table or selection binding failed.")
      )
    })


    # ▶️ OBSERVE - IMPORT SAMPLING FRAME ####

    observe({
      print("drawn sample updated")
      print(protocol_r()$sampling_frame$drawn_sample_full)
    })

    observeEvent(input$import_frame, {

      iphra_try({

        req(input$import_frame)

        file_path <- input$import_frame$datapath
        file_ext  <- tolower(tools::file_ext(input$import_frame$name))

        imported_df <- switch(
          file_ext,
          csv = read.csv(
            file_path,
            stringsAsFactors = FALSE
          ),
          xlsx = as.data.frame(
            readxl::read_excel(file_path)
          ),
          xls = as.data.frame(
            readxl::read_excel(file_path)
          ),
          stop("Unsupported file type.")
        ) |>
          dplyr::mutate(
            inclusion = ifelse(inclusion == "False", FALSE, TRUE))

        # Check required columns
        required_cols <- c(
          "stratum",
          "psu",
          "population_size",
          "inclusion"
        )

        missing_cols <- setdiff(required_cols, names(imported_df))

        if (length(missing_cols) > 0) {
          phrutils::phr_warning(
            sprintf(
              "Missing required columns: %s",
              paste(missing_cols, collapse = ", ")
            )
          )
          return()
        }

        # Optional columns must be NA_real_ if present
        for (col in c("sampled_psu", "allocated_sample")) {
          if (col %in% names(imported_df)) {
            imported_df[[col]] <- NA_real_
          }
        }

        # ---- Coerce required column types

        imported_df$stratum <- as.character(imported_df$stratum)
        imported_df$psu <- as.character(imported_df$psu)

        imported_df$population_size <- suppressWarnings(
          as.numeric(imported_df$population_size)
        )

        imported_df$inclusion <- dplyr::case_when(
          imported_df$inclusion %in% c(TRUE, "TRUE", "true", 1, "1") ~ TRUE,
          imported_df$inclusion %in% c(FALSE, "FALSE", "false", 0, "0") ~ FALSE,
          TRUE ~ NA
        )

        # if (any(is.na(imported_df$inclusion))) {
        #   phrutils::phr_warning(
        #     "Some values in 'inclusion' could not be converted to logical (TRUE/FALSE)."
        #   )
        #   return()
        # }

        # ---- Validate coercion results

        bad_population_size <- !is.na(imported_df$population_size) &
          !is.finite(imported_df$population_size)

        if (any(bad_population_size)) {
          phrutils::phr_warning(
            "Invalid values detected in 'population_size'. Values could not be converted to numeric."
          )
          return()
        }

        bad_inclusion <- !is.na(imported_df$inclusion) &
          !imported_df$inclusion %in% c(TRUE, FALSE)

        if (any(bad_inclusion)) {
          phrutils::phr_warning(
            "Invalid values detected in 'inclusion'. Values must be coercible to TRUE/FALSE."
          )
          return()
        }

        protocol_r()$sampling_frame$set(
          field = "log_df",
          value = imported_df
        )

        phr_touch_module(
          module_name = "protocol",
          session = session
        )

      },
      on_error = "warn",
      origin = "Sample Module: Import Sampling Frame",
      hint = "Verify file format and sampling frame structure."
      )

    })



    # ▶️ OBSERVE - DRAW SAMPLE ####
    observeEvent(input$draw_sample, {

      cat("BEFORE draw_sample\n")

      result <- tryCatch({

        protocol_r()$access_nested(
          field = "sampling_frame",
          member = "draw_sample",
          strata_table = protocol_r()$get_sample_table(),
          seed = 987
        )

        "SUCCESS"

      }, error = function(e) {

        cat("ERROR INSIDE DRAW SAMPLE:\n")
        print(e)

        e

      })

      cat("AFTER draw_sample\n")
      print(result)

    })

    observeEvent(input$draw_sample, {

      cat("\n====================\n")
      cat("DRAW SAMPLE CLICKED\n")
      cat("====================\n")
      cat("Frame rows: ",
          nrow(protocol_r()$sampling_frame$get("log_df")),
          "\n")
      cat("Sample table rows: ",
          nrow(protocol_r()$get_sample_table()),
          "\n")

      # iphra_try({

        cat("Calling draw_sample()...\n")

        protocol_r()$sampling_frame$draw_sample(
          strata_table = protocol_r()$get_sample_table(),
          seed = 987
        )

        cat("Returned from draw_sample()\n")

        result <- protocol_r()$sampling_frame$drawn_sample_full

        cat("drawn_sample_full class:\n")
        print(class(result))

        cat("drawn_sample_full dimensions:\n")
        print(dim(result))

        cat("drawn_sample_full preview:\n")
        print(utils::head(result))

        phr_touch_module(module_name = "protocol", session = session)

      # },
      # on_error = "warn",
      # origin = iphra_txt("Sample Module: Draw Sample"),
      # hint   = iphra_txt("Verify sampling frame availability and randomization logic if this fails.")
      # )
    })

    # ▶️ OBSERVE - SAMPLING FRAME EDIT DETECTION ####

    observeEvent(input$sampling_frame, {
      iphra_try({

        updated_frame <- rhandsontable::hot_to_r(input$sampling_frame)

        protocol_r()$sampling_frame$set(field = "log_df", value = updated_frame)

        phr_touch_module(
          module_name = "protocol",
          session = session
          )
        },
      on_error = "warn",
      origin = iphra_txt("Sample Module: Sampling Frame Update"),
      hint   = iphra_txt("Ensure rhandsontable input is correctly bound or re-rendered if updates fail.")
      )
    })

  })
}

## To be copied in the UI
# mod_planning_sample_size_ui("planning_sample_size_1")

## To be copied in the server
# mod_planning_sample_size_server("planning_sample_size_1")
