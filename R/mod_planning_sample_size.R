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

    # ────────────────────────────────────────────────
    # MAIN SPLIT LAYOUT
    # ────────────────────────────────────────────────
    fluidRow(

      # ───────────────────────────────
      # LEFT SIDE (3/4 WIDTH): Tabset
      # ───────────────────────────────
      column(width = 9,

             # ───────────────────────────────
             # COMPLETION CHECKBOXES (placed above tabsetPanel)
             # ───────────────────────────────
             fluidRow(
               style = "margin-bottom:10px;",
               column(
                 width = 12,
                 div(
                   style = "display:flex; gap:30px; align-items:center;",
                   checkboxInput(ns("sample_size_complete"), "Sample Size Complete", value = FALSE, width = "auto"),
                   checkboxInput(ns("survey_teams_complete"), "Survey Teams Complete", value = FALSE, width = "auto"),
                   checkboxInput(ns("sampling_complete"), "Sampling Complete", value = FALSE, width = "auto")
                 )
               )
             ),

             tabsetPanel(
               id = ns("sample_tabs"),
               type = "tabs",

               # ───────────────────────────────
               # TAB 1: SAMPLE SIZE
               # ───────────────────────────────
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
                                       value = 1000, min = 1, width = "100%")
                   ),
                   column(2,
                          numericInput(ns("total_population"), "Total Population:",
                                       value = 5000, min = 1, width = "100%")
                   ),
                   column(2,
                          radioButtons(ns("sampling_method"), "Sampling Method:",
                                       choices = c("Simple Random" = "srs",
                                                   "Cluster Sampling" = "cluster"),
                                       inline = FALSE)
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
                               "Mortality Rate" = "rate"
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
                         ),
                         column(
                           6,
                           actionButton(
                             ns("remove_sample"),
                             "Remove Row",
                             class = "btn-danger btn-sm",
                             width = "100%"
                           )
                         )
                       )
                     )
                   )
                 ),

                 hr(),

                 # Three sample calculators
                 fluidRow(

                   # Household-level
                   column(width = 4, style = "min-width: 300px;",
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
                            numericInput(ns("pop_result_dummy"), "Sample Size Result (Dummy):",
                                         value = 120, min = 1),
                            actionButton(ns("pop_calculate"), "Calculate")
                          )
                   ),

                   # Individual-level
                   column(width = 4, style = "min-width: 300px;",
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
                            numericInput(ns("ind_result_dummy"), "Sample Size Result (Dummy):",
                                         value = 220, min = 1),
                            actionButton(ns("ind_calculate"), "Calculate")
                          )
                   ),

                   # Mortality-level
                   column(width = 4, style = "min-width: 300px;",
                          wellPanel(
                            style = "padding: 10px; font-size: 12px;",
                            h4("Mortality Rate Sample Size", style = "font-size:14px;"),
                            selectInput(ns("mort_indicator"), "Select Indicator:",
                                        choices = c("General", "Crude Death Rate")),
                            numericInput(ns("mort_expected_death_rate"),
                                         "Expected Death Rate:", value = 0.5, min = 0, max = 100),
                            numericInput(ns("mort_precision"),
                                         "Desired Precision:", value = 0.1, min = 0.01, max = 50),
                            numericInput(ns("mort_nonresponse"),
                                         "Non-response Rate (%):", value = 10, min = 0, max = 100),
                            numericInput(ns("mort_design_effect"),
                                         "Design Effect:", value = 1, min = 1),
                            numericInput(ns("mort_recall_days"),
                                         "Number of Recall Days:", value = 30, min = 1),
                            numericInput(ns("mort_avg_hh_size"),
                                         "Average Household Size:", value = 5, min = 1),
                            checkboxInput(ns("mort_fpc"), "Apply FPC?", value = FALSE),
                            numericInput(ns("mort_result_dummy"),
                                         "Sample Size Result (Dummy):", value = 50, min = 1),
                            actionButton(ns("mort_calculate"), "Calculate")
                          )
                   )
                 )
               ),

               # ───────────────────────────────
               # TAB 2: SURVEY TEAMS
               # ───────────────────────────────
               tabPanel(
                 title = "Survey Teams",

                 br(), br(),

                 # ---- Part A: Summary Outputs ----
                 fluidRow(
                   style = "background-color:#f9f9f9; border-radius:8px; padding:15px; margin-bottom:10px;
                            box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

                   column(
                     3,
                     div(style = "text-align:center;",
                         h5("Estimated Number of Days", style = "font-weight:600; color:#333;"),
                         verbatimTextOutput(ns("days_result"), placeholder = TRUE)
                     )
                   ),
                   column(
                     3,
                     div(style = "text-align:center;",
                         h5("Recommended Cluster Size", style = "font-weight:600; color:#333;"),
                         verbatimTextOutput(ns("cluster_size_result"), placeholder = TRUE)
                     )
                   ),
                   column(
                     3,
                     div(style = "text-align:center;",
                         h5("Number of Recommended Clusters", style = "font-weight:600; color:#333;"),
                         verbatimTextOutput(ns("n_clusters_result"), placeholder = TRUE)
                     )
                   ),
                   column(
                     3,
                     div(style = "text-align:center; padding-top:12px;",
                         actionButton(
                           ns("update_population"),
                           "Update Selected Population",
                           class = "btn-primary btn-sm",
                           style = "width:100%; font-weight:500; letter-spacing:0.2px;"
                         )
                     )
                   )
                 ),

                 br(), hr(),

                 # ---- Part B: Data Collection Planning ----
                 fluidRow(
                   column(
                     12,
                     wellPanel(
                       h4("Data Collection Planning", style = "font-size:14px;"),
                       fluidRow(
                         column(
                           4,
                           numericInput(ns("n_teams"), "Number of Teams", value = 2, min = 1, width = "100%"),
                           numericInput(ns("n_enum"), "Enumerators per Team", value = 3, min = 1, width = "100%"),
                           textInput(ns("start_time"), "Start Time (HH:MM)", value = "08:00", width = "100%"),
                           textInput(ns("end_time"), "End Time (HH:MM)", value = "17:00", width = "100%")
                         ),
                         column(
                           4,
                           numericInput(ns("interview_time"), "Avg Interview Time (minutes)", value = 30, min = 1, width = "100%"),
                           numericInput(ns("rest_time"), "Avg Daily Rest Time (minutes)", value = 60, min = 0, width = "100%"),
                           numericInput(ns("travel_time"), "Avg Travel Time (minutes)", value = 30, min = 0, width = "100%")
                         ),
                         column(
                           4,
                           numericInput(ns("clusters_per_day"), "Clusters per Day (per team)", value = 2, min = 1, width = "100%"),
                           br(),
                           actionButton(ns("calc_days"), "Calculate Plan", class = "btn-success btn-sm", width = "100%")
                         )
                       )
                     )
                   )
                 ),

                 br()
               ),

               # ───────────────────────────────
               # TAB 3: SAMPLING
               # ───────────────────────────────
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
                     actionButton(ns("import_frame"), "Import Sampling Frame", class = "btn-primary", width = "100%"),
                     br(), br(),
                     radioButtons(
                       ns("sampling_method_ext"),
                       "Sampling Method:",
                       choices = c(
                         "Simple Random Sampling (SRS)" = "srs",
                         "Proportional" = "proportional",
                         "Probability Proportional to Size (PPS)" = "pps",
                         "Random Location Clusters (RLC)" = "rlc",
                         "Systematic" = "systematic"
                       ),
                       inline = FALSE
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

      # ───────────────────────────────
      # RIGHT SIDE (1/4 WIDTH): Always Visible Table
      # ───────────────────────────────
      column(width = 3, style = "min-width: 300px;",
             h4("Sample Table", style = "font-size:14px;"),

             selectInput(
               ns("remove_target"),
               "Select Population:",
               choices = NULL,
               width = "100%"
             ),

             div(style = "max-height: 800px; overflow-y: auto;",
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

    iphra_message("Initializing Sample Module server...", origin = "Sample Module")

    # ────────────────────────────────────────────────
    # INITIALIZATION
    # ────────────────────────────────────────────────
    updating_dropdown <- reactiveVal(FALSE)  # new flag to prevent re-entry loops

    # Initialize master data frame with all fields
    samples <- reactiveVal(
      data.frame(
        Population_Name = "Population A",
        Total_Households = 1000,
        Total_Population = 5000,
        Sampling_Method = "srs",
        pop_indicator = "General",
        pop_expected_prevalence = 10,
        pop_precision = 5,
        pop_nonresponse = 10,
        pop_design_effect = 1,
        pop_fpc = FALSE,
        pop_result_dummy = 120,
        ind_indicator = "General",
        ind_expected_prevalence = 10,
        ind_precision = 5,
        ind_nonresponse = 10,
        ind_design_effect = 1,
        ind_avg_hh_size = 5,
        ind_subpop_prop = 100,
        ind_fpc = FALSE,
        ind_result_dummy = 220,
        mort_indicator = "General",
        mort_expected_death_rate = 0.5,
        mort_precision = 0.1,
        mort_nonresponse = 10,
        mort_design_effect = 1,
        mort_recall_days = 30,
        mort_avg_hh_size = 5,
        mort_fpc = FALSE,
        mort_result_dummy = 50,
        teams = 5,
        avg_interview_time = 30,
        clusters_per_day = 4,
        enumerators_per_team = 3,
        avg_rest_time = 60,
        avg_travel_time = 45,
        start_time = "08:00",
        end_time = "17:00",
        stringsAsFactors = FALSE
      )
    )

    sampling_frame_data <- reactiveVal(
      data.frame(
        ClusterID = 1:10,
        Stratum = rep(c("Urban", "Rural"), each = 5),
        Households = sample(80:150, 10),
        stringsAsFactors = FALSE
      )
    )

    sample_results_data <- reactiveVal(
      data.frame(
        SampleID = 1:5,
        ClusterID = sample(1:10, 5),
        HouseholdID = sample(1001:2000, 5),
        stringsAsFactors = FALSE
      )
    )

    # ────────────────────────────────────────────────
    # RENDER filtered table for display
    # ────────────────────────────────────────────────
    output$samples_table <- rhandsontable::renderRHandsontable({
      tbl <- samples()
      display_tbl <- tbl[, c("Population_Name", "Total_Households", "Total_Population",
                             "Sampling_Method", "pop_result_dummy", "ind_result_dummy", "mort_result_dummy")]
      names(display_tbl) <- c("Population Name", "Total HH", "Total Pop", "Method",
                              "Household Sample Size", "Individual Sample Size", "Mortality Sample Size")

      rhandsontable::rhandsontable(display_tbl, readOnly = TRUE, rowHeaders = NULL)
    })

    # ---- Sampling Frame Table ----
    output$sampling_frame <- rhandsontable::renderRHandsontable({
      df <- sampling_frame_data()
      if (is.null(df)) return(NULL)

      rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 500
      ) %>%
        rhandsontable::hot_cols(
          columnSorting = TRUE,
          stretchH = "all"
        )
    })

    # ---- Sample Results Table ----
    output$sample_results <- rhandsontable::renderRHandsontable({
      df <- sample_results_data()
      if (is.null(df)) return(NULL)

      rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 500,
        readOnly = TRUE   # ← prevents direct editing by user
      ) %>%
        rhandsontable::hot_cols(
          columnSorting = TRUE,
          stretchH = "all"
        )
    })

    # ────────────────────────────────────────────────
    # ▶️ ADD SAMPLE
    # ────────────────────────────────────────────────
    observeEvent(input$add_sample, {
      iphra_try({

        # 1️⃣ VALIDATION ----
        if (is.null(input$sample_to_add) || !("household" %in% input$sample_to_add)) {
          iphra_error("Household must always be included.", origin = "Sample Module: Add Sample")
          return(NULL)
        }

        # 2️⃣ CORE LOGIC ----
        new_entry <- data.frame(
          Population_Name = input$population_name,
          Total_Households = input$total_households,
          Total_Population = input$total_population,
          Sampling_Method = input$sampling_method,
          pop_indicator = input$pop_indicator,
          pop_expected_prevalence = input$pop_expected_prevalence,
          pop_precision = input$pop_precision,
          pop_nonresponse = input$pop_nonresponse,
          pop_design_effect = input$pop_design_effect,
          pop_fpc = input$pop_fpc,
          pop_result_dummy = input$pop_result_dummy,
          ind_indicator = input$ind_indicator,
          ind_expected_prevalence = input$ind_expected_prevalence,
          ind_precision = input$ind_precision,
          ind_nonresponse = input$ind_nonresponse,
          ind_design_effect = input$ind_design_effect,
          ind_avg_hh_size = input$ind_avg_hh_size,
          ind_subpop_prop = input$ind_subpop_prop,
          ind_fpc = input$ind_fpc,
          ind_result_dummy = input$ind_result_dummy,
          mort_indicator = input$mort_indicator,
          mort_expected_death_rate = input$mort_expected_death_rate,
          mort_precision = input$mort_precision,
          mort_nonresponse = input$mort_nonresponse,
          mort_design_effect = input$mort_design_effect,
          mort_recall_days = input$mort_recall_days,
          mort_avg_hh_size = input$mort_avg_hh_size,
          mort_fpc = input$mort_fpc,
          mort_result_dummy = input$mort_result_dummy,
          teams = 5,
          avg_interview_time = 30,
          clusters_per_day = 4,
          enumerators_per_team = 3,
          avg_rest_time = 60,
          avg_travel_time = 45,
          start_time = "08:00",
          end_time = "17:00",
          stringsAsFactors = FALSE
        )

        updated <- rbind(samples(), new_entry)
        rownames(updated) <- NULL
        samples(updated)

        iphra_message("New sample added successfully.", origin = "Sample Module: Add Sample")

      }, on_error = "warn")
    })

    # ────────────────────────────────────────────────
    # ▶️ UPDATE POPULATION DROPDOWN (safe version)
    # ────────────────────────────────────────────────
    # ---- Sync remove_target dropdown with samples() ----
    observeEvent(samples(), {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (isTRUE(updating_dropdown())) return()
        updating_dropdown(TRUE)
        on.exit(updating_dropdown(FALSE), add = TRUE)

        tbl <- samples()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        }, step = "mod_planning_sample_size_server/unknown/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          choices <- unique(tbl$Population_Name)
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

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
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

    # ────────────────────────────────────────────────
    # ▶️ REMOVE SAMPLE (by dropdown exact match)
    # ────────────────────────────────────────────────
    observeEvent(input$remove_sample, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION ----
        # ────────────────────────────────────────────────
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

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC ----
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          idx <- match(target, current$Population_Name, nomatch = NA_integer_)
        if (is.na(idx)) {
          iphra_warning(
            paste0("Selected population not found: '", target, "'."),
            origin = "Sample Module: Remove Sample",
            hint = "Refresh the dropdown; the row may have already been removed."
          )
          return(NULL)
        }

        updated <- current[-idx, , drop = FALSE]
        rownames(updated) <- NULL
        samples(updated)

        # Prevent dropdown re-trigger
        updating_dropdown(TRUE)
        on.exit(updating_dropdown(FALSE), add = TRUE)

        updateSelectInput(session, "remove_target",
                          choices = unique(updated$Population_Name),
                          selected = tail(unique(updated$Population_Name), 1))
        }, step = "mod_planning_sample_size_server/observeEvent_remove_sample/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING ----
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(paste("Removed population:", target),
                      origin = "Sample Module: Remove Sample")
        }, step = "mod_planning_sample_size_server/observeEvent_remove_sample/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Remove Sample"),
      hint   = iphra_txt("Check reactive sample frame or dropdown value if this fails.")
      )
    })

    # ────────────────────────────────────────────────
    # ▶️ SAMPLING METHOD CHANGE (no change)
    # ────────────────────────────────────────────────
    # ---- Update Design Effect Field State based on Sampling Method ----
    observeEvent(input$sampling_method, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$sampling_method)) {
          iphra_warning(
            iphra_txt("Sampling method input is NULL — skipping update."),
            origin = iphra_txt("Sample Module: Sampling Method Change")
          )
          return(NULL)
        }
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (input$sampling_method == "cluster") {
          shinyjs::enable(ns("pop_design_effect"))
          shinyjs::enable(ns("ind_design_effect"))
          shinyjs::enable(ns("mort_design_effect"))
          shinyjs::runjs(sprintf(
            "$('#%s, #%s, #%s').prop('readonly', false);",
            ns('pop_design_effect'),
            ns('ind_design_effect'),
            ns('mort_design_effect')
          ))

        } else if (input$sampling_method == "srs") {
          shinyjs::disable(ns("pop_design_effect"))
          shinyjs::disable(ns("ind_design_effect"))
          shinyjs::disable(ns("mort_design_effect"))
          shinyjs::runjs(sprintf(
            "$('#%s, #%s, #%s').prop('readonly', true);",
            ns('pop_design_effect'),
            ns('ind_design_effect'),
            ns('mort_design_effect')
          ))
        }
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Design Effect fields updated successfully."),
          origin = iphra_txt("Sample Module: Sampling Method Change")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Sampling Method Change"),
      hint = iphra_txt("Verify shinyjs bindings or ns() IDs if this fails.")
      )
    })

    # ---- Household Sample Size: Calculate ----
    observeEvent(input$pop_calculate, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validating inputs for household sample calculation..."),
          origin = iphra_txt("Planning: Household Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_pop_calculate/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # --- Dummy calculation placeholder ---
        # Future: sample <- iphra_calc_household_sample(input$pop_expected_prevalence, ...)
        iphra_message(
          iphra_txt("Dummy calculation performed for household sample size."),
          origin = iphra_txt("Planning: Household Sample")
        )

        # --- Future: Store result in session state ---
        # session$userData$project$sample$household <- sample
        }, step = "mod_planning_sample_size_server/observeEvent_pop_calculate/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
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


    # ---- Individual Sample Size: Calculate ----
    observeEvent(input$ind_calculate, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validating inputs for individual sample calculation..."),
          origin = iphra_txt("Planning: Individual Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_ind_calculate/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Dummy calculation performed for individual sample size."),
          origin = iphra_txt("Planning: Individual Sample")
        )

        # --- Future: session$userData$project$sample$individual <- result ---
        }, step = "mod_planning_sample_size_server/observeEvent_ind_calculate/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Individual sample size calculation completed successfully."),
          origin = iphra_txt("Planning: Individual Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_ind_calculate/Result Handling")
        if (iphra_failed(result)) return(result)

}, on_error = "warn",
      origin = iphra_txt("Planning: Individual Sample"),
      hint = iphra_txt("Check numeric inputs or missing data fields if this fails.")
      )
    })


    # ---- Mortality Sample Size: Calculate ----
    observeEvent(input$mort_calculate, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validating inputs for mortality sample calculation..."),
          origin = iphra_txt("Planning: Mortality Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_mort_calculate/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Dummy calculation performed for mortality sample size."),
          origin = iphra_txt("Planning: Mortality Sample")
        )

        # --- Future: session$userData$project$sample$mortality <- result ---
        }, step = "mod_planning_sample_size_server/observeEvent_mort_calculate/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Mortality sample size calculation completed successfully."),
          origin = iphra_txt("Planning: Mortality Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_mort_calculate/Result Handling")
        if (iphra_failed(result)) return(result)

}, on_error = "warn",
      origin = iphra_txt("Planning: Mortality Sample"),
      hint = iphra_txt("Check numeric inputs or missing data fields if this fails.")
      )
    })


    # ---- Survey Days: Calculate Plan ----
    observeEvent(input$calc_days, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validating inputs for survey days calculation..."),
          origin = iphra_txt("Planning: Survey Days")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_calc_days/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Dummy survey plan calculation performed."),
          origin = iphra_txt("Planning: Survey Days")
        )

        # --- Future: session$userData$project$planning$survey_days <- days ---
        }, step = "mod_planning_sample_size_server/observeEvent_calc_days/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Survey plan calculation completed successfully."),
          origin = iphra_txt("Planning: Survey Days")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_calc_days/Result Handling")
        if (iphra_failed(result)) return(result)

}, on_error = "warn",
      origin = iphra_txt("Planning: Survey Days"),
      hint = iphra_txt("Check time fields or numeric entries if this fails.")
      )
    })


    # ---- Update Selected Population ----
    observeEvent(input$update_population, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Validating selected population update..."),
          origin = iphra_txt("Planning: Update Population")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_update_population/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Dummy population update performed."),
          origin = iphra_txt("Planning: Update Population")
        )

        # --- Future: session$userData$project$populations$update(selected_population) ---
        }, step = "mod_planning_sample_size_server/observeEvent_update_population/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Population update processed successfully."),
          origin = iphra_txt("Planning: Update Population")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_update_population/Result Handling")
        if (iphra_failed(result)) return(result)

}, on_error = "warn",
      origin = iphra_txt("Planning: Update Population"),
      hint = iphra_txt("Check if population table or selection binding failed.")
      )
    })

    # ────────────────────────────────────────────────
    # ▶️ IMPORT SAMPLING FRAME
    # ────────────────────────────────────────────────
    observeEvent(input$import_frame, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$import_frame)) {
          iphra_message(
            iphra_txt("Import Sampling Frame button event is NULL — skipping action."),
            origin = iphra_txt("Sample Module: Import Sampling Frame")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_import_frame/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sampling frame import initiated."),
          origin = iphra_txt("Sample Module: Import Sampling Frame")
        )

        # --- Future logic (e.g., open file dialog, read sampling frame, validate data) ---
        # session$userData$project$sampling_frame <- iphra_import_sampling_frame()

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_import_frame/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sampling frame import completed successfully."),
          origin = iphra_txt("Sample Module: Import Sampling Frame")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_import_frame/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Import Sampling Frame"),
      hint   = iphra_txt("Check file import bindings or future sampling frame logic if this fails.")
      )
    })


    # ────────────────────────────────────────────────
    # ▶️ DRAW SAMPLE
    # ────────────────────────────────────────────────
    observeEvent(input$draw_sample, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$draw_sample)) {
          iphra_message(
            iphra_txt("Draw Sample button event is NULL — skipping action."),
            origin = iphra_txt("Sample Module: Draw Sample")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_draw_sample/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sample drawing initiated."),
          origin = iphra_txt("Sample Module: Draw Sample")
        )

        # --- Future logic (e.g., stratified random sampling from frame, save drawn list) ---
        # session$userData$project$sample_store$draw_sample()
        #  sample_results_data(new_results) # make sure to update the sample_results table

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_draw_sample/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sample drawing process completed successfully."),
          origin = iphra_txt("Sample Module: Draw Sample")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_draw_sample/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Draw Sample"),
      hint   = iphra_txt("Verify sampling frame availability and randomization logic if this fails.")
      )
    })

    # ────────────────────────────────────────────────
    # ▶️ SAMPLING METHOD (EXTERNAL)
    # ────────────────────────────────────────────────
    observeEvent(input$sampling_method_ext, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$sampling_method_ext)) {
          iphra_message(
            iphra_txt("Sampling method (external) input is NULL — no action taken."),
            origin = iphra_txt("Sample Module: Sampling Method (External)")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method_ext/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt(paste("Sampling method changed to:", input$sampling_method_ext)),
          origin = iphra_txt("Sample Module: Sampling Method (External)")
        )

        # --- Future logic (e.g., adjust selection UI, update frame filtering, or enable method-specific inputs) ---
        # session$userData$project$sampling_method <- input$sampling_method_ext

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method_ext/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Sampling method update processed successfully."),
          origin = iphra_txt("Sample Module: Sampling Method (External)")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_method_ext/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Sampling Method (External)"),
      hint   = iphra_txt("Verify that sampling method reactive logic and downstream filters are properly linked.")
      )
    })


    # ────────────────────────────────────────────────
    # ▶️ INCLUDE RESERVE CLUSTERS TOGGLE
    # ────────────────────────────────────────────────
    observeEvent(input$include_reserves, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$include_reserves)) {
          iphra_message(
            iphra_txt("Include reserves checkbox event is NULL — skipping action."),
            origin = iphra_txt("Sample Module: Include Reserves")
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_include_reserves/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        status <- if (isTRUE(input$include_reserves)) "enabled" else "disabled"
        iphra_message(
          iphra_txt(paste("Reserve clusters option has been", status)),
          origin = iphra_txt("Sample Module: Include Reserves")
        )

        # --- Future logic (e.g., enable/disable numeric input or trigger recalculation) ---
        # shinyjs::toggleState(ns("n_reserves"), condition = input$include_reserves)
        # session$userData$project$reserve_clusters$active <- input$include_reserves

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_include_reserves/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Reserve clusters toggle processed successfully."),
          origin = iphra_txt("Sample Module: Include Reserves")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_include_reserves/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Include Reserves"),
      hint   = iphra_txt("Check UI toggle binding or downstream reserve cluster logic if this fails.")
      )
    })


    # ────────────────────────────────────────────────
    # ▶️ NUMBER OF RESERVE CLUSTERS
    # ────────────────────────────────────────────────
    observeEvent(input$n_reserves, {
      iphra_try({

        # ────────────────────────────────────────────────
        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        if (is.null(input$n_reserves)) {
          iphra_message(
            iphra_txt("Number of reserves input is NULL — skipping action."),
            origin = iphra_txt("Sample Module: Number of Reserves")
          )
          return(NULL)
        }

        if (!is.numeric(input$n_reserves) || input$n_reserves < 0) {
          iphra_warning(
            message = "Invalid number of reserves specified.",
            origin  = "Sample Module: Number of Reserves",
            hint    = "Ensure value is a non-negative numeric."
          )
          return(NULL)
        }

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_n_reserves/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt(paste("Number of reserve clusters set to:", input$n_reserves)),
          origin = iphra_txt("Sample Module: Number of Reserves")
        )

        # --- Future logic (e.g., update reserve cluster allocation) ---
        # session$userData$project$reserve_clusters$count <- input$n_reserves

        # ────────────────────────────────────────────────
        }, step = "mod_planning_sample_size_server/observeEvent_n_reserves/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # ────────────────────────────────────────────────
        iphra_message(
          iphra_txt("Reserve cluster number updated successfully."),
          origin = iphra_txt("Sample Module: Number of Reserves")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_n_reserves/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Number of Reserves"),
      hint   = iphra_txt("Ensure numeric input is valid and linked to sampling reserve logic.")
      )
    })

    # ────────────────────────────────────────────────
    # ▶️ SAMPLING FRAME EDIT DETECTION
    # ────────────────────────────────────────────────
    observeEvent(input$sampling_frame, {
      iphra_try({

        
        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS ----
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          if (is.null(input$sampling_frame)) return(NULL)
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_frame/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC ----
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          updated_frame <- rhandsontable::hot_to_r(input$sampling_frame)

        iphra_message(
          iphra_txt(paste("Sampling frame manually updated —", nrow(updated_frame), "rows now present.")),
          origin = iphra_txt("Sample Module: Sampling Frame Update")
        )

        # --- Future logic (e.g., store updated frame, re-run checks, re-calc sample) ---
        # session$userData$project$sampling_frame <- updated_frame
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_frame/Core Logic")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 3️⃣ RESULT HANDLING ----
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          iphra_message(
          iphra_txt("Sampling frame changes registered successfully."),
          origin = iphra_txt("Sample Module: Sampling Frame Update")
        )
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_frame/Result Handling")
        if (iphra_failed(result)) return(result)

},
      on_error = "warn",
      origin = iphra_txt("Sample Module: Sampling Frame Update"),
      hint   = iphra_txt("Ensure rhandsontable input is correctly bound or re-rendered if updates fail.")
      )
    })

    observeEvent(input$pop_fpc, {
      iphra_try({
        # ── Validation ─────────────────────────────────────
        if (is.null(input$pop_fpc)) return(NULL)

        # ── Core Logic ─────────────────────────────────────
        if (isTRUE(input$pop_fpc)) {
          iphra_message("Finite population correction (Household) enabled.", origin = "Sample Module: pop_fpc")
          # Future logic: session$userData$sample_settings$pop_fpc <- TRUE
        } else {
          iphra_message("Finite population correction (Household) disabled.", origin = "Sample Module: pop_fpc")
          # Future logic: session$userData$sample_settings$pop_fpc <- FALSE
        }

        # ── Result Handling ────────────────────────────────
        # (placeholder for any recalculation triggers)

      }, on_error = "warn")
    })

    observeEvent(input$ind_fpc, {
      iphra_try({
        # ── Validation ─────────────────────────────────────
        if (is.null(input$ind_fpc)) return(NULL)

        # ── Core Logic ─────────────────────────────────────
        if (isTRUE(input$ind_fpc)) {
          iphra_message("Finite population correction (Individual) enabled.", origin = "Sample Module: ind_fpc")
          # Future logic: session$userData$sample_settings$ind_fpc <- TRUE
        } else {
          iphra_message("Finite population correction (Individual) disabled.", origin = "Sample Module: ind_fpc")
          # Future logic: session$userData$sample_settings$ind_fpc <- FALSE
        }

        # ── Result Handling ────────────────────────────────
        # (placeholder for any recalculation triggers)

      }, on_error = "warn")
    })

    observeEvent(input$mort_fpc, {
      iphra_try({
        # ── Validation ─────────────────────────────────────
        if (is.null(input$mort_fpc)) return(NULL)

        # ── Core Logic ─────────────────────────────────────
        if (isTRUE(input$mort_fpc)) {
          iphra_message("Finite population correction (Mortality) enabled.", origin = "Sample Module: mort_fpc")
          # Future logic: session$userData$sample_settings$mort_fpc <- TRUE
        } else {
          iphra_message("Finite population correction (Mortality) disabled.", origin = "Sample Module: mort_fpc")
          # Future logic: session$userData$sample_settings$mort_fpc <- FALSE
        }

        # ── Result Handling ────────────────────────────────
        # (placeholder for any recalculation or UI update triggers)

      }, on_error = "warn")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sample_size_complete/Validation")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sample_size_complete/Core Logic")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sample_size_complete/Result Handling")
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
        }, step = "mod_planning_sample_size_server/observeEvent_survey_teams_complete/Validation")
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
        }, step = "mod_planning_sample_size_server/observeEvent_survey_teams_complete/Core Logic")
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
        }, step = "mod_planning_sample_size_server/observeEvent_survey_teams_complete/Result Handling")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_complete/Validation")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_complete/Core Logic")
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
        }, step = "mod_planning_sample_size_server/observeEvent_sampling_complete/Result Handling")
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
# mod_planning_sample_size_ui("planning_sample_size_1")

## To be copied in the server
# mod_planning_sample_size_server("planning_sample_size_1")
