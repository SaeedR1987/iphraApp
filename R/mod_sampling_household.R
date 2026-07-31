#' sampling_household UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sampling_household_ui <- function(id) {
  ns <- NS(id)
  tagList(

    br(),br(),br(),

    # ---- First Row: Controls ----
    fluidRow(
      column(
        4,
        actionButton(ns("import_frame"), "Import Sampling Frame", class = "btn-primary")
      ),
      column(
        4,
        radioButtons(
          ns("sampling_method"),
          "Sampling Method:",
          choices = c(
            "Simple Random Sampling (SRS)" = "srs",
            "Proportional" = "proportional",
            "Probability Proportional to Size (PPS)" = "pps",
            "Random Location Clusters (RLC)" = "rlc",
            "Systematic" = "systematic"
          ),
          inline = FALSE
        )
      ),
      column(
        4,
        wellPanel(
          h5("Reserve Clusters"),
          checkboxInput(ns("include_reserves"), "Include reserve clusters", value = FALSE),
          numericInput(ns("n_reserves"), "Number per strata", value = 0, min = 0)
        )
      )
    ),

    br(),

    # ---- Second Row: Tables ----
    fluidRow(
      # Left column (4 wide) - row info + summary table
      column(
        4,
        fluidRow(
          column(
            12,
            h5("Selected Row Info"),
            verbatimTextOutput(ns("selected_row_info"))
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            h5("Summary Table"),
            DT::DTOutput(ns("summary_table"))
          )
        )
      ),

      # Middle column (4 wide) - imported frame
      column(
        4,
        h5("Imported Sampling Frame"),
        div(
          style = "max-height:400px; overflow-y:auto; overflow-x:auto;",
          DT::DTOutput(ns("sampling_frame"))
        )
      ),

      # Right column (4 wide) - results + draw button
      column(
        4,
        actionButton(ns("draw_sample"), "Draw Sample", class = "btn-success"),
        br(), br(),
        h5("Sample Results"),
        DT::DTOutput(ns("sample_results"))
      )
    )

  )
}

#' sampling_household Server Functions
#'
#' @noRd
mod_sampling_household_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ---- Reactive data with dummy values ----
    summary_data <- reactiveVal(
      data.frame(
        Stratum = c("Urban", "Rural"),
        Clusters = c(10, 15),
        Households = c(200, 300),
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

    # ---- Outputs ----
    output$summary_table <- DT::renderDT({
      DT::datatable(
        summary_data(),
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE,
                       scrollY = "400x",
                       paging = FALSE,
                       dom = 't')
      )
    })

    output$sampling_frame <- DT::renderDT({
      DT::datatable(
        sampling_frame_data(),
        selection = "none",
        rownames = FALSE,
        options = list(scrollX = TRUE,
                       scrollY = "400x",
                       paging = FALSE,
                       dom = 't')
      )
    })

    output$sample_results <- DT::renderDT({
      DT::datatable(
        sample_results_data(),
        selection = "none",
        rownames = FALSE,
        options = list(scrollX = TRUE,
                       scrollY = "400x",
                       paging = FALSE,
                       dom = 't')
      )
    })

    # ---- Selected row info ----
    output$selected_row_info <- renderPrint({
      s <- input$summary_table_rows_selected
      if (length(s)) {
        row <- summary_data()[s, , drop = FALSE]
        return(row)
      } else {
        return("No row selected.")
      }
    })

    # =====================================================
    # --- Observer: Import Sampling Frame ------------------
    # =====================================================
    observeEvent(input$import_frame, {
      iphra_try({

        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # Validation placeholder
        }, step = "mod_sampling_household_server/observeEvent_import_frame/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          showNotification("Import Sampling Frame clicked", type = "message")
        }, step = "mod_sampling_household_server/observeEvent_import_frame/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = "mod_sampling_household_server/observeEvent_import_frame",
      hint = "Check import parameters."
      )
    })

    # =====================================================
    # --- Observer: Draw Sample ----------------------------
    # =====================================================
    observeEvent(input$draw_sample, {
      iphra_try({

        # ────────────────────────────────────────────────
        # 1️⃣ VALIDATION & PRECONDITIONS
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          # Validation placeholder
        }, step = "mod_sampling_household_server/observeEvent_draw_sample/Validation")
        if (iphra_failed(result)) return(result)

        # ────────────────────────────────────────────────
        # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
        # ────────────────────────────────────────────────
        result <- iphra_try_step({
          showNotification("Draw Sample clicked", type = "message")
        }, step = "mod_sampling_household_server/observeEvent_draw_sample/Core Logic")
        if (iphra_failed(result)) return(result)

      },
      on_error = "warn",
      origin = "mod_sampling_household_server/observeEvent_draw_sample",
      hint = "Check sampling parameters."
      )
    })

  })
}

## To be copied in the UI
# mod_sampling_household_ui("sampling_household_1")

## To be copied in the server
# mod_sampling_household_server("sampling_household_1")
