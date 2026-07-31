#' planning_days_teams UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_planning_days_teams_ui <- function(id) {
  ns <- NS(id)
  tagList(

    br(),br(),br(),

    # ---- Part A: Summary Outputs ----
    fluidRow(
      column(
        2,
        h5("Estimated Number of Days: "),
        verbatimTextOutput(ns("days_result"))
      ),
      column(
        2,
        h5("Recommended Cluster Size: "),
        verbatimTextOutput(ns("cluster_size_result"))
      ),
      column(
        2,
        h5("Number of Recommended Clusters: "),
        verbatimTextOutput(ns("n_clusters_result"))
      ),
      column(
        2,
        # New button to add selected row
        actionButton(ns("add_sample"), "Add Selected Row", class = "btn-primary")
      )
    ),

    br(),br(),br(),  # spacing between summary and table/params rows

    # ---- Part B: Sample Table + Data Collection Parameters side-by-side ----
    fluidRow(
      # Left: Sample table
      column(
        6,
        style="min-width: 300px;",
        h4("Sample Table", style="font-size:14px;"),
        actionButton(ns("remove_sample"), "Remove Selected Row(s)", class = "btn-danger"),
        br(), br(),
        div(
          style = "max-height:300px; overflow-y: auto;",
          DT::DTOutput(ns("samples_table"))
        )
      ),

      # Right: Data collection planning
      column(
        6,
        wellPanel(
          h4("Data Collection Planning"),
          fluidRow(
            column(
              4,
              numericInput(ns("n_teams"), "Number of Teams", value = 2, min = 1),
              numericInput(ns("n_enum"), "Enumerators per Team", value = 3, min = 1),
              textInput(ns("start_time"), "Start Time (HH:MM)", value = "08:00"),
              textInput(ns("end_time"), "End Time (HH:MM)", value = "17:00")
            ),
            column(
              4,
              numericInput(ns("interview_time"), "Avg Interview Time (minutes)", value = 30, min = 1),
              numericInput(ns("rest_time"), "Avg Daily Rest Time (minutes)", value = 60, min = 0),
              numericInput(ns("travel_time"), "Avg Travel Time (minutes)", value = 30, min = 0)
            ),
            column(
              4,
              numericInput(ns("clusters_per_day"), "Clusters per Day (per team)", value = 2, min = 1)
            )
          ),
          hr(),
          actionButton(ns("calc_days"), "Calculate Plan", class = "btn-success")
        )
      )
    ),

    br() # extra spacing at bottom

    )

}

#' planning_days_teams Server Functions
#'
#' @noRd
mod_planning_days_teams_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_planning_days_teams_ui("planning_days_teams_1")

## To be copied in the server
# mod_planning_days_teams_server("planning_days_teams_1")
