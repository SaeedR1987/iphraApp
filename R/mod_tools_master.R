# ────────────────────────────────────────────────────────────────────────────────
# Module: Tools Master Status
# ────────────────────────────────────────────────────────────────────────────────
#
# This module displays a summary status table of tools/DAP steps.
# It reads from the checkbox_status in the session state to show
# checkmarks or X marks for each tools step.
#
# ────────────────────────────────────────────────────────────────────────────────

#' tools_master UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tools_master_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        12,
        actionButton(ns("export_tools"), "Export All Tools", class = "btn-warning"),
        br(), br(),
        # --- Status table with collapse toggle ---
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          h4("Tools Status"),
          actionButton(ns("toggle_status"), "", icon = icon("chevron-down"), 
                      class = "btn-sm btn-default",
                      style = "margin-bottom: 10px;")
        ),
        div(
          id = ns("status_table_container"),
          shiny::uiOutput(ns("status_table"))
        ),
        br(), br(), br()
      )
    )

  )
}

#' tools_master Server Functions
#'
#' @noRd
mod_tools_master_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive expression to get current status
    tools_status <- shiny::reactive({
      # Access checkbox status to create reactive dependency
      checkbox_status <- iphra_get_checkbox_status(session)
      tools <- checkbox_status$tools

      # Create status list from reactive values
      list(
        household_tools_complete = isTRUE(tools$household_tools_complete),
        community_tools_complete = isTRUE(tools$community_tools_complete),
        fsl_tools_complete = isTRUE(tools$fsl_tools_complete),
        health_tools_complete = isTRUE(tools$health_tools_complete),
        wash_tools_complete = isTRUE(tools$wash_tools_complete)
      )
    })

    # Render the status table
    output$status_table <- shiny::renderUI({
      status_list <- tools_status()
      labels <- tools_status_labels()
      
      # Create matrix format: columns are tools, single row for "Tools Complete"
      steps <- c(iphra_txt("Tools Complete"))
      status_matrix <- build_status_matrix(status_list, labels, steps, step_index = 1)
      
      render_matrix_master_status_table(status_matrix, title = iphra_txt("Tools Checklist"))
    })

    # ---- Toggle status table visibility ----
    status_table_visible <- reactiveVal(TRUE)
    
    observeEvent(input$toggle_status, {
      current_state <- status_table_visible()
      status_table_visible(!current_state)
      
      if (!current_state) {
        shinyjs::show("status_table_container")
        shinyjs::html("toggle_status", html = '<i class="fa fa-chevron-down"></i>')
      } else {
        shinyjs::hide("status_table_container")
        shinyjs::html("toggle_status", html = '<i class="fa fa-chevron-right"></i>')
      }
    })

  })
}

## To be copied in the UI
# mod_tools_master_ui("tools_master_1")

## To be copied in the server
# mod_tools_master_server("tools_master_1")
