# -----------------------------------------------------------------------------
# Module: Tools Preview (read-only preview of a selected tool)
# -----------------------------------------------------------------------------
#
# Right-hand pane of the Tool Design tab. A select input picks one of the
# tools currently in the protocol; read-only tables show its revised survey,
# choices and settings, a summary of the selected indicators, and the issues
# stored in the tool's private `..issues` field (populated by `validate()`).

#' tools_preview UI Function
#'
#' @param id Module id.
#' @noRd
mod_tools_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    title = "Tool Preview",
    width = 12,
    shiny::selectInput(ns("tool"), "Tool to display", choices = character(0), width = "100%"),
    shiny::tabsetPanel(
      id = ns("preview_tabs"),
      shiny::tabPanel("Survey", DT::DTOutput(ns("survey_tbl"))),
      shiny::tabPanel("Choices", DT::DTOutput(ns("choices_tbl"))),
      shiny::tabPanel("Settings", DT::DTOutput(ns("settings_tbl"))),
      shiny::tabPanel("Summary", shiny::tableOutput(ns("summary_tbl"))),
      shiny::tabPanel("Issues", DT::DTOutput(ns("issues_tbl")))
    )
  )
}

#' tools_preview Server Functions
#'
#' @param id Module id.
#' @param active_tab Reactive returning the active tool tab (role).
#' @noRd
mod_tools_preview_server <- function(id, active_tab = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    protocol_r <- phr_get_module_reactive("protocol", session)

    defs <- iphra_tool_definitions()
    all_roles <- vapply(defs, function(t) t$name, character(1))
    labels <- vapply(defs, function(t) t$label, character(1))

    current_roles <- shiny::reactive({
      r <- tryCatch(as.character(protocol_r()$get_tool_names()), error = function(e) character(0))
      intersect(all_roles, r)
    })

    shiny::observe({
      roles <- current_roles()
      choices <- stats::setNames(roles, labels[match(roles, all_roles)])
      keep <- shiny::isolate(input$tool)
      sel <- if (!is.null(keep) && keep %in% roles) keep else if (length(roles)) roles[1] else NULL
      shiny::updateSelectInput(session, "tool", choices = choices, selected = sel)
    })

    # Follow the active tool tab.
    shiny::observeEvent(active_tab(), {
      at <- active_tab()
      if (!is.null(at) && at %in% shiny::isolate(current_roles())) {
        shiny::updateSelectInput(session, "tool", selected = at)
      }
    }, ignoreNULL = TRUE)

    tool_r <- shiny::reactive({
      protocol_r()
      role <- input$tool
      shiny::req(role, role %in% current_roles())
      p <- protocol_r()
      tool <- tryCatch(p$get(field = "tools", role = role), error = function(e) NULL)
      if (is.null(tool)) tool <- tryCatch(p$tools[[role]], error = function(e) NULL)
      tool
    })

    sheet <- function(field) {
      tool <- tool_r()
      df <- tryCatch(tool$get(field = field), error = function(e) NULL)
      if (!is.data.frame(df)) df <- data.frame()
      df
    }

    render_tbl <- function(df, empty_msg) {
      if (nrow(df) == 0 && ncol(df) == 0) {
        df <- data.frame(Message = empty_msg)
      }
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        editable = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip")
      )
    }

    output$survey_tbl <- DT::renderDT(render_tbl(sheet("revised_survey"), "No survey available."))
    output$choices_tbl <- DT::renderDT(render_tbl(sheet("revised_choices"), "No choices available."))
    output$settings_tbl <- DT::renderDT(render_tbl(sheet("revised_settings"), "No settings available."))

    output$summary_tbl <- shiny::renderTable({
      empty <- data.frame(Indicator = character(0), Minutes = numeric(0), stringsAsFactors = FALSE)
      tool <- tool_r()
      codes <- as.character(tryCatch(tool$get(field = "selected_indicator_codes"), error = function(e) NULL))
      survey <- sheet("revised_survey")
      if (!length(codes) || !all(c("indicator_code", "time_seconds") %in% names(survey))) {
        return(empty)
      }

      bank <- tryCatch(
        protocol_r()$get(field = "framework", member = "modified_indicator_bank"),
        error = function(e) NULL
      )
      name_of <- function(code) {
        if (is.data.frame(bank) && all(c("indicator_code", "indicator_name") %in% names(bank))) {
          nm <- bank$indicator_name[match(code, bank$indicator_code)]
          ifelse(is.na(nm), code, nm)
        } else {
          code
        }
      }

      secs <- tapply(as.numeric(survey$time_seconds), survey$indicator_code, sum, na.rm = TRUE)
      mins <- as.numeric(secs[codes]) / 60
      mins[is.na(mins)] <- 0
      out <- data.frame(Indicator = name_of(codes), Minutes = mins, stringsAsFactors = FALSE)
      rbind(out, data.frame(Indicator = "Total", Minutes = sum(mins), stringsAsFactors = FALSE))
    })

    output$issues_tbl <- DT::renderDT({
      tool <- tool_r()
      issues <- tryCatch(tool$get(field = "..issues"), error = function(e) NULL)
      if (!is.data.frame(issues) || nrow(issues) == 0) {
        issues <- data.frame(
          check = character(0), sheet = character(0), row = integer(0),
          column = character(0), message = character(0), stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        issues,
        rownames = FALSE,
        selection = "none",
        editable = FALSE,
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "frtip",
          language = list(emptyTable = "No issues recorded. Run Validate Tool to check.")
        )
      )
    })
  })
}
