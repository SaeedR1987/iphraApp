#' Workflow status: header project/save indicator, next-step guidance and
#' recent-activity log. Read-only with respect to existing app state.
#' @noRd
iphra_workflow_steps <- function() {
  s <- function(title, hint, icon, placeholder = TRUE) {
    list(title = title, hint = hint, icon = icon, placeholder = placeholder, nxt = NULL)
  }
  steps <- list(
    goals = s("Goals and Objectives",
              "Define the assessment goals and objectives, then choose the tools to use.",
              "glyphicon-flag", FALSE),
    tools = s("Tool Design",
              "Add the data collection tools you need, then plan the sample.",
              "glyphicon-wrench", FALSE),
    planning = s("Planning and Sampling",
                 "Plan the sample size and field days, then save from the File menu.",
                 "glyphicon-calendar", FALSE),
    import = s("Data Import", "Import the collected data for this assessment.",
               "glyphicon-import", FALSE),
    standardization = s("Data Standardization",
                        "Standardize the imported data to the expected structure and variables.",
                        "glyphicon-sort-by-attributes", FALSE),
    quality = s("Quality Reports", "Review quality reports on the imported data.",
                "glyphicon-check"),
    cleaning = s("Data Cleaning", "Clean the data and resolve quality issues.",
                 "glyphicon-filter"),
    analysis_quant = s("Quantitative Data Analysis", "Analyse the quantitative data.",
                       "glyphicon-stats"),
    analysis_qual = s("Qualitative Data Analysis", "Analyse the qualitative data.",
                      "glyphicon-comment"),
    analysis_integrated = s("Integrated Analysis",
                            "Combine quantitative and qualitative findings.",
                            "glyphicon-th-large"),
    reporting = s("Reporting", "Prepare and export assessment reports.",
                  "glyphicon-file")
  )
  nm <- names(steps)
  for (i in seq_len(length(nm) - 1L)) steps[[i]]$nxt <- nm[i + 1L]
  steps
}

#' Placeholder tab for a step whose module is not built yet.
#' @noRd
iphra_placeholder_tab <- function(v) {
  shiny::tabPanel(
    shiny::textOutput(paste0("tab_title_", v), inline = TRUE), value = v,
    shiny::uiOutput(paste0("phr_ph_", v))
  )
}

#' Sidebar structure: top-level items, with optional children.
#' @noRd
iphra_sidebar_items <- function() {
  list(
    list(value = "goals"), list(value = "tools"), list(value = "planning"),
    list(value = "import"), list(value = "standardization"), list(value = "quality"), list(value = "cleaning"),
    list(value = "analysis", title = "Data Analysis", icon = "glyphicon-signal",
         children = c("analysis_quant", "analysis_qual", "analysis_integrated")),
    list(value = "reporting")
  )
}

#' @noRd
iphra_workflow_status_server <- function(input, output, session, iphra_session, lang_r) {
  tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))
  steps <- iphra_workflow_steps()
  project <- session$userData$project

  output$tab_title_analysis <- shiny::renderText(tx("Data Analysis"))
  for (key in names(steps)[vapply(steps, function(x) isTRUE(x$placeholder), logical(1))]) {
    local({
      k <- key
      output[[paste0("tab_title_", k)]] <- shiny::renderText(tx(steps[[k]]$title))
      output[[paste0("phr_ph_", k)]] <- shiny::renderUI(
        phr_info_panel(tx("This step is not available yet."), title = tx(steps[[k]]$title))
      )
    })
  }

  dirty <- shiny::reactiveVal(FALSE)
  activity <- shiny::reactiveVal(
    data.frame(time = format(Sys.time(), "%H:%M:%S"), event = "Session started",
               arg = "", stringsAsFactors = FALSE)
  )
  log_event <- function(event, arg = "") {
    cur <- shiny::isolate(activity())
    activity(utils::head(rbind(
      data.frame(time = format(Sys.time(), "%H:%M:%S"), event = event, arg = arg,
                 stringsAsFactors = FALSE),
      cur
    ), 50L))
  }

  shiny::observeEvent(iphra_session$version_signal()(), {
    dirty(TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(project$last_saved, {
    dirty(FALSE)
    log_event("Project saved")
  }, ignoreInit = TRUE)

  shiny::observeEvent(project$path, {
    dirty(FALSE)
    log_event("Project saved or loaded")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$workflow_nav, {
    s <- steps[[input$workflow_nav]]
    if (!is.null(s)) log_event("Opened:", s$title)
  }, ignoreInit = TRUE)

  output$phr_project_status <- shiny::renderUI({
    nm <- tryCatch(iphra_session$get_project_name(), error = function(e) "")
    path <- project$path
    never_saved <- is.null(project$last_saved) && is.null(path)
    shiny::div(
      class = "phr-header__status",
      shiny::span(class = "phr-header__project", title = if (!is.null(path)) path,
                  tx("Project:"), " ", shiny::strong(nm)),
      if (never_saved) phr_badge(tx("Not saved yet"), "neutral")
      else if (isTRUE(dirty())) phr_badge(tx("Unsaved changes"), "warning")
      else phr_badge(tx("Saved"), "success")
    )
  })

  output$phr_guidance <- shiny::renderUI({
    key <- input$workflow_nav %||% "goals"
    s <- steps[[key]]
    if (is.null(s)) return(NULL)
    idx <- match(key, names(steps))
    shiny::div(
      class = "phr-guidance",
      shiny::div(
        class = "phr-guidance__text",
        shiny::div(class = "phr-guidance__title",
                   sprintf("%s %d/%d: %s", tx("Step"), idx, length(steps), tx(s$title))),
        shiny::div(class = "phr-guidance__hint", tx(s$hint))
      ),
      phr_progress(round(100 * idx / length(steps)), class = "phr-guidance__progress"),
      if (!is.null(s$nxt)) {
        shiny::actionButton(
          "phr_next_step",
          paste(tx("Next:"), tx(steps[[s$nxt]]$title)),
          class = "btn-primary btn-sm"
        )
      }
    )
  })

  shiny::observeEvent(input$phr_next_step, {
    nxt <- steps[[input$workflow_nav %||% "goals"]]$nxt
    if (!is.null(nxt)) shiny::updateNavbarPage(session, "workflow_nav", selected = nxt)
  })

  shiny::observeEvent(input$phr_activity_btn, {
    a <- activity()
    shiny::showModal(shiny::modalDialog(
      title = tx("Recent activity"), easyClose = TRUE, footer = shiny::modalButton(tx("Close")),
      shiny::tags$ul(
        class = "phr-activity",
        lapply(seq_len(nrow(a)), function(i) {
          shiny::tags$li(shiny::span(class = "phr-activity__time", a$time[i]), " ", tx(a$event[i]),
                         if (nzchar(a$arg[i])) paste0(" ", tx(a$arg[i])))
        })
      )
    ))
  })
}
