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
    quality = s("Data Monitoring and Quality",
                "Monitor data collection and review data quality checks.",
                "glyphicon-check", FALSE),
    cleaning = s("Data Cleaning", "Clean the data and resolve quality issues.",
                 "glyphicon-filter", FALSE),
    analysis_quant = s("Quantitative Data Analysis", "Analyse the quantitative data.",
                       "glyphicon-stats", FALSE),
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
    list(value = "goals"),
    list(value = "tools"),
    list(value = "planning"),
    list(value = "import"), list(value = "standardization"), list(value = "quality"), list(value = "cleaning"),
    list(value = "analysis", title = "Data Analysis", icon = "glyphicon-signal",
         children = c("analysis_quant", "analysis_qual", "analysis_integrated")),
    list(value = "reporting")
  )
}

#' Read / write a Log field, trying both the plain and the `..`-prefixed name.
#' @noRd
iphra_log_get <- function(lg, field) {
  for (f in c(paste0("..", field), field)) {
    v <- tryCatch(lg$get(f), error = function(e) NULL)
    if (!is.null(v)) return(v)
  }
  NULL
}

iphra_log_set <- function(lg, field, value) {
  err <- NULL
  for (f in c(paste0("..", field), field)) {
    r <- tryCatch({ lg$set(f, value); TRUE }, error = function(e) { err <<- e; FALSE })
    if (isTRUE(r)) return(NULL)
  }
  err
}

#' Status of one log of a Data object: "red" not generated, "yellow" generated
#' but not marked complete, "green" marked complete.
#' @noRd
iphra_log_status <- function(obj, kind = c("cleaning", "deletion")) {
  kind <- match.arg(kind)
  lg <- tryCatch(obj$get(field = paste0("..", kind, "_log")), error = function(e) NULL)
  if (is.null(lg)) return("red")
  md <- iphra_log_get(lg, "metadata")
  if (is.list(md) && isTRUE(md$complete)) return("green")
  df <- iphra_log_get(lg, "log_df")
  if (is.data.frame(df) && nrow(df) > 0L) "yellow" else "red"
}

#' Overall status per workflow step (named character vector). Steps without
#' criteria yet are "grey".
#' @noRd
iphra_step_status <- function(datasets, steps, goals = NULL, tools = NULL) {
  out <- stats::setNames(rep("grey", length(steps)), names(steps))
  if (!is.null(tools)) {
    out["tools"] <- if (tools$n == 0L) "red" else if (tools$done >= tools$n) "green" else "yellow"
  }
  if (!is.null(goals)) {
    out["goals"] <- if (!isTRUE(goals$any)) "red" else if (isTRUE(goals$complete)) "green" else "yellow"
  }
  ds <- datasets %||% list()
  if (length(ds)) out["import"] <- "green" else out["import"] <- "red"

  std <- vapply(ds, function(o) "standardized" %in% tryCatch(iphra_available_stages(o), error = function(e) character(0)),
                logical(1))
  out["standardization"] <- if (!length(ds) || !any(std)) "red" else if (all(std)) "green" else "yellow"

  st <- unlist(lapply(ds, function(o) c(iphra_log_status(o, "cleaning"), iphra_log_status(o, "deletion"))))
  out["cleaning"] <- if (!length(st) || all(st == "red")) "red"
  else if (all(st == "green")) "green" else "yellow"
  out
}

#' Session-wide "Goals and Objectives marked complete" flag (reactiveVal),
#' created on first use so the goals module and the sidebar status share it.
#' @noRd
iphra_goals_complete_rv <- function(session) {
  ud <- session$userData
  if (!is.function(ud$goals_complete)) ud$goals_complete <- shiny::reactiveVal(FALSE)
  ud$goals_complete
}

#' Session-wide set of tool roles marked complete on the Tool Design tab
#' (reactiveVal holding a character vector).
#' @noRd
iphra_tools_complete_rv <- function(session) {
  ud <- session$userData
  if (!is.function(ud$tools_complete)) ud$tools_complete <- shiny::reactiveVal(character(0))
  ud$tools_complete
}

#' Completion state of the Goals and Objectives step from a protocol object.
#' @return list(info, primary, secondary, sources, any)
#' @noRd
iphra_goals_state <- function(proto) {
  filled <- function(x) {
    !is.null(x) && length(x) > 0 && any(!is.na(x) & nzchar(trimws(as.character(x))))
  }
  meta <- tryCatch(proto$get(field = "..metadata"), error = function(e) NULL)
  info_fields <- c("assessment_title", "population", "geographic_coverage", "month_year")
  info <- !is.null(meta) &&
    all(vapply(info_fields, function(f) filled(meta[[f]]), logical(1))) &&
    (filled(meta$country_name) || filled(meta$country))
  info_any <- !is.null(meta) &&
    any(vapply(c(info_fields, "country_name", "country", "research_cycle_id", "rationale"),
               function(f) filled(meta[[f]]), logical(1)))
  fw <- function(m) tryCatch(proto$get(field = "framework", member = m), error = function(e) NULL)
  primary <- length(fw("primary_objectives")) > 0
  secondary <- length(fw("secondary_objectives")) > 0
  src <- fw("secondary_data_sources")
  sources <- is.data.frame(src) && nrow(src) > 0 &&
    any(!is.na(src$source) & nzchar(trimws(as.character(src$source))))
  list(info = info, primary = primary, secondary = secondary, sources = sources,
       any = info_any || primary || secondary || sources)
}

#' @noRd
iphra_workflow_status_server <- function(input, output, session, iphra_session, lang_r) {
  tx <- function(x) phrutils::phr_txt(x, lang = lang_r(), default = gsub("\\{", "{{", x))
  steps <- iphra_workflow_steps()
  project <- session$userData$project

  ds_r <- tryCatch(iphra_session$get_module_reactive("datasets"), error = function(e) function() NULL)
  datasets <- shiny::reactive(ds_r() %||% list())

  proto_r <- tryCatch(iphra_session$get_module_reactive("protocol"), error = function(e) function() NULL)
  goals_complete <- iphra_goals_complete_rv(session)
  goals_state <- shiny::reactive({
    p <- proto_r()
    if (is.null(p)) NULL else iphra_goals_state(p)
  })

  tools_complete <- iphra_tools_complete_rv(session)
  tools_state <- shiny::reactive({
    p <- proto_r()
    if (is.null(p)) return(NULL)
    roles <- tryCatch(as.character(p$get_tool_names()), error = function(e) character(0))
    list(n = length(roles), done = length(intersect(roles, tools_complete())))
  })

  shiny::observe({
    gs <- goals_state()
    goals <- if (is.null(gs)) NULL else list(any = gs$any, complete = goals_complete())
    st <- tryCatch(iphra_step_status(datasets(), steps, goals, tools_state()), error = function(e) NULL)
    if (!is.null(st)) session$sendCustomMessage("phr_step_status", as.list(st))
  })

  output$phr_goals_status <- shiny::renderUI({
    if ((input$workflow_nav %||% "goals") != "goals") return(NULL)
    gs <- goals_state()
    if (is.null(gs)) return(NULL)
    col <- c(red = "#c62828", green = "#2e7d32")
    cell <- "text-align:center; padding:1px 6px; vertical-align:middle;"
    items <- list(
      list(tx("Assessment Info Complete"), gs$info),
      list(tx("Primary Objectives Selected"), gs$primary),
      list(tx("Secondary Objectives Selected"), gs$secondary),
      list(tx("Secondary Sources Included"), gs$sources)
    )
    shiny::tags$table(
      style = "margin-top:4px; font-size:8.5px; border-collapse:collapse;",
      shiny::tags$thead(shiny::tags$tr(lapply(items, function(i) {
        shiny::tags$th(style = paste0(cell, "font-weight:600; white-space:nowrap;"), i[[1]])
      }))),
      shiny::tags$tbody(shiny::tags$tr(lapply(items, function(i) {
        shiny::tags$td(style = cell, shiny::span(
          title = if (i[[2]]) tx("Yes") else tx("No"),
          style = paste0("display:inline-block; width:8px; height:8px; border-radius:50%; background:",
                         if (i[[2]]) col[["green"]] else col[["red"]], ";")
        ))
      })))
    )
  })

  output$phr_log_status <- shiny::renderUI({
    if ((input$workflow_nav %||% "goals") != "cleaning") return(NULL)
    ds <- datasets()
    if (!length(ds)) return(NULL)
    col <- c(red = "#c62828", yellow = "#f9a825", green = "#2e7d32")
    ttl <- c(red = tx("Not generated"), yellow = tx("Not marked complete"), green = tx("Marked complete"))
    dot <- function(s) {
      shiny::span(title = ttl[[s]], style = paste0("display:inline-block; width:8px; height:8px; border-radius:50%; background:", col[[s]], ";"))
    }
    cell <- "text-align:center; padding:1px 4px; vertical-align:middle;"
    shiny::tags$table(
      style = "margin-top:4px; font-size:8.5px; border-collapse:collapse;",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(style = cell),
        lapply(names(ds), function(nm) {
          shiny::tags$th(style = paste0(cell, "font-weight:600; white-space:nowrap;"), title = nm, nm)
        })
      )),
      shiny::tags$tbody(
        shiny::tags$tr(
          shiny::tags$td(style = paste0(cell, "text-align:right;"), tx("Cleaning")),
          lapply(names(ds), function(nm) shiny::tags$td(style = cell, dot(iphra_log_status(ds[[nm]], "cleaning"))))
        ),
        shiny::tags$tr(
          shiny::tags$td(style = paste0(cell, "text-align:right;"), tx("Deletion")),
          lapply(names(ds), function(nm) shiny::tags$td(style = cell, dot(iphra_log_status(ds[[nm]], "deletion"))))
        )
      )
    )
  })

  analytics_r <- tryCatch(iphra_session$get_module_reactive("analytics"), error = function(e) function() NULL)
  analytics <- shiny::reactive(analytics_r() %||% list())

  output$phr_std_status <- shiny::renderUI({
    if ((input$workflow_nav %||% "goals") != "standardization") return(NULL)
    ds <- datasets()
    if (!length(ds)) return(NULL)
    an_names <- as.character(names(analytics()))
    flag <- function(obj, f) isTRUE(tryCatch(obj$get(field = f), error = function(e) FALSE))
    col <- c(red = "#c62828", green = "#2e7d32")
    dot <- function(ok, ttl) {
      shiny::span(title = ttl, style = paste0("display:inline-block; width:8px; height:8px; border-radius:50%; background:", if (ok) col[["green"]] else col[["red"]], ";"))
    }
    cell <- "text-align:center; padding:1px 5px; vertical-align:middle;"
    row <- function(label, fn) {
      shiny::tags$tr(
        shiny::tags$td(style = paste0(cell, "text-align:right;"), label),
        lapply(names(ds), function(nm) shiny::tags$td(style = cell, fn(nm, ds[[nm]])))
      )
    }
    shiny::tags$table(
      style = "margin-top:4px; font-size:8.5px; border-collapse:collapse;",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(style = cell),
        lapply(names(ds), function(nm) shiny::tags$th(style = paste0(cell, "font-weight:600; white-space:nowrap;"), nm))
      )),
      shiny::tags$tbody(
        row(tx("Validation"), function(nm, o) {
          ok <- flag(o, "..validated"); dot(ok, if (ok) tx("Validated") else tx("Not validated"))
        }),
        row(tx("Standardization"), function(nm, o) {
          ok <- flag(o, "..standardized"); dot(ok, if (ok) tx("Standardized") else tx("Not standardized"))
        }),
        row(tx("Generate Analytics"), function(nm, o) {
          ok <- any(startsWith(an_names, paste0(nm, " / ")))
          dot(ok, if (ok) tx("Analytics generated") else tx("Not generated"))
        })
      )
    )
  })

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
      shiny::tags$style(shiny::HTML(
        "body.phr-guidance-collapsed .phr-guidance__extra{display:none;}
         .phr-guidance__toggle{cursor:pointer; margin-right:6px; color:#555; background:none; border:0; padding:0 4px;}
         body.phr-guidance-collapsed .phr-guidance__toggle .glyphicon{transform:rotate(-90deg);}"
      )),
      shiny::tags$button(
        type = "button", class = "phr-guidance__toggle", title = tx("Collapse / expand"),
        onclick = "document.body.classList.toggle('phr-guidance-collapsed');",
        shiny::span(class = "glyphicon glyphicon-chevron-down")
      ),
      shiny::div(
        class = "phr-guidance__text",
        shiny::div(class = "phr-guidance__title",
                   sprintf("%s %d/%d: %s", tx("Step"), idx, length(steps), tx(s$title))),
        shiny::div(
          class = "phr-guidance__extra",
          shiny::div(class = "phr-guidance__hint", tx(s$hint)),
          if (key == "cleaning") shiny::uiOutput("phr_log_status"),
          if (key == "goals") shiny::uiOutput("phr_goals_status"),
          if (key == "standardization") shiny::uiOutput("phr_std_status")
        )
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
