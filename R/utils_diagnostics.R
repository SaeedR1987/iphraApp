#' Describe a condition in full (never blank)
#'
#' Some phr/phrutils/rlang conditions have an empty `conditionMessage()`,
#' which is what produces the bare `Error: ` in the console.
#'
#' @param e A condition object.
#' @return A single character string.
#' @noRd
iphra_describe_condition <- function(e) {
  msg <- tryCatch(conditionMessage(e), error = function(...) "")
  if (!nzchar(trimws(msg))) {
    msg <- tryCatch(rlang::cnd_message(e), error = function(...) "")
  }
  if (!nzchar(trimws(msg))) {
    msg <- "<empty condition message>"
  }
  call_txt <- tryCatch(
    paste(deparse(conditionCall(e), width.cutoff = 120L), collapse = " "),
    error = function(...) "<unknown>"
  )
  paste0(
    "class: ", paste(class(e), collapse = " / "), "\n",
    "message: ", msg, "\n",
    "call: ", call_txt
  )
}

#' Run an expression, logging full details on failure
#'
#' Logs the condition class, message, call and a traceback to the console,
#' shows a notification in the session (if any), then re-throws an error
#' with a non-empty message.
#'
#' @param expr Expression to evaluate.
#' @param stage Short label describing the start-up stage.
#' @param session Shiny session (optional).
#' @noRd
iphra_startup_guard <- function(expr, stage,
                                session = shiny::getDefaultReactiveDomain()) {
  withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        details <- iphra_describe_condition(e)
        full <- paste0("[iphraApp] Start-up failed at stage '", stage, "'\n", details)
        message(full)
        if (!is.null(session)) {
          try(
            shiny::showNotification(
              paste0("Start-up failed at '", stage, "': ", conditionMessage(e)),
              type = "error",
              duration = NULL
            ),
            silent = TRUE
          )
        }
        stop(full, call. = FALSE)
      }
    ),
    error = function(e) {
      tr <- tryCatch(
        paste(utils::capture.output(print(rlang::trace_back())), collapse = "\n"),
        error = function(...) ""
      )
      if (nzchar(tr)) message("[iphraApp] Backtrace at '", stage, "':\n", tr)
    }
  )
}

#' Check that the installed phr / phrutils expose what iphraApp uses
#'
#' Returns (and messages) every mismatch at once so an incompatible
#' phr/phrutils version is reported clearly instead of crashing mid-start-up.
#'
#' @return Invisibly, a character vector of problems (empty if none).
#' @noRd
iphra_check_phr_api <- function() {
  problems <- character()

  need <- list(
    phr = c(
      "SurveyProtocol", "HouseholdTool", "KeyInformantTool", "ObservationTool",
      "calculate_sample_size_general", "calculate_sample_size_individual",
      "calculate_sample_size_rate", "estimate_field_plan"
    ),
    phrutils = c(
      "phr_txt", "phr_load_translations", "set_phr_language", "phr_message",
      "phr_warning", "phr_error", "phr_assert", "phr_try", "phr_try_step"
    )
  )

  for (pkg in names(need)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      problems <- c(problems, paste0("Package '", pkg, "' is not installed."))
      next
    }
    exports <- getNamespaceExports(pkg)
    missing <- setdiff(need[[pkg]], exports)
    if (length(missing)) {
      problems <- c(problems, paste0(
        pkg, " (", as.character(utils::packageVersion(pkg)),
        ") no longer exports: ", paste(missing, collapse = ", ")
      ))
    }
  }

  # IPHRAProtocol$initialize forwards these args to SurveyProtocol$initialize
  if ("SurveyProtocol" %in% tryCatch(getNamespaceExports("phr"), error = function(e) character())) {
    init <- tryCatch(phr::SurveyProtocol$public_methods$initialize, error = function(e) NULL)
    if (is.function(init)) {
      fmls <- names(formals(init))
      if (!"..." %in% fmls) {
        sent <- c("assessment_title", "country_name", "month_year",
                  "framework_type", "reference_doc_filename")
        bad <- setdiff(sent, fmls)
        if (length(bad)) {
          problems <- c(problems, paste0(
            "phr::SurveyProtocol$initialize() has no argument(s): ",
            paste(bad, collapse = ", "),
            " (formals: ", paste(fmls, collapse = ", "), ")"
          ))
        }
      }
    }
  }

  if (length(problems)) {
    message(
      "[iphraApp] phr/phrutils compatibility problems detected:\n  - ",
      paste(problems, collapse = "\n  - ")
    )
  }
  invisible(problems)
}
