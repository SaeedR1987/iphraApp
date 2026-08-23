# Project File I/O ####
#
# This file implements the persistence layer for the "Save Project" and
# "Open Project" actions in the app navbar.
#
# A project is persisted as a single `.iphra` file (an RDS payload with a
# custom extension) containing a snapshot of the serializable parts of
# `session$userData` established by `init_session()`:
#
#   * `phr_session`         — IPHRASession R6 object's serialized state
#                             (project name, metadata, module state)
#   * `settings`            — user preferences (language, theme, livelihoods
#                             mapping, ...)
#   * `project`             — project-level reactiveValues (name, description,
#                             data pointers, indicators, summary, params, ...)
#   * `modules`             — module-specific R6 objects such as the
#                             `IPHRAProtocol` (with its metadata, framework
#                             SVGs, tools, sample table, objectives, ...).
#                             R6 objects are saved directly via `saveRDS()`;
#                             they round-trip through R6 provided their class
#                             generator (e.g. `phr::IPHRAProtocol`) is loaded
#                             at read time.
#   * `ui$active_tab`       — remembers the last active tab so a re-opened
#                             project lands the user back where they were.
#
# Purely transient parts of the session (session_id, session_start, flags,
# temp caches, reactive `modules_version` counters, ...) are intentionally
# excluded — they are re-created by `init_session()` and re-established for
# each loaded module via `set_module()` / `phr_touch_module()`.

# Current on-disk version tag for the .iphra format. Bump on breaking changes.
.IPHRA_FILE_VERSION <- "1.0.0"

# Canonical file extension for iphraApp project files.
.IPHRA_FILE_EXT <- "iphra"


#' @title Build a Serializable Snapshot of the Current Session
#' @description
#' Collects the serializable parts of `session$userData` (see file header)
#' into a single named list suitable for `saveRDS()`. R6 objects registered
#' as modules (e.g. `IPHRAProtocol`) are included directly.
#'
#' @param session Shiny session object.
#' @return A named list describing the current project.
#' @keywords internal
iphra_build_project_snapshot <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("[IPHRA::Error] No active Shiny session found; cannot build snapshot.")
  }

  phr_session <- get_session(session)

  # Ensure the PHRSession's project name reflects the reactive `project$name`
  # if the user has renamed the project in the UI.
  project_rv <- session$userData$project
  project_name_rv <- if (!is.null(project_rv)) shiny::isolate(project_rv$name) else NULL
  if (!is.null(project_name_rv) &&
      is.character(project_name_rv) &&
      nzchar(project_name_rv)) {
    tryCatch(phr_session$set_project_name(project_name_rv), error = function(e) NULL)
  }

  settings_snap <- if (!is.null(session$userData$settings)) {
    shiny::isolate(shiny::reactiveValuesToList(session$userData$settings))
  } else {
    list()
  }

  project_snap <- if (!is.null(project_rv)) {
    shiny::isolate(shiny::reactiveValuesToList(project_rv))
  } else {
    list()
  }

  ui_snap <- if (!is.null(session$userData$ui)) {
    ui_rv <- shiny::isolate(shiny::reactiveValuesToList(session$userData$ui))
    # Only preserve stable pieces; drop transient notification queues.
    list(active_tab = ui_rv$active_tab)
  } else {
    list()
  }

  # `modules` is a plain list of R6 (or plain) objects — save them as-is.
  # R6 objects round-trip through saveRDS/readRDS as long as their class
  # generator is available at read time.
  modules_snap <- if (is.list(session$userData$modules)) {
    session$userData$modules
  } else {
    list()
  }

  list(
    iphra_file_version = .IPHRA_FILE_VERSION,
    saved_at           = Sys.time(),
    phr_session        = phr_session$serialize(),
    settings           = settings_snap,
    project            = project_snap,
    ui                 = ui_snap,
    modules            = modules_snap
  )
}


#' @title Save the Current Project to a `.iphra` File
#' @description
#' Serializes the current session's project state (see
#' [iphra_build_project_snapshot()]) and writes it to disk as an RDS payload
#' with an `.iphra` extension.
#'
#' @param path Character. Destination file path. If it does not end in
#'   `.iphra`, that extension is appended.
#' @param session Shiny session object.
#' @return Invisibly returns the (possibly extension-adjusted) file path.
#' @export
iphra_save_project_file <- function(path,
                                    session = shiny::getDefaultReactiveDomain()) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("[IPHRA::Error] path must be a single non-empty character string.")
  }

  # Ensure the destination directory exists.
  dir_path <- dirname(path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Ensure canonical `.iphra` extension.
  if (!grepl(paste0("\\.", .IPHRA_FILE_EXT, "$"), tolower(path))) {
    path <- paste0(path, ".", .IPHRA_FILE_EXT)
  }

  snapshot <- iphra_build_project_snapshot(session)
  saveRDS(snapshot, file = path)

  # Update last_saved marker so the UI can react to persistence events.
  if (!is.null(session) && !is.null(session$userData$project)) {
    session$userData$project$last_saved <- Sys.time()
    session$userData$project$path <- path
  }
  iphra_set_unsaved_changes(FALSE, session)

  invisible(path)
}


#' @title Restore Session State from a `.iphra` File
#' @description
#' Reads a `.iphra` project file previously written by
#' [iphra_save_project_file()] and reinitializes the current session's
#' serializable state:
#'
#' * The `PHRSession` R6 object is repopulated via `deserialize()`.
#' * `session$userData$settings` and `session$userData$project`
#'   reactiveValues are updated key-by-key so existing reactive dependencies
#'   fire correctly.
#' * Each entry in the saved `modules` list is re-registered with
#'   [set_module()], which stores the object and bumps its reactive
#'   version counter so downstream `phr_get_module_reactive()` consumers
#'   pick up the loaded object.
#'
#' Purely transient parts of the session (flags, temp caches, session ids)
#' are left untouched and continue to be managed by `init_session()`.
#'
#' @param path Character. Path to a `.iphra` file.
#' @param session Shiny session object.
#' @return Invisibly returns the loaded snapshot list.
#' @export
iphra_load_project_file <- function(path,
                                    session = shiny::getDefaultReactiveDomain()) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("[IPHRA::Error] path must be a single non-empty character string.")
  }
  if (!file.exists(path)) {
    stop(paste0("[IPHRA::Error] File not found: ", path))
  }

  snapshot <- tryCatch(
    readRDS(path),
    error = function(e) {
      stop(paste0(
        "[IPHRA::Error] Could not read '", path,
        "' as an .iphra project file: ", conditionMessage(e)
      ))
    }
  )

  if (!is.list(snapshot)) {
    stop("[IPHRA::Error] Loaded file did not contain an IPHRA project snapshot.")
  }

  # Signal to observers that a load is in progress; they may want to skip
  # marking the session dirty while restoring state.
  if (!is.null(session) && !is.null(session$userData$flags)) {
    session$userData$flags$load_in_progress <- TRUE
    on.exit({
      if (!is.null(session$userData$flags)) {
        session$userData$flags$load_in_progress <- FALSE
      }
    }, add = TRUE)
  }

  phr_session <- get_session(session)

  # 1. PHRSession serialized state
  if (!is.null(snapshot$phr_session) && is.list(snapshot$phr_session)) {
    phr_session$deserialize(snapshot$phr_session, merge = FALSE)
  }

  # 2. Settings (transient reactiveValues, but user preferences persist)
  if (is.list(snapshot$settings) && !is.null(session$userData$settings)) {
    for (key in names(snapshot$settings)) {
      session$userData$settings[[key]] <- snapshot$settings[[key]]
    }
  }

  # 3. Project reactiveValues (name, description, data pointers, params, ...)
  if (is.list(snapshot$project) && !is.null(session$userData$project)) {
    for (key in names(snapshot$project)) {
      session$userData$project[[key]] <- snapshot$project[[key]]
    }
    session$userData$project$path <- path
  }

  # 4. UI state (last active tab)
  if (is.list(snapshot$ui) && !is.null(session$userData$ui) &&
      !is.null(snapshot$ui$active_tab)) {
    session$userData$ui$active_tab <- snapshot$ui$active_tab
  }

  # 5. Module R6 objects (IPHRAProtocol, ...). Re-register via set_module()
  #    so their reactive version signals are (re)created and bumped, causing
  #    any `phr_get_module_reactive()` consumers to re-read the new object.
  if (is.list(snapshot$modules)) {
    for (module_name in names(snapshot$modules)) {
      module_obj <- snapshot$modules[[module_name]]
      if (!is.null(module_obj)) {
        set_module(module_name, module_obj, session = session)
      }
    }
  }

  iphra_set_unsaved_changes(FALSE, session)

  invisible(snapshot)
}
