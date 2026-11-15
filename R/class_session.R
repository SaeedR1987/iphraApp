# IPHRA Session State Management
#
# `PHRSession` is the single project-level state object of the app. It inherits
# from `phr::Asset`, so it reuses Asset's `get()` / `set()` / `call()` access
# API, its private metadata (`..metadata$version`, created/modified
# timestamps), its change log (`..touch()` / `..track()`) and its translation
# helper (`..phr_txt()`).
#
# ## Session structure (session$userData)
#
#   session$userData$
#   ├── session_id          # Unique session identifier
#   ├── session_start       # Session start timestamp
#   ├── phr_session         # PHRSession R6 object (modules + project metadata)
#   ├── settings            # reactiveValues: language, theme, auto_save, ...
#   ├── project             # reactiveValues: name, description, data, ...
#   ├── ui                  # reactiveValues: active_tab, last_action, ...
#   ├── flags               # reactiveValues: is_busy, has_unsaved_changes, ...
#   └── temp                # reactiveValues: cache, upload_progress, ...
#
# ## Modules and reactivity
#
# Module objects (e.g. the `IPHRAProtocol` R6 object) are stored ONCE, inside
# the PHRSession (`private$..modules`). They are also what gets saved/loaded
# (see `serialize()` / `deserialize()`); there is no separate copy in
# `session$userData`.
#
# R6 objects are plain mutable environments: mutating one does NOT invalidate
# any Shiny reactive context. PHRSession therefore keeps reactiveVal signals
# that mirror `..metadata$version` (bumped by Asset's `..touch()`):
#
#   1. Register / replace a module:   `session$set_module(name, object)`.
#   2. After mutating the object:     `session$touch_module(name)`.
#   3. Read reactively:               `session$get_module_reactive(name)`
#        protocol_r <- phr_get_module_reactive("protocol", session)
#        protocol_r()$tools$tool_household_iphra_v2
#
# Always mutate through these wrappers: calling `set(field = "..modules", ...)`
# directly would bump `..metadata$version` but not notify Shiny.


#' @title Null Coalescing Operator
#' @description
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @param x The left-hand side value.
#' @param y The right-hand side value (default).
#' @return x if not NULL, otherwise y.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' @title PHR Session State Object
#' @description
#' R6 class (inheriting from `phr::Asset`) managing the project-level state of
#' the app: the registry of module objects, project metadata, reactive
#' change signals, and serialization (save/load) of project files.
#'
#' @export
PHRSession <- R6::R6Class(
  "PHRSession",
  inherit = phr::Asset,

  public = list(

    #' @description
    #' Initialize a new PHRSession object.
    #'
    #' @param project_name Optional project name for the session.
    #' @return A new PHRSession object.
    initialize = function(project_name = NULL) {
      super$initialize(
        name = project_name %||% paste0("PHR_Project_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      )

      private$..append_app_translations()
      private$..app_metadata <- list(
        app_version = tryCatch(
          as.character(utils::packageVersion("iphraApp")),
          error = function(e) "0.0.0.9000"
        ),
        r_version = R.version.string
      )
      private$..version_rv <- shiny::reactiveVal(private$..metadata$version)
      invisible(self)
    },

    #' @description
    #' Session-wide reactive signal. Calling the returned function inside a
    #' reactive context creates a dependency on ANY change to the session
    #' (it mirrors `..metadata$version`).
    #' @return A `shiny::reactiveVal()` function.
    version_signal = function() {
      private$..version_rv
    },

    # Module registry ####

    #' @description
    #' Register (or replace) a module object and notify dependents.
    #'
    #' @param module_id Character string identifying the module.
    #' @param object The module object (typically an R6 object).
    #' @param serializable Logical; whether the module is saved with the project.
    #' @return Invisibly returns the module object.
    set_module = function(module_id, object, serializable = TRUE) {
      private$..assert_module_id(module_id)
      self$set(
        field = "..modules",
        role = module_id,
        value = object,
        update_modified = FALSE
      )
      private$..serializable[[module_id]] <- isTRUE(serializable)
      private$..touch(
        log = TRUE,
        name = module_id,
        message = private$..phr_txt("Registered module '{module_id}'.")
      )
      private$..notify(module_id)
      invisible(object)
    },

    #' @description
    #' Get a registered module object (non-reactive).
    #'
    #' @param module_id Character string identifying the module.
    #' @param default Value returned if the module is not registered.
    #' @return The module object, or `default`.
    get_module = function(module_id, default = NULL) {
      if (!self$has_module(module_id)) {
        return(default)
      }
      self$get(field = "..modules", role = module_id)
    },

    #' @description
    #' Notify dependents that a module's state changed. Call after mutating
    #' the module object (e.g. `protocol$add_tools()`).
    #'
    #' @param module_id Character string identifying the module.
    #' @return Invisibly returns the new session version.
    touch_module = function(module_id) {
      private$..assert_module_id(module_id)
      private$..touch(
        log = FALSE,
        name = module_id,
        message = private$..phr_txt("Touched module '{module_id}'.")
      )
      private$..notify(module_id)
      invisible(private$..metadata$version)
    },

    #' @description
    #' Reactive accessor for a module: depends on the module's signal and
    #' yields the current module object.
    #'
    #' @param module_id Character string identifying the module.
    #' @return A `shiny::reactive()` returning the module object (or `NULL`).
    get_module_reactive = function(module_id) {
      signal <- self$get_module_signal(module_id)
      shiny::reactive({
        signal()
        self$get_module(module_id)
      })
    },

    #' @description
    #' Get the reactive signal backing a module (created lazily).
    #'
    #' @param module_id Character string identifying the module.
    #' @return A `shiny::reactiveVal()` function.
    get_module_signal = function(module_id) {
      private$..assert_module_id(module_id)
      if (is.null(private$..module_signals[[module_id]])) {
        private$..module_signals[[module_id]] <- shiny::reactiveVal(private$..metadata$version)
      }
      private$..module_signals[[module_id]]
    },

    #' @description
    #' Check if a module has been registered.
    #'
    #' @param module_id Character string identifying the module.
    #' @return Logical.
    has_module = function(module_id) {
      is.character(module_id) && length(module_id) == 1L &&
        module_id %in% names(private$..modules)
    },

    #' @description
    #' List registered module IDs.
    #' @return Character vector.
    list_modules = function() {
      names(private$..modules) %||% character(0)
    },

    #' @description
    #' Remove a module from the session and notify dependents.
    #'
    #' @param module_id Character string identifying the module.
    #' @return Invisibly returns self.
    remove_module = function(module_id) {
      if (self$has_module(module_id)) {
        private$..modules[[module_id]] <- NULL
        private$..serializable[[module_id]] <- NULL
        private$..touch(
          log = TRUE,
          name = module_id,
          message = private$..phr_txt("Removed module '{module_id}'.")
        )
        private$..notify(module_id)
      }
      invisible(self)
    },

    # Project metadata ####

    #' @description
    #' Get the project name.
    #' @return Character string.
    get_project_name = function() {
      self$get(field = "..name")
    },

    #' @description
    #' Set the project name.
    #'
    #' @param name Non-empty character string.
    #' @return Invisibly returns self.
    set_project_name = function(name) {
      phrutils::phr_assert(
        is.character(name) && length(name) == 1L && nzchar(name),
        message = private$..phr_txt("Project name must be a non-empty character string."),
        origin = "PHRSession$set_project_name"
      )
      self$set(field = "..name", value = name, update_modified = FALSE)
      private$..touch()
      private$..notify()
      invisible(self)
    },

    # Serialization (save/load) ####

    #' @description
    #' Serialize the session (project metadata, change log and serializable
    #' modules) to a list suitable for `saveRDS()`.
    #' @return Named list.
    serialize = function() {
      keep <- vapply(
        names(private$..modules),
        function(id) isTRUE(private$..serializable[[id]] %||% TRUE),
        logical(1)
      )
      list(
        phr_session_version = .PHR_SESSION_VERSION,
        project_name = private$..name,
        metadata = private$..metadata,
        app_metadata = private$..app_metadata,
        change_log = private$..change_log,
        modules = private$..modules[names(private$..modules)[keep]]
      )
    },

    #' @description
    #' Restore the session from a list produced by `serialize()` (legacy
    #' `iphra_session_version` / `state` payloads are also accepted).
    #'
    #' @param data Named list.
    #' @param merge Logical. If `TRUE`, keep modules not present in `data`.
    #' @return Invisibly returns self.
    deserialize = function(data, merge = FALSE) {
      phrutils::phr_assert(
        is.list(data),
        message = private$..phr_txt("Data must be a list."),
        origin = "PHRSession$deserialize"
      )
      if (!any(c("phr_session_version", "iphra_session_version") %in% names(data))) {
        warning(private$..phr_txt(
          "Missing phr_session_version in loaded data; proceeding with caution"
        ), call. = FALSE)
      }

      if (!isTRUE(merge)) {
        for (id in names(private$..modules)) {
          private$..modules[[id]] <- NULL
        }
        private$..serializable <- list()
      }

      if (!is.null(data$project_name)) {
        private$..name <- data$project_name
      }
      if (is.list(data$app_metadata)) {
        private$..app_metadata <- data$app_metadata
      } else if (is.list(data$metadata) && !is.null(data$metadata$app_version)) {
        private$..app_metadata <- data$metadata
      }
      if (is.list(data$metadata) && !is.null(data$metadata$created_datetime)) {
        private$..metadata$created_datetime <- data$metadata$created_datetime
      } else if (!is.null(data$created_at)) {
        private$..metadata$created_datetime <- data$created_at
      }
      if (!is.null(data$change_log)) {
        private$..change_log <- data$change_log
      }

      # Legacy payloads stored plain list state under `state`.
      modules <- c(data$modules, data$state[setdiff(names(data$state), names(data$modules))])
      for (id in names(modules)) {
        self$set_module(id, modules[[id]])
      }

      private$..touch(
        log = TRUE,
        name = private$..name,
        message = private$..phr_txt("Deserialized project '{private$..name}'.")
      )
      private$..notify()
      invisible(self)
    },

    #' @description
    #' Save the session to an `.rds` file.
    #'
    #' @param path Character string with the file path.
    #' @return Invisibly returns the file path.
    save_project = function(path) {
      phrutils::phr_assert(
        is.character(path) && length(path) == 1L,
        message = private$..phr_txt("Path must be a single character string."),
        origin = "PHRSession$save_project"
      )
      dir_path <- dirname(path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      if (!grepl("\\.rds$", tolower(path))) {
        path <- paste0(path, ".rds")
      }
      saveRDS(self$serialize(), file = path)
      invisible(path)
    },

    #' @description
    #' Load the session from an `.rds` file written by `save_project()`.
    #'
    #' @param path Character string with the file path.
    #' @param merge Logical; see `deserialize()`.
    #' @return Invisibly returns self.
    load_project = function(path, merge = FALSE) {
      phrutils::phr_assert(
        file.exists(path),
        message = private$..phr_txt("File not found: {path}"),
        origin = "PHRSession$load_project"
      )
      self$deserialize(readRDS(path), merge = merge)
      invisible(self)
    },

    #' @description
    #' Print a summary of the session.
    print = function() {
      cat(private$..phr_txt("PHR Session State"), "\n", sep = "")
      cat(strrep("-", 20), "\n", sep = "")
      cat(private$..phr_txt("Project"), ": ", private$..name, "\n", sep = "")
      cat(private$..phr_txt("Created"), ": ",
          format(private$..metadata$created_datetime, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
      cat(private$..phr_txt("Modified"), ": ",
          format(private$..metadata$modified_datetime, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
      cat(private$..phr_txt("Version"), ": ", private$..metadata$version, "\n", sep = "")
      cat(private$..phr_txt("Modules"), ": ", length(private$..modules), "\n", sep = "")
      for (id in names(private$..modules)) {
        cat("  - ", id, "\n", sep = "")
      }
      invisible(self)
    }
  ),

  private = list(

    # @field ..modules Named list of registered module objects.
    ..modules = list(),

    # @field ..serializable Named list of logicals: is the module saved?
    ..serializable = list(),

    # @field ..app_metadata App/R version information.
    ..app_metadata = NULL,

    # @field ..version_rv Session-wide reactiveVal mirroring `..metadata$version`.
    ..version_rv = NULL,

    # @field ..module_signals Named list of per-module reactiveVals.
    ..module_signals = list(),

    # Mirror `..metadata$version` (already bumped by `..touch()`) into the
    # session signal and, optionally, one module's signal.
    ..notify = function(module_id = NULL) {
      version <- private$..metadata$version
      if (!is.null(module_id) && !is.null(private$..module_signals[[module_id]])) {
        private$..module_signals[[module_id]](version)
      }
      if (is.function(private$..version_rv)) {
        private$..version_rv(version)
      }
      invisible(NULL)
    },

    ..assert_module_id = function(module_id) {
      phrutils::phr_assert(
        is.character(module_id) && length(module_id) == 1L && nzchar(module_id),
        message = private$..phr_txt("module_id must be a single non-empty character string."),
        origin = "PHRSession"
      )
    },

    ..append_app_translations = function() {
      path <- system.file("resources", "translations.json", package = "iphraApp")
      if (nzchar(path)) {
        private$..append_translations(path = path)
      }
      invisible(NULL)
    },

    # Allow Asset's get()/set() to reach the module registry.
    ..additional_get_fields = function() {
      c("..modules", "..app_metadata")
    },
    ..additional_set_fields = function() {
      c("..modules")
    }
  )
)

# On-disk version tag of the serialized PHRSession payload.
.PHR_SESSION_VERSION <- "2.0.0"
