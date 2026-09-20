# IPHRA Session State Management

#
# This module provides a comprehensive session state management system for the
# IPHRA Shiny application. It combines:
#
# 1. **IPHRASession R6 Class** - For project-level serializable state (save/load)
# 2. **Reactive Session Structure** - For runtime state (settings, UI, flags, temp)
# 3. **Checkbox Status Management** - For tracking step completion across modules
#
# ## Session Structure (session$userData)
#
# After calling iphra_init_session(), the following structure is available:
#
#   session$userData$
#   ├── session_id          # Unique session identifier
#   ├── session_start       # Session start timestamp
#   ├── iphra_session       # IPHRASession R6 object (serializable project state)
#   ├── settings            # reactiveValues: language, theme, auto_save, etc.
#   ├── project             # reactiveValues: name, description, data, cleaned, etc.
#   ├── modules             # List of module-specific R6 or reactive objects
#   ├── ui                  # reactiveValues: active_tab, last_action, notifications
#   ├── flags               # reactiveValues: is_busy, has_unsaved_changes, etc.
#   ├── temp                # reactiveValues: cache, upload_progress, preview_data
#   └── checkbox_status     # reactiveValues: status of completion checkboxes
#
# ## Checkbox Status Structure
#
# The checkbox_status reactiveValues contains nested structures for each module area:
#
#   checkbox_status$
#   ├── data                # Data import/processing steps
#   │   ├── household_data_imported
#   │   ├── roster_data_imported
#   │   └── ...
#   ├── cleaning            # Data cleaning steps
#   │   ├── main_cleaning_complete
#   │   ├── roster_cleaning_complete
#   │   └── ...
#   ├── quality             # Data quality checks
#   │   ├── general_quality_complete
#   │   ├── nutrition_quality_complete
#   │   └── ...
#   └── analysis            # Analysis steps
#       ├── demographics_analysis_complete
#       ├── nutrition_analysis_complete
#       └── ...
#   └── temp                # reactiveValues: cache, upload_progress, preview_data
#
# ## Best Practices
#
# ### 1. For Serializable Project Data (saved with project file):
#   iphra_session <- iphra_get_session(session)
#   iphra_session$register_module("goals", list(selected = character(0)))
#   iphra_session$set("goals", "selected", selected())
#
# ### 2. For Runtime Settings (user preferences):
#   session$userData$settings$language <- "fr"
#
# ### 3. For UI/Interaction State (transient):
#   session$userData$ui$active_tab <- "analysis"
#   session$userData$flags$is_busy <- TRUE
#
# ### 4. For Temporary Data (never serialized):
#   session$userData$temp$cache$preview <- some_data
#
# ### 5. For R6 Objects Stored in session$userData$modules (e.g. IPHRAProtocol):
#   R6 objects are plain mutable environments — assigning to or mutating one
#   of their fields (e.g. `protocol$add_tools(...)`) does NOT invalidate any
#   Shiny reactive context. Reading an R6 field directly inside a `reactive()`
#   / `observe()` / `render*()` block will only ever compute once and then
#   silently go stale.
#
#   To make an R6 object stored under `session$userData$modules` reactive:
#     1. Register / replace it with `iphra_set_module(name, object, session)`.
#     2. After ANY call that mutates the object's state, notify dependents by
#        calling `iphra_touch_module(name, session)`.
#     3. Never read the object directly for reactive purposes. Instead, use
#        `iphra_get_module_reactive(name, session)`, which returns a
#        `shiny::reactive()` that depends on the module's version counter and
#        yields the current object reference:
#          protocol_r <- iphra_get_module_reactive("protocol", session)
#          protocol_r()$tools$tool_household_iphra_v2
#   This "version counter" is the only reactive signal involved; the R6
#   object itself remains a plain mutable object. This is the same technique
#   originally hand-rolled as the `protocol_tools` reactiveVal, generalized so
#   every R6 module in `session$userData$modules` can follow one convention.
#



# Null-coalescing operator (if not already available)


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


# R6 Class: PHRSession


#' @title PHR Session State Object
#' @description
#' R6 class for managing session state across multiple modules.
#' Provides methods for state storage, retrieval, serialization (save),
#' and deserialization (load) of project files.
#'
#' @export
PHRSession <- R6::R6Class(
  "IPHRASession",
  public = list(
    #' @description
    #' Initialize a new PHRSession object.
    #'
    #' @param project_name Optional project name for the session.
    #' @return A new PHRSession object.
    initialize = function(project_name = NULL) {
      private$.project_name <- project_name %||% paste0("IPHRA_Project_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      private$.created_at <- Sys.time()
      private$.modified_at <- Sys.time()
      private$.version <- "1.0.0"
      private$.state <- list()
      private$.metadata <- list(
        app_version = tryCatch(
          as.character(utils::packageVersion(project_name)),
          error = function(e) "0.0.0.9000"
        ),
        r_version = R.version.string
      )
      # Reactive version counter, bumped on every `.touch()`. Lets Shiny
      # reactive contexts depend on changes to this (otherwise non-reactive)
      # R6 object's state via `get_version_signal()` / `phr_reactive_state()`.
      private$.version_signal <- shiny::reactiveVal(0)
      invisible(self)
    },

    #' @description
    #' Get the reactive version signal for this session object. Calling the
    #' returned function inside a Shiny reactive context (`reactive()`,
    #' `observe()`, `render*()`) creates a dependency that invalidates
    #' whenever the session state changes (i.e. whenever `set()`/`deserialize()`
    #' /`clear_module()`/etc. are called).
    #'
    #' @return A `shiny::reactiveVal()` function.
    get_version_signal = function() {
      private$.version_signal
    },


    # Module Registration


    #' @description
    #' Register a module with its default state structure.
    #' This should be called during module initialization.
    #'
    #' @param module_id Character string identifying the module.
    #' @param defaults Named list of default values for the module's state.
    #' @param serializable Logical vector or named list indicating which keys
    #'   should be serialized when saving. Default is TRUE (all keys serializable).
    #' @return Invisibly returns self for method chaining.
    register_module = function(module_id, defaults = list(), serializable = TRUE) {
      if (!is.character(module_id) || length(module_id) != 1) {
        stop("module_id must be a single character string")
      }

      if (!module_id %in% names(private$.state)) {
        private$.state[[module_id]] <- defaults
      } else {
        # Merge with existing, keeping current values
        for (key in names(defaults)) {
          if (!key %in% names(private$.state[[module_id]])) {
            private$.state[[module_id]][[key]] <- defaults[[key]]
          }
        }
      }

      # Track serialization preferences
      if (is.logical(serializable) && length(serializable) == 1) {
        private$.serializable[[module_id]] <- rep(serializable, length(defaults))
        names(private$.serializable[[module_id]]) <- names(defaults)
      } else if (is.list(serializable) || is.logical(serializable)) {
        private$.serializable[[module_id]] <- as.list(serializable)
      }

      private$.touch()
      invisible(self)
    },


    # State Getters and Setters


    #' @description
    #' Set a state value for a specific module.
    #'
    #' @param module_id Character string identifying the module.
    #' @param key Character string identifying the state key.
    #' @param value The value to store.
    #' @return Invisibly returns self for method chaining.
    set = function(module_id, key, value) {
      if (!module_id %in% names(private$.state)) {
        private$.state[[module_id]] <- list()
      }
      private$.state[[module_id]][[key]] <- value
      private$.touch()
      invisible(self)
    },

    #' @description
    #' Get a state value for a specific module.
    #'
    #' @param module_id Character string identifying the module.
    #' @param key Character string identifying the state key.
    #' @param default Default value to return if key is not found.
    #' @return The stored value, or the default if not found.
    get = function(module_id, key, default = NULL) {
      if (!module_id %in% names(private$.state)) {
        return(default)
      }
      if (!key %in% names(private$.state[[module_id]])) {
        return(default)
      }
      private$.state[[module_id]][[key]]
    },

    #' @description
    #' Get all state values for a specific module.
    #'
    #' @param module_id Character string identifying the module.
    #' @return Named list of all state values for the module, or empty list.
    get_module_state = function(module_id) {
      if (!module_id %in% names(private$.state)) {
        return(list())
      }
      private$.state[[module_id]]
    },

    #' @description
    #' Check if a module has been registered.
    #'
    #' @param module_id Character string identifying the module.
    #' @return Logical indicating whether the module is registered.
    has_module = function(module_id) {
      module_id %in% names(private$.state)
    },

    #' @description
    #' Check if a specific key exists for a module.
    #'
    #' @param module_id Character string identifying the module.
    #' @param key Character string identifying the state key.
    #' @return Logical indicating whether the key exists.
    has_key = function(module_id, key) {
      if (!self$has_module(module_id)) return(FALSE)
      key %in% names(private$.state[[module_id]])
    },

    #' @description
    #' Remove a state key from a module.
    #'
    #' @param module_id Character string identifying the module.
    #' @param key Character string identifying the state key.
    #' @return Invisibly returns self for method chaining.
    remove = function(module_id, key) {
      if (self$has_key(module_id, key)) {
        private$.state[[module_id]][[key]] <- NULL
        private$.touch()
      }
      invisible(self)
    },

    #' @description
    #' Clear all state for a specific module.
    #'
    #' @param module_id Character string identifying the module.
    #' @return Invisibly returns self for method chaining.
    clear_module = function(module_id) {
      if (self$has_module(module_id)) {
        private$.state[[module_id]] <- list()
        private$.touch()
      }
      invisible(self)
    },

    #' @description
    #' Clear all session state.
    #'
    #' @return Invisibly returns self for method chaining.
    clear_all = function() {
      private$.state <- list()
      private$.touch()
      invisible(self)
    },


    # Project Metadata


    #' @description
    #' Get the project name.
    #'
    #' @return Character string with the project name.
    get_project_name = function() {
      private$.project_name
    },

    #' @description
    #' Set the project name.
    #'
    #' @param name Character string for the project name.
    #' @return Invisibly returns self for method chaining.
    set_project_name = function(name) {
      if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
        stop("Project name must be a non-empty character string")
      }
      private$.project_name <- name
      private$.touch()
      invisible(self)
    },

    #' @description
    #' Get session metadata.
    #'
    #' @return Named list with session metadata.
    get_metadata = function() {
      list(
        project_name = private$.project_name,
        created_at = private$.created_at,
        modified_at = private$.modified_at,
        version = private$.version,
        app_metadata = private$.metadata
      )
    },


    # Serialization (Save/Load)


    #' @description
    #' Serialize the session state to a list suitable for saving.
    #' Only includes keys marked as serializable.
    #'
    #' @return Named list with serializable session data.
    serialize = function() {
      serializable_state <- list()

      for (module_id in names(private$.state)) {
        module_state <- private$.state[[module_id]]
        module_serializable <- private$.serializable[[module_id]]

        if (is.null(module_serializable)) {
          # Default: serialize everything
          serializable_state[[module_id]] <- module_state
        } else {
          serializable_state[[module_id]] <- list()
          for (key in names(module_state)) {
            should_serialize <- if (key %in% names(module_serializable)) {
              module_serializable[[key]]
            } else {
              TRUE  # Default to serializing if not specified
            }
            if (is.null(should_serialize) || isTRUE(should_serialize)) {
              serializable_state[[module_id]][[key]] <- module_state[[key]]
            }
          }
        }
      }

      list(
        iphra_session_version = private$.version,
        project_name = private$.project_name,
        created_at = private$.created_at,
        modified_at = private$.modified_at,
        metadata = private$.metadata,
        state = serializable_state
      )
    },

    #' @description
    #' Deserialize session state from a loaded list.
    #' This replaces the current state with the loaded state.
    #'
    #' @param data Named list with session data (from serialize or load_project).
    #' @param merge Logical. If TRUE, merge with existing state. If FALSE,
    #'   replace existing state completely. Default is FALSE.
    #' @return Invisibly returns self for method chaining.
    deserialize = function(data, merge = FALSE) {
      if (!is.list(data)) {
        stop("Data must be a list")
      }

      # Validate structure
      if (!"iphra_session_version" %in% names(data)) {
        warning("Missing iphra_session_version in loaded data; proceeding with caution")
      }

      # Load metadata
      if (!is.null(data$project_name)) {
        private$.project_name <- data$project_name
      }
      if (!is.null(data$created_at)) {
        private$.created_at <- data$created_at
      }
      if (!is.null(data$metadata)) {
        private$.metadata <- data$metadata
      }

      # Load state
      if (!is.null(data$state)) {
        if (merge) {
          for (module_id in names(data$state)) {
            if (!module_id %in% names(private$.state)) {
              private$.state[[module_id]] <- list()
            }
            for (key in names(data$state[[module_id]])) {
              private$.state[[module_id]][[key]] <- data$state[[module_id]][[key]]
            }
          }
        } else {
          private$.state <- data$state
        }
      }

      private$.touch()
      invisible(self)
    },

    #' @description
    #' Save the session to a file.
    #'
    #' @param path Character string with the file path.
    #'   File extension determines format: .rds for R binary, .json for JSON.
    #' @return Invisibly returns the file path.
    save_project = function(path) {
      if (!is.character(path) || length(path) != 1) {
        stop("Path must be a single character string")
      }

      # Ensure directory exists
      dir_path <- dirname(path)
      if (!dir.exists(dir_path) && dir_path != ".") {
        dir.create(dir_path, recursive = TRUE)
      }

      data <- self$serialize()
      ext <- tolower(tools::file_ext(path))

      if (ext == "json") {
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          stop("Package 'jsonlite' required for JSON export")
        }
        jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE)
      } else {
        # Default to RDS format
        if (!grepl("\\.rds$", tolower(path))) {
          path <- paste0(path, ".rds")
        }
        saveRDS(data, file = path)
      }

      invisible(path)
    },

    #' @description
    #' Load session state from a file.
    #'
    #' @param path Character string with the file path.
    #' @param merge Logical. If TRUE, merge with existing state. Default is FALSE.
    #' @return Invisibly returns self for method chaining.
    load_project = function(path, merge = FALSE) {
      if (!file.exists(path)) {
        stop(paste("File not found:", path))
      }

      ext <- tolower(tools::file_ext(path))

      if (ext == "json") {
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          stop("Package 'jsonlite' required for JSON import")
        }
        data <- jsonlite::read_json(path, simplifyVector = TRUE)
      } else {
        data <- readRDS(path)
      }

      self$deserialize(data, merge = merge)
      invisible(self)
    },

    #' @description
    #' Get a list of all registered modules.
    #'
    #' @return Character vector of module IDs.
    list_modules = function() {
      names(private$.state)
    },

    #' @description
    #' Print a summary of the session state.
    print = function() {
      cat("IPHRA Session State\n")
      cat("───────────────────\n")
      cat("Project:", private$.project_name, "\n")
      cat("Created:", format(private$.created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Modified:", format(private$.modified_at, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Modules:", length(private$.state), "\n")

      if (length(private$.state) > 0) {
        cat("\nRegistered Modules:\n")
        for (module_id in names(private$.state)) {
          n_keys <- length(private$.state[[module_id]])
          cat(sprintf("  • %s (%d keys)\n", module_id, n_keys))
        }
      }

      invisible(self)
    }
  ),

  private = list(
    .project_name = NULL,
    .created_at = NULL,
    .modified_at = NULL,
    .version = NULL,
    .state = NULL,
    .metadata = NULL,
    .serializable = list(),
    .version_signal = NULL,

    # Update modification timestamp and bump the reactive version signal so
    # any Shiny reactive context depending on `get_version_signal()` (e.g.
    # via `phr_reactive_state()`) is invalidated and re-evaluates.
    .touch = function() {
      private$.modified_at <- Sys.time()
      if (is.function(private$.version_signal)) {
        current <- shiny::isolate(private$.version_signal())
        private$.version_signal(current + 1)
      }
    }
  )
)
