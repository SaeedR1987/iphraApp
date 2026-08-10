# ────────────────────────────────────────────────────────────────────────────────
# IPHRA Session State Management
# ────────────────────────────────────────────────────────────────────────────────
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
# ────────────────────────────────────────────────────────────────────────────────

# ────────────────────────────────────────────────────────────────────────────────
# Null-coalescing operator (if not already available)
# ────────────────────────────────────────────────────────────────────────────────

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

# ────────────────────────────────────────────────────────────────────────────────
# R6 Class: IPHRASession
# ────────────────────────────────────────────────────────────────────────────────

#' @title IPHRA Session State Object
#' @description
#' R6 class for managing session state across multiple modules.
#' Provides methods for state storage, retrieval, serialization (save),
#' and deserialization (load) of project files.
#'
#' @export
IPHRASession <- R6::R6Class(
  "IPHRASession",
  public = list(
    #' @description
    #' Initialize a new IPHRASession object.
    #'
    #' @param project_name Optional project name for the session.
    #' @return A new IPHRASession object.
    initialize = function(project_name = NULL) {
      private$.project_name <- project_name %||% paste0("IPHRA_Project_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      private$.created_at <- Sys.time()
      private$.modified_at <- Sys.time()
      private$.version <- "1.0.0"
      private$.state <- list()
      private$.metadata <- list(
        app_version = tryCatch(
          as.character(utils::packageVersion("iphRa")),
          error = function(e) "0.0.0.9000"
        ),
        r_version = R.version.string
      )
      invisible(self)
    },

    # ────────────────────────────────────────────────────
    # Module Registration
    # ────────────────────────────────────────────────────

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

    # ────────────────────────────────────────────────────
    # State Getters and Setters
    # ────────────────────────────────────────────────────

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

    # ────────────────────────────────────────────────────
    # Project Metadata
    # ────────────────────────────────────────────────────

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

    # ────────────────────────────────────────────────────
    # Serialization (Save/Load)
    # ────────────────────────────────────────────────────

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

    # Update modification timestamp
    .touch = function() {
      private$.modified_at <- Sys.time()
    }
  )
)


# ────────────────────────────────────────────────────────────────────────────────
# Session Access Functions
# ────────────────────────────────────────────────────────────────────────────────

#' @title Initialize IPHRA Session
#' @description
#' Initialize the complete IPHRA session structure in `session$userData`.
#' This function should be called at the start of the app server to ensure
#' all session components are available to modules.
#'
#' The session structure includes:
#' - `session_id`: Unique session identifier
#' - `session_start`: Session start timestamp
#' - `iphra_session`: IPHRASession R6 object for serializable project state
#' - `settings`: reactiveValues for user preferences (language, theme, etc.)
#' - `project`: reactiveValues for project-level data pointers
#' - `modules`: List for module-specific R6 or reactive objects
#' - `ui`: reactiveValues for UI/interaction state
#' - `flags`: reactiveValues for runtime status flags
#' - `temp`: reactiveValues for temporary/cached data
#' - `checkbox_status`: reactiveValues for tracking completion checkboxes
#'
#' @param session Shiny session object.
#' @param project_name Optional project name for new sessions.
#' @return The IPHRASession object (for backward compatibility).
#' @export
iphra_init_session <- function(session = shiny::getDefaultReactiveDomain(),
                                project_name = NULL) {
  if (is.null(session)) {
    stop("[IPHRA::Error] No active Shiny session found; cannot initialize session state.")
  }

  # Only initialize if not already done

  if (is.null(session$userData$session_id)) {
    iphra_message("Initializing IPHRA session...")

    #----------------------------------------------------------
    # 1. Session metadata
    #----------------------------------------------------------
    session$userData$session_id <- paste0("iphra_", as.integer(Sys.time()))
    session$userData$session_start <- Sys.time()

    #----------------------------------------------------------
    # 2. IPHRASession R6 object (serializable project state)
    #----------------------------------------------------------
    session$userData$iphra_session <- IPHRASession$new(project_name = project_name)

    #----------------------------------------------------------
    # 3. Global settings and user preferences (transient)
    #----------------------------------------------------------
    session$userData$settings <- shiny::reactiveValues(
      language = "en",
      theme = "light",
      auto_save = TRUE,
      auto_validate = TRUE,
      developer_mode = FALSE,
      livelihoods_mapping = iphra_default_livelihoods_mapping()
    )

    #----------------------------------------------------------
    # 4. Project-level reactive pointers (for UI binding)
    #----------------------------------------------------------
    # These point to data in the serializable state or hold
    # runtime references. Actual persistent data goes in iphra_session.
    session$userData$project <- shiny::reactiveValues(
      name = NULL,
      description = NULL,
      path = NULL,
      data = NULL,           # raw or imported data
      cleaned = NULL,        # cleaned dataset
      metadata = NULL,       # variable definitions, types, etc.
      indicators = NULL,     # derived indicators / analysis parameters
      summary = NULL,        # summary tables or results
      params = list(),       # reproducible parameters (sample seed, sizes, etc.)
      last_saved = NULL
    )

    #----------------------------------------------------------
    # 5. Module-level runtime objects (transient)
    #----------------------------------------------------------
    # Store module-specific R6 objects or state here.
    # These are reinitialized on startup or after loading a project.
    session$userData$modules <- list()

    #----------------------------------------------------------
    # 6. UI / Interaction state (transient)
    #----------------------------------------------------------
    session$userData$ui <- shiny::reactiveValues(
      active_tab = "home",
      last_action = NULL,
      notifications = list()
    )

    #----------------------------------------------------------
    # 7. Runtime flags and status (transient)
    #----------------------------------------------------------
    session$userData$flags <- shiny::reactiveValues(
      is_busy = FALSE,
      has_unsaved_changes = FALSE,
      validation_required = FALSE,
      load_in_progress = FALSE
    )

    #----------------------------------------------------------
    # 8. Temporary objects and caches (never serialized)
    #----------------------------------------------------------
    session$userData$temp <- shiny::reactiveValues(
      cache = list(),
      upload_progress = NULL,
      preview_data = NULL
    )

    #----------------------------------------------------------
    # 9. Checkbox Status for step completion tracking
    #----------------------------------------------------------
    # This structure tracks the completion status of various
    # steps across different modules. The _master modules will
    # read from these values to display status tables.
    session$userData$checkbox_status <- shiny::reactiveValues(
      # Data Import/Processing Status
      data = shiny::reactiveValues(
        household_data_imported = FALSE,
        roster_data_imported = FALSE,
        mortality_data_imported = FALSE,
        nutrition_data_imported = FALSE,
        woman_data_imported = FALSE,
        health_data_imported = FALSE,
        water_data_imported = FALSE,
        fsl_kii_data_imported = FALSE,
        health_kii_data_imported = FALSE,
        nut_kii_data_imported = FALSE,
        wash_kii_data_imported = FALSE,
        community_kii_data_imported = FALSE,
        community_obs_data_imported = FALSE,
        healthfacility_obs_data_imported = FALSE,
        nutfacility_obs_data_imported = FALSE,
        water_obs_data_imported = FALSE,
        latrine_obs_data_imported = FALSE,
        livelihoods_obs_data_imported = FALSE
      ),
      # Data Cleaning Status
      cleaning = shiny::reactiveValues(
        main_cleaning_complete = FALSE,
        roster_cleaning_complete = FALSE,
        mortality_cleaning_complete = FALSE,
        nutrition_cleaning_complete = FALSE,
        woman_cleaning_complete = FALSE,
        health_cleaning_complete = FALSE,
        water_cleaning_complete = FALSE,
        fsl_kii_cleaning_complete = FALSE,
        health_kii_cleaning_complete = FALSE,
        nut_kii_cleaning_complete = FALSE,
        wash_kii_cleaning_complete = FALSE,
        community_kii_cleaning_complete = FALSE,
        community_obs_cleaning_complete = FALSE,
        healthfacility_obs_cleaning_complete = FALSE,
        nutfacility_obs_cleaning_complete = FALSE,
        water_obs_cleaning_complete = FALSE,
        latrine_obs_cleaning_complete = FALSE,
        livelihoods_obs_cleaning_complete = FALSE,
        graves_obs_cleaning_complete = FALSE
      ),
      # Data Quality Status
      quality = shiny::reactiveValues(
        general_quality_complete = FALSE,
        nutrition_quality_complete = FALSE,
        muac_quality_complete = FALSE,
        mortality_quality_complete = FALSE,
        health_quality_complete = FALSE,
        fsl_quality_complete = FALSE,
        wash_quality_complete = FALSE,
        other_quality_complete = FALSE
      ),
      # Analysis Status
      analysis = shiny::reactiveValues(
        demographics_analysis_complete = FALSE,
        nutrition_analysis_complete = FALSE,
        muac_analysis_complete = FALSE,
        mortality_analysis_complete = FALSE,
        health_analysis_complete = FALSE,
        fsl_analysis_complete = FALSE,
        wash_analysis_complete = FALSE,
        other_analysis_complete = FALSE,
        summary_analysis_complete = FALSE
      ),
      # Tools/DAP Status
      tools = shiny::reactiveValues(
        household_tools_complete = FALSE,
        community_tools_complete = FALSE,
        fsl_tools_complete = FALSE,
        health_tools_complete = FALSE,
        wash_tools_complete = FALSE
      )
    )

    #----------------------------------------------------------
    # 10. IPHRA Protocol tool tracking
    #----------------------------------------------------------
    # Reactive vector of tool names currently added to the
    # IPHRAProtocol instance (session$userData$modules$protocol).
    # Add/remove buttons in mod_tools_master update this and the
    # underlying protocol; mod_tools_* modules subscribe to it to
    # dynamically show/hide their sub-tool sections.
    session$userData$protocol_tools <- shiny::reactiveVal(character(0))

    # Reactive integer bumped whenever the master_indicator_bank
    # is modified via `iphra_modify_indicator_bank`, so downstream
    # UIs can react to indicator bank changes.
    session$userData$indicator_bank_version <- shiny::reactiveVal(0L)

    iphra_message("Session initialized successfully.")
  }

  session$userData$iphra_session
}

#' @title Get IPHRA Session
#' @description
#' Retrieve the IPHRASession R6 object from the current Shiny session.
#' If the session has not been initialized, this will initialize it.
#'
#' @param session Shiny session object.
#' @return The IPHRASession object.
#' @export
iphra_get_session <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("[IPHRA::Error] No active Shiny session found; cannot access session state.")
  }

  if (is.null(session$userData$iphra_session)) {
    iphra_init_session(session)
  }

  session$userData$iphra_session
}


# ────────────────────────────────────────────────────────────────────────────────
# Checkbox Status Access Functions
# ────────────────────────────────────────────────────────────────────────────────

#' @title Get Checkbox Status
#' @description
#' Get the checkbox status reactiveValues object for tracking step completion.
#' Contains nested structures for: data, cleaning, quality, analysis, tools.
#'
#' @param session Shiny session object.
#' @return The checkbox_status reactiveValues object.
#' @export
iphra_get_checkbox_status <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$checkbox_status)) {
    iphra_init_session(session)
  }
  session$userData$checkbox_status
}

#' @title Set Checkbox Status
#' @description
#' Set the status of a specific checkbox in the session state.
#'
#' @param category Character string: "data", "cleaning", "quality", "analysis", or "tools".
#' @param key Character string identifying the specific checkbox.
#' @param value Logical value indicating the checkbox status.
#' @param session Shiny session object.
#' @return Invisibly returns the new value.
#' @export
iphra_set_checkbox_status <- function(category, key, value,
                                       session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  if (!is.null(checkbox_status[[category]])) {
    checkbox_status[[category]][[key]] <- value
  }
  invisible(value)
}

#' @title Get Single Checkbox Status
#' @description
#' Get the status of a specific checkbox from the session state.
#'
#' @param category Character string: "data", "cleaning", "quality", "analysis", or "tools".
#' @param key Character string identifying the specific checkbox.
#' @param session Shiny session object.
#' @return Logical value indicating the checkbox status, or FALSE if not found.
#' @export
iphra_get_single_checkbox_status <- function(category, key,
                                              session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  if (!is.null(checkbox_status[[category]]) && !is.null(checkbox_status[[category]][[key]])) {
    return(checkbox_status[[category]][[key]])
  }
  FALSE
}

#' @title Get Status Icon
#' @description
#' Returns an HTML icon element representing the status (checkmark or X).
#'
#' @param status Logical value indicating the status.
#' @return HTML string with appropriate icon.
#' @export
iphra_status_icon <- function(status) {
  if (isTRUE(status)) {
    '<span style="color: green; font-weight: bold;">&#10004;</span>'  # Checkmark

} else {
    '<span style="color: red; font-weight: bold;">&#10008;</span>'    # X mark
  }
}

#' @title Build Status Table Data
#' @description
#' Creates a data frame suitable for rendering a status table in a master module.
#'
#' @param status_list A named list of logical values representing status.
#' @param labels Optional named list of display labels for each key.
#' @return A data frame with Step, Status, and Icon columns.
#' @export
iphra_build_status_table <- function(status_list, labels = NULL) {
  if (length(status_list) == 0) {
    return(data.frame(Step = character(0), Status = character(0), stringsAsFactors = FALSE))
  }

  keys <- names(status_list)
  display_labels <- if (!is.null(labels)) {
    sapply(keys, function(k) if (k %in% names(labels)) labels[[k]] else k)
  } else {
    # Convert key names to readable labels
    sapply(keys, function(k) {
      gsub("_", " ", gsub("_(complete|imported)$", "", k))
    })
  }

  statuses <- sapply(status_list, function(x) if (isTRUE(x)) "Complete" else "Pending")
  icons <- sapply(status_list, iphra_status_icon)

  data.frame(
    Step = display_labels,
    Status = statuses,
    Icon = icons,
    stringsAsFactors = FALSE
  )
}


# ────────────────────────────────────────────────────────────────────────────────
# Convenience Functions for Module Integration
# ────────────────────────────────────────────────────────────────────────────────

#' @title Create Reactive Session State Accessor
#' @description
#' Creates a reactive accessor for a specific module's state.
#' This is useful for creating reactive dependencies on session state.
#'
#' @param module_id Character string identifying the module.
#' @param key Character string identifying the state key.
#' @param session Shiny session object.
#' @return A reactive expression that returns the current state value.
#' @export
iphra_reactive_state <- function(module_id, key,
                                  session = shiny::getDefaultReactiveDomain()) {
  shiny::reactive({
    iphra_session <- iphra_get_session(session)
    iphra_session$get(module_id, key)
  })
}

#' @title Session State Observer
#' @description
#' Creates an observer that automatically syncs a reactive value to session state.
#' Use this to automatically persist reactive values to the session.
#'
#' @param module_id Character string identifying the module.
#' @param key Character string identifying the state key.
#' @param reactive_expr Reactive expression to observe.
#' @param session Shiny session object.
#' @return Invisibly returns the observer handle.
#' @export
iphra_sync_state <- function(module_id, key, reactive_expr,
                              session = shiny::getDefaultReactiveDomain()) {
  shiny::observe({
    value <- reactive_expr()
    iphra_session <- iphra_get_session(session)
    iphra_session$set(module_id, key, value)
  })
}


# ────────────────────────────────────────────────────────────────────────────────
# Session Structure Accessors
# ────────────────────────────────────────────────────────────────────────────────
# These helper functions provide convenient access to different parts of the
# session$userData structure.

#' @title Get Session Settings
#' @description
#' Get the settings reactiveValues object for user preferences.
#' Settings include: language, theme, auto_save, auto_validate, developer_mode.
#'
#' @param session Shiny session object.
#' @return The settings reactiveValues object.
#' @export
#' @examples
#' \dontrun{
#' settings <- iphra_get_settings(session)
#' settings$language <- "fr"
#' current_lang <- settings$language
#' }
iphra_get_settings <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$settings)) {
    iphra_init_session(session)
  }
  session$userData$settings
}

#' @title Get Project Reactive Data
#' @description
#' Get the project reactiveValues object for project-level data pointers.
#' Project includes: name, description, path, data, cleaned, metadata,
#' indicators, summary, params, last_saved.
#'
#' @param session Shiny session object.
#' @return The project reactiveValues object.
#' @export
#' @examples
#' \dontrun{
#' project <- iphra_get_project(session)
#' project$data <- imported_data
#' project$cleaned <- cleaned_data
#' }
iphra_get_project <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$project)) {
    iphra_init_session(session)
  }
  session$userData$project
}

#' @title Get Module Objects
#' @description
#' Get the modules list for storing module-specific R6 or reactive objects.
#' Use this to register and retrieve module instances.
#'
#' @param session Shiny session object.
#' @return The modules list.
#' @export
#' @examples
#' \dontrun{
#' modules <- iphra_get_modules(session)
#' modules$sample <- SampleModule$new()
#' sample_mod <- modules$sample
#' }
iphra_get_modules <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$modules)) {
    iphra_init_session(session)
  }
  session$userData$modules
}

#' @title Set Module Object
#' @description
#' Register a module-specific object (R6, reactive, etc.) in the session.
#'
#' @param module_name Character string identifying the module.
#' @param module_object The module object to store.
#' @param session Shiny session object.
#' @return Invisibly returns the module object.
#' @export
iphra_set_module <- function(module_name, module_object,
                              session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$modules)) {
    iphra_init_session(session)
  }
  session$userData$modules[[module_name]] <- module_object
  invisible(module_object)
}

#' @title Get UI State
#' @description
#' Get the UI reactiveValues object for interaction state.
#' UI state includes: active_tab, last_action, notifications.
#'
#' @param session Shiny session object.
#' @return The ui reactiveValues object.
#' @export
iphra_get_ui <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$ui)) {
    iphra_init_session(session)
  }
  session$userData$ui
}

#' @title Get Runtime Flags
#' @description
#' Get the flags reactiveValues object for runtime status.
#' Flags include: is_busy, has_unsaved_changes, validation_required, load_in_progress.
#'
#' @param session Shiny session object.
#' @return The flags reactiveValues object.
#' @export
iphra_get_flags <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$flags)) {
    iphra_init_session(session)
  }
  session$userData$flags
}

#' @title Get Temporary Cache
#' @description
#' Get the temp reactiveValues object for temporary/cached data.
#' Temp includes: cache, upload_progress, preview_data.
#'
#' @param session Shiny session object.
#' @return The temp reactiveValues object.
#' @export
iphra_get_temp <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$temp)) {
    iphra_init_session(session)
  }
  session$userData$temp
}

#' @title Mark Session as Having Unsaved Changes
#' @description
#' Convenience function to mark that the session has unsaved changes.
#'
#' @param has_changes Logical indicating whether there are unsaved changes.
#' @param session Shiny session object.
#' @return Invisibly returns the new value.
#' @export
iphra_set_unsaved_changes <- function(has_changes = TRUE,
                                       session = shiny::getDefaultReactiveDomain()) {
  flags <- iphra_get_flags(session)
  flags$has_unsaved_changes <- has_changes
  invisible(has_changes)
}

#' @title Check if Session Has Unsaved Changes
#' @description
#' Check whether the current session has unsaved changes.
#'
#' @param session Shiny session object.
#' @return Logical indicating whether there are unsaved changes.
#' @export
iphra_has_unsaved_changes <- function(session = shiny::getDefaultReactiveDomain()) {
  flags <- iphra_get_flags(session)
  isTRUE(flags$has_unsaved_changes)
}


# ────────────────────────────────────────────────────────────────────────────────
# Livelihoods Mapping Functions
# ────────────────────────────────────────────────────────────────────────────────

#' @title Get Default Livelihoods Mapping
#' @description
#' Returns a data frame with the default mapping of LCSI coping strategies
#' to categories. Used for initialization and reset operations.
#'
#' @return A data frame with columns: strategy (LCSI variable names) and
#'   category (mapping categories: livestock, agriculture, child-related,
#'   asset-selling, none)
#' @export
iphra_default_livelihoods_mapping <- function() {
  data.frame(
    strategy = c(
      "fsl_lcsi_stress1",
      "fsl_lcsi_stress2",
      "fsl_lcsi_stress3",
      "fsl_lcsi_stress4",
      "fsl_lcsi_crisis1",
      "fsl_lcsi_crisis2",
      "fsl_lcsi_crisis3",
      "fsl_lcsi_emergency1",
      "fsl_lcsi_emergency2",
      "fsl_lcsi_emergency3"
    ),
    category = rep("none", 10),
    stringsAsFactors = FALSE
  )
}

#' @title Get Livelihoods Category Choices
#' @description
#' Returns the valid category choices for livelihoods mapping.
#'
#' @return A character vector of valid category names.
#' @export
iphra_livelihoods_category_choices <- function() {
  c("livestock", "agriculture", "child-related", "asset-selling", "none")
}

#' @title Get Livelihoods Mapping
#' @description
#' Get the current livelihoods mapping from session settings.
#'
#' @param session Shiny session object.
#' @return A data frame with the current livelihoods mapping.
#' @export
iphra_get_livelihoods_mapping <- function(session = shiny::getDefaultReactiveDomain()) {
  settings <- iphra_get_settings(session)
  if (is.null(settings$livelihoods_mapping)) {
    return(iphra_default_livelihoods_mapping())
  }
  settings$livelihoods_mapping
}

#' @title Set Livelihoods Mapping
#' @description
#' Set the livelihoods mapping in session settings.
#'
#' @param mapping A data frame with columns: strategy and category.
#' @param session Shiny session object.
#' @return Invisibly returns the mapping.
#' @export
iphra_set_livelihoods_mapping <- function(mapping,
                                           session = shiny::getDefaultReactiveDomain()) {
  settings <- iphra_get_settings(session)
  settings$livelihoods_mapping <- mapping
  iphra_set_unsaved_changes(TRUE, session)
  invisible(mapping)
}

#' @title Reset Livelihoods Mapping to Default
#' @description
#' Reset the livelihoods mapping to the default values.
#'
#' @param session Shiny session object.
#' @return The default livelihoods mapping data frame.
#' @export
iphra_reset_livelihoods_mapping <- function(session = shiny::getDefaultReactiveDomain()) {
  default_mapping <- iphra_default_livelihoods_mapping()
  iphra_set_livelihoods_mapping(default_mapping, session)
  default_mapping
}


# ────────────────────────────────────────────────────────────────────────────────
# IPHRAProtocol / Tool Accessors
# ────────────────────────────────────────────────────────────────────────────────

#' @title Get IPHRAProtocol Instance
#' @description Retrieve the phr::IPHRAProtocol object stored in
#' `session$userData$modules$protocol` (set in `app_server.R`).
#'
#' @param session Shiny session object.
#' @return The IPHRAProtocol R6 object, or NULL if not initialised.
#' @export
iphra_get_protocol <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session) || is.null(session$userData$modules)) return(NULL)
  session$userData$modules$protocol
}

#' @title Get Reactive Protocol Tools Vector
#' @description Return the reactiveVal holding the character vector of
#' tool names currently added to the protocol.
#'
#' @param session Shiny session object.
#' @return A `shiny::reactiveVal`.
#' @export
iphra_get_protocol_tools <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session$userData$protocol_tools)) {
    iphra_init_session(session)
  }
  session$userData$protocol_tools
}

#' @title Is Protocol Tool Present?
#' @description Reactive-safe check whether `tool_name` has been added
#' to the current protocol.
#'
#' @param tool_name Character. Name of the tool (see
#'   `protocol$get_allowable_tools()`).
#' @param session Shiny session object.
#' @return Logical scalar.
#' @export
iphra_has_protocol_tool <- function(tool_name,
                                    session = shiny::getDefaultReactiveDomain()) {
  tools_rv <- iphra_get_protocol_tools(session)
  tool_name %in% tools_rv()
}

#' @title Add a Tool to the Protocol
#' @description Adds `tool_name` to the underlying protocol via
#' `protocol$add_tools(name = tool_name)` and updates the reactive
#' tools vector. No-op if the tool is already present.
#'
#' @param tool_name Character. Name of the tool.
#' @param session Shiny session object.
#' @return Invisibly TRUE if added, FALSE otherwise.
#' @export
iphra_add_protocol_tool <- function(tool_name,
                                    session = shiny::getDefaultReactiveDomain()) {
  tools_rv <- iphra_get_protocol_tools(session)
  current  <- tools_rv()
  if (tool_name %in% current) {
    iphra_message(
      paste0("Tool '", tool_name, "' is already present in the protocol."),
      origin = "Protocol: Add Tool"
    )
    return(invisible(FALSE))
  }

  protocol <- iphra_get_protocol(session)
  if (is.null(protocol)) {
    iphra_warning(
      "No IPHRAProtocol found in session; cannot add tool.",
      origin = "Protocol: Add Tool"
    )
    return(invisible(FALSE))
  }

  ok <- tryCatch({
    protocol$add_tools(name = tool_name)
    TRUE
  }, error = function(e) {
    iphra_warning(
      paste0("Failed to add tool '", tool_name, "': ", conditionMessage(e)),
      origin = "Protocol: Add Tool"
    )
    FALSE
  })

  if (isTRUE(ok)) {
    tools_rv(unique(c(current, tool_name)))
    iphra_message(
      paste0("Tool '", tool_name, "' added to protocol."),
      origin = "Protocol: Add Tool"
    )
  }
  invisible(isTRUE(ok))
}

#' @title Remove a Tool from the Protocol
#' @description Removes `tool_name` from the underlying protocol via
#' `protocol$remove_tools(name = tool_name)` and updates the reactive
#' tools vector.
#'
#' @param tool_name Character. Name of the tool.
#' @param session Shiny session object.
#' @return Invisibly TRUE if removed, FALSE otherwise.
#' @export
iphra_remove_protocol_tool <- function(tool_name,
                                       session = shiny::getDefaultReactiveDomain()) {
  tools_rv <- iphra_get_protocol_tools(session)
  current  <- tools_rv()
  if (!tool_name %in% current) {
    iphra_message(
      paste0("Tool '", tool_name, "' is not present in the protocol."),
      origin = "Protocol: Remove Tool"
    )
    return(invisible(FALSE))
  }

  protocol <- iphra_get_protocol(session)
  if (!is.null(protocol)) {
    ok <- tryCatch({
      protocol$remove_tools(name = tool_name)
      TRUE
    }, error = function(e) {
      iphra_warning(
        paste0("Failed to remove tool '", tool_name, "' on protocol: ",
               conditionMessage(e)),
        origin = "Protocol: Remove Tool"
      )
      FALSE
    })
    if (!isTRUE(ok)) return(invisible(FALSE))
  }

  tools_rv(setdiff(current, tool_name))
  iphra_message(
    paste0("Tool '", tool_name, "' removed from protocol."),
    origin = "Protocol: Remove Tool"
  )
  invisible(TRUE)
}

#' @title Get Master Indicator Bank
#' @description Return the `master_indicator_bank` data.frame from the
#' framework nested in the current protocol. Returns an empty data
#' frame with the expected columns when the protocol / framework is not
#' available, so downstream UI code can safely operate on it.
#'
#' @param session Shiny session object.
#' @return A data.frame with at least `indicator_code` and
#'   `indicator_name` columns.
#' @export
iphra_get_indicator_bank <- function(session = shiny::getDefaultReactiveDomain()) {
  empty <- data.frame(
    indicator_code = character(0),
    indicator_name = character(0),
    stringsAsFactors = FALSE
  )
  protocol <- iphra_get_protocol(session)
  if (is.null(protocol)) return(empty)

  bank <- tryCatch(protocol$framework$master_indicator_bank,
                   error = function(e) NULL)
  if (is.null(bank) || !is.data.frame(bank) || nrow(bank) == 0) return(empty)

  # Ensure required columns exist
  if (!all(c("indicator_code", "indicator_name") %in% names(bank))) return(empty)
  bank
}

#' @title Modify the Framework's Indicator Bank
#' @description Convenience wrapper around
#' `protocol$framework$modify_indicator_bank(indicator_codes)`. Bumps
#' the `indicator_bank_version` reactive so listeners can react.
#'
#' @param indicator_codes Character vector of indicator codes.
#' @param session Shiny session object.
#' @return Invisibly TRUE on success.
#' @export
iphra_modify_indicator_bank <- function(indicator_codes,
                                         session = shiny::getDefaultReactiveDomain()) {
  protocol <- iphra_get_protocol(session)
  if (is.null(protocol)) return(invisible(FALSE))

  ok <- tryCatch({
    protocol$framework$modify_indicator_bank(indicator_codes)
    TRUE
  }, error = function(e) {
    iphra_warning(
      paste0("modify_indicator_bank failed: ", conditionMessage(e)),
      origin = "Protocol: modify_indicator_bank"
    )
    FALSE
  })

  if (isTRUE(ok) && !is.null(session$userData$indicator_bank_version)) {
    v <- session$userData$indicator_bank_version()
    session$userData$indicator_bank_version(v + 1L)
  }
  invisible(isTRUE(ok))
}

#' @title Filter a Tool's Survey by Indicator Codes
#' @description Wrapper around
#' `protocol$tools[[tool_name]]$filter_survey_by_indicator(indicator_codes = ...)`.
#'
#' @param tool_name Character. Name of the tool.
#' @param indicator_codes Character vector of indicator codes.
#' @param session Shiny session object.
#' @return Invisibly TRUE on success, FALSE otherwise.
#' @export
iphra_filter_tool_survey <- function(tool_name, indicator_codes,
                                      session = shiny::getDefaultReactiveDomain()) {
  protocol <- iphra_get_protocol(session)
  if (is.null(protocol)) return(invisible(FALSE))

  tool_obj <- tryCatch(protocol$tools[[tool_name]], error = function(e) NULL)
  if (is.null(tool_obj)) {
    iphra_message(
      paste0("Tool '", tool_name, "' not present in protocol; skipping filter."),
      origin = "Protocol: filter_survey_by_indicator"
    )
    return(invisible(FALSE))
  }

  ok <- tryCatch({
    tool_obj$filter_survey_by_indicator(indicator_codes = indicator_codes)
    TRUE
  }, error = function(e) {
    iphra_warning(
      paste0("filter_survey_by_indicator failed for '", tool_name, "': ",
             conditionMessage(e)),
      origin = "Protocol: filter_survey_by_indicator"
    )
    FALSE
  })
  invisible(isTRUE(ok))
}

#' @title Map Indicator Display Names to Codes
#' @description Given a character vector of display names (as returned
#' by the sortable rank lists), look up the corresponding
#' `indicator_code` values in the master indicator bank. Names with no
#' match are dropped.
#'
#' @param names_selected Character vector of `indicator_name` values.
#' @param session Shiny session object.
#' @return Character vector of `indicator_code` values.
#' @export
iphra_indicator_names_to_codes <- function(names_selected,
                                            session = shiny::getDefaultReactiveDomain()) {
  if (length(names_selected) == 0) return(character(0))
  bank <- iphra_get_indicator_bank(session)
  if (nrow(bank) == 0) return(character(0))
  codes <- bank$indicator_code[match(names_selected, bank$indicator_name)]
  codes[!is.na(codes)]
}
