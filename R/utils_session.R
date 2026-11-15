
# Session Access Functions


#' @title Initialize Session
#' @description
#' Initialize the standard session structure in `session$userData`.
#' This function should be called at the start of the app server to ensure
#' all session components are available to modules.
#'
#' The session structure includes:
#' - `session_id`: Unique session identifier
#' - `session_start`: Session start timestamp
#' - `phr_session`: PHRSession R6 object (module registry + project state)
#' - `settings`: reactiveValues for user preferences (language, theme, etc.)
#' - `project`: reactiveValues for project-level data pointers
#' - `ui`: reactiveValues for UI/interaction state
#' - `flags`: reactiveValues for runtime status flags
#' - `temp`: reactiveValues for temporary/cached data
#'
#' @param session Shiny session object.
#' @param project_name Optional project name for new sessions.
#' @return The PHRSession object (for backward compatibility).
#' @export
init_session <- function(session = shiny::getDefaultReactiveDomain(),
                                project_name = NULL) {
  if (is.null(session)) {
    stop("[IPHRA::Error] No active Shiny session found; cannot initialize session state.")
  }

  # Only initialize if not already done

  if (is.null(session$userData$session_id)) {
    iphra_message("Initializing IPHRA session...")


    # 1. Session metadata

    session$userData$session_id <- paste0(project_name, "_", as.integer(Sys.time()))
    session$userData$session_start <- Sys.time()


    # 2. IPHRASession R6 object (serializable project state)

    session$userData$phr_session <- PHRSession$new(project_name = project_name)


    # 3. Global settings and user preferences (transient)

    session$userData$settings <- shiny::reactiveValues(
      language = "en",
      theme = "light",
      auto_save = TRUE,
      auto_validate = TRUE,
      developer_mode = FALSE,
      livelihoods_mapping = iphra_default_livelihoods_mapping()
    )


    # 4. Project-level reactive pointers (for UI binding)

    # These point to data in the serializable state or hold
    # runtime references. Actual persistent data goes in phr_session.
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


    # 5. Module objects live inside the PHRSession (see `set_module()`),
    # together with their reactive signals.

    # Backwards-compatible alias: several mod_tools_* modules already read
    # `session$userData$indicator_bank_version()` to know when the protocol's
    # indicator bank has changed. Point it at the "protocol" module signal so
    # it stays in sync whenever `phr_touch_module("protocol", session)` is called.
    session$userData$indicator_bank_version <-
      session$userData$phr_session$get_module_signal("protocol")


    # 6. UI / Interaction state (transient)

    session$userData$ui <- shiny::reactiveValues(
      active_tab = "home",
      last_action = NULL,
      notifications = list()
    )


    # 7. Runtime flags and status (transient)

    session$userData$flags <- shiny::reactiveValues(
      is_busy = FALSE,
      has_unsaved_changes = FALSE,
      validation_required = FALSE,
      load_in_progress = FALSE,
      # Incremented each time a project file is successfully loaded.
      # Module observers can use `observeEvent(session$userData$flags$project_loaded, ...)`
      # with `ignoreInit = TRUE` to restore UI state after a load.
      project_loaded = 0L
    )


    # 8. Temporary objects and caches (never serialized)

    session$userData$temp <- shiny::reactiveValues(
      cache = list(),
      upload_progress = NULL,
      preview_data = NULL
    )

    iphra_message("Session initialized successfully.")
  }

  session$userData$phr_session
}

#' @title Get Serializable Session
#' @description
#' Retrieve the PHRSession R6 object from the current Shiny session.
#' If the session has not been initialized, this will initialize it.
#'
#' @param session Shiny session object.
#' @return The PHRSession object.
#' @export
get_session <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("[IPHRA::Error] No active Shiny session found; cannot access session state.")
  }

  if (is.null(session$userData$phr_session)) {
    init_session(session)
  }

  session$userData$phr_session
}

# Convenience Functions for Module Integration ####



# Session Structure Accessors

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
    init_session(session)
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
    init_session(session)
  }
  session$userData$project
}

#' @title Get Module Objects
#' @description
#' Get the named list of module objects registered in the PHRSession.
#' This is a non-reactive snapshot; use `phr_get_module_reactive()` for
#' reactive access and `set_module()` to register modules.
#'
#' @param session Shiny session object.
#' @return A named list of module objects.
#' @export
#' @examples
#' \dontrun{
#' modules <- phr_get_modules(session)
#' protocol <- modules[["protocol"]]
#' }
phr_get_modules <- function(session = shiny::getDefaultReactiveDomain()) {
  get_session(session)$get(field = "..modules")
}

#' @title Set Module Object
#' @description
#' Register a module-specific object (R6, etc.) in the session's PHRSession.
#' R6 objects are plain mutable environments and are not reactive on their
#' own, so registering (or replacing) a module also bumps its reactive signal
#' (see `phr_touch_module()` and `phr_get_module_reactive()`), refreshing any
#' consumer reading the module reactively.
#'
#' @param module_name Character string identifying the module.
#' @param module_object The module object to store.
#' @param session Shiny session object.
#' @return Invisibly returns the module object.
#' @export
set_module <- function(module_name, module_object,
                       session = shiny::getDefaultReactiveDomain()) {
  get_session(session)$set_module(module_name, module_object)
}

#' @title Get a Module's Reactive Signal
#' @description
#' Internal helper returning the `shiny::reactiveVal()` backing a module's
#' reactive signal, without wrapping it in a `shiny::reactive()`. Shared by
#' `phr_get_module_reactive()`, `iphra_has_protocol_tool()`, and
#' `iphra_get_indicator_bank()`.
#'
#' @param module_name Character string identifying the module.
#' @param session Shiny session object.
#' @return The `shiny::reactiveVal()` function for this module.
#' @keywords internal
.iphra_module_version <- function(module_name, session) {
  get_session(session)$get_module_signal(module_name)
}

#' @title Touch (Notify) a Module's Reactive Signal
#' @description
#' Notify dependents that a module registered in the PHRSession changed.
#' Call this immediately after mutating an R6 module object's state (e.g.
#' `protocol$add_tools(...)`) so that any reactive created via
#' `phr_get_module_reactive()` is invalidated and re-evaluates.
#'
#' @param module_name Character string identifying the module.
#' @param session Shiny session object.
#' @return Invisibly returns the new session version number.
#' @export
#' @examples
#' \dontrun{
#' protocol <- phr_get_modules(session)[["protocol"]]
#' protocol$add_tools("tool_household_iphra_v2")
#' phr_touch_module("protocol", session)
#' }
phr_touch_module <- function(module_name, session = shiny::getDefaultReactiveDomain()) {
  get_session(session)$touch_module(module_name)
}

#' @title Get a Reactive Accessor for a Module
#' @description
#' Returns a `shiny::reactive()` expression that depends on the module's
#' version counter (bumped by `set_module()` / `phr_touch_module()`)
#' and yields the current module object. Use this instead of reading
#' `phr_get_modules(session)[[module_name]]` directly whenever the value
#' needs to stay current inside a `reactive()`, `observe()`, or `render*()`
#' block.
#'
#' @param module_name Character string identifying the module.
#' @param session Shiny session object.
#' @return A reactive expression returning the current module object.
#' @export
#' @examples
#' \dontrun{
#' protocol_r <- phr_get_module_reactive("protocol", session)
#' output$tool_present <- shiny::renderText({
#'   if ("tool_household_iphra_v2" %in% names(protocol_r()$tools)) "true" else "false"
#' })
#' }
phr_get_module_reactive <- function(module_name, session = shiny::getDefaultReactiveDomain()) {
  get_session(session)$get_module_reactive(module_name)
}

#' @title Check if the Protocol Has a Given Tool
#' @description
#' Reactively checks whether a given tool is currently present on the
#' `IPHRAProtocol` object registered as the `"protocol"` module. Depends on
#' the protocol module's version signal, so it correctly re-evaluates inside
#' a `reactive()` / `render*()` block whenever tools are added or removed via
#' `protocol$add_tools()` / `protocol$remove_tools()` (followed by
#' `phr_touch_module("protocol", session)`).
#'
#' Must be called from within an active reactive context (e.g. inside
#' `shiny::reactive()`, `shiny::observe()`, `render*()`, or `shiny::isolate()`)
#' for the dependency to be established; the version counter is read
#' directly rather than via a freshly-constructed `shiny::reactive()`.
#'
#' @param tool_name Character string with the tool's identifier
#'   (e.g. `"tool_household_iphra_v2"`).
#' @param session Shiny session object.
#' @return Logical indicating whether the tool is present on the protocol.
#' @export
iphra_has_protocol_tool <- function(tool_name, session = shiny::getDefaultReactiveDomain()) {
  version <- .iphra_module_version("protocol", session)
  version()

  protocol <- phr_get_modules(session)[["protocol"]]
  if (is.null(protocol) || is.null(protocol$tools)) {
    return(FALSE)
  }
  tool_name %in% names(protocol$tools)
}

#' @title Get the Current Indicator Bank
#' @description
#' Reactively retrieves the modified indicator bank from the `IPHRAProtocol`
#' object's `framework`. Depends on the protocol module's version signal, so
#' it stays current when the protocol / framework changes.
#'
#' Must be called from within an active reactive context (see
#' `iphra_has_protocol_tool()` for details); the version counter is read
#' directly rather than via a freshly-constructed `shiny::reactive()`.
#'
#' @param session Shiny session object.
#' @return A data frame with the current modified indicator bank, or an
#'   empty data frame with `indicator_code`, `indicator_name`, and `tool`
#'   columns if the protocol or framework is not yet available.
#' @export
iphra_get_indicator_bank <- function(session = shiny::getDefaultReactiveDomain()) {
  empty_bank <- data.frame(
    indicator_code = character(0),
    indicator_name = character(0),
    tool = character(0),
    stringsAsFactors = FALSE
  )

  version <- .iphra_module_version("protocol", session)
  version()

  protocol <- phr_get_modules(session)[["protocol"]]
  if (is.null(protocol) || is.null(protocol$framework)) {
    return(empty_bank)
  }

  bank <- protocol$framework$modified_indicator_bank
  if (is.null(bank) || !is.data.frame(bank)) {
    return(empty_bank)
  }
  bank
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
    init_session(session)
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
    init_session(session)
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
    init_session(session)
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



# Livelihoods Mapping Functions


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
