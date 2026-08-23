 options(warn = -1) # suppress warnings globally

 suppressPackageStartupMessages({
   library(shiny)
   # other libraries
 })

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  volumes <- c(Home = fs::path_home())

  shinyFiles::shinyFileSave(
    input,
    "save_tor",
    roots = volumes,
    session = session
  )

  # ────────────────────────────────────────────────────────────────────────────
  # CHECKBOX STATUS OBSERVERS
  # ────────────────────────────────────────────────────────────────────────────
  # These observers update the checkbox_status in session state when
  # checkboxes are toggled in the UI. The _master modules will reactively

  # update their status tables based on these values.

  # ---- Data Import Checkbox Observers ----
  # Example: When a data import checkbox is checked, update session state
  # These will be triggered by checkboxInput elements with matching IDs

  shiny::observeEvent(input$household_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "household_data_imported",
                                isTRUE(input$household_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$roster_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "roster_data_imported",
                                isTRUE(input$roster_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$mortality_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "mortality_data_imported",
                                isTRUE(input$mortality_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$nutrition_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "nutrition_data_imported",
                                isTRUE(input$nutrition_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$woman_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "woman_data_imported",
                                isTRUE(input$woman_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$health_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "health_data_imported",
                                isTRUE(input$health_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$water_data_imported_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("data", "water_data_imported",
                                isTRUE(input$water_data_imported_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # ---- Cleaning Checkbox Observers ----
  shiny::observeEvent(input$main_cleaning_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("cleaning", "main_cleaning_complete",
                                isTRUE(input$main_cleaning_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # ---- Quality Check Checkbox Observers ----
  shiny::observeEvent(input$general_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "general_quality_complete",
                                isTRUE(input$general_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$nutrition_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "nutrition_quality_complete",
                                isTRUE(input$nutrition_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$muac_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "muac_quality_complete",
                                isTRUE(input$muac_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$mortality_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "mortality_quality_complete",
                                isTRUE(input$mortality_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$health_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "health_quality_complete",
                                isTRUE(input$health_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$fsl_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "fsl_quality_complete",
                                isTRUE(input$fsl_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$wash_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "wash_quality_complete",
                                isTRUE(input$wash_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$other_quality_complete_checkbox, {
    iphra_try({
      iphra_set_checkbox_status("quality", "other_quality_complete",
                                isTRUE(input$other_quality_complete_checkbox), session)
    }, on_error = "warn", origin = "app_server", hint = "checkbox_status_update")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # ────────────────────────────────────────────────────────────────────────────
  # PLACEHOLDER OBSERVERS FOR NEW NAVBAR ITEMS
  # ────────────────────────────────────────────────────────────────────────────
  # These observers handle the new navbar menu items.
  # Most are placeholders for future functionality.

  # File Menu Observers
  observeEvent(input$new_project_btn, {
    iphra_try({
      # TODO: Implement new project creation
      showNotification(iphra_txt("New Project functionality coming soon"), type = "message")
      iphra_message("New Project button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: New Project")
  })

  observeEvent(input$save_project_as_btn, {
    iphra_try({
      # TODO: Implement save as dialog
      showNotification(iphra_txt("Save As functionality coming soon"), type = "message")
      iphra_message("Save Project As button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Save As")
  })

  observeEvent(input$import_data_type, {
    iphra_try({
      # TODO: Implement data import dialogs
      showNotification(paste(iphra_txt("Import"), input$import_data_type, iphra_txt("functionality coming soon")), type = "message")
      iphra_message(paste("Import data type:", input$import_data_type, "(placeholder)"), origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Import Data")
  })

  observeEvent(input$project_properties_btn, {
    iphra_try({
      # TODO: Implement project properties dialog
      showNotification(iphra_txt("Project Properties functionality coming soon"), type = "message")
      iphra_message("Project Properties button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Project Properties")
  })

  observeEvent(input$exit_app_btn, {
    iphra_try({
      # TODO: Implement exit confirmation
      showNotification(iphra_txt("Exit functionality coming soon - close browser tab to exit"), type = "message")
      iphra_message("Exit button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Exit")
  })

  # Edit Menu Observers
  observeEvent(input$undo_btn, {
    iphra_try({
      # TODO: Implement undo functionality
      showNotification(iphra_txt("Undo functionality coming soon"), type = "message")
      iphra_message("Undo button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Undo")
  })

  observeEvent(input$redo_btn, {
    iphra_try({
      # TODO: Implement redo functionality
      showNotification(iphra_txt("Redo functionality coming soon"), type = "message")
      iphra_message("Redo button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Redo")
  })

  observeEvent(input$find_replace_btn, {
    iphra_try({
      # TODO: Implement find and replace dialog
      showNotification(iphra_txt("Find and Replace functionality coming soon"), type = "message")
      iphra_message("Find and Replace button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Find Replace")
  })

  observeEvent(input$clear_all_data_btn, {
    iphra_try({
      # TODO: Implement clear all data with confirmation
      showNotification(iphra_txt("Clear All Data functionality coming soon"), type = "warning")
      iphra_message("Clear All Data button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Clear All Data")
  })

  observeEvent(input$preferences_btn, {
    iphra_try({
      # TODO: Implement preferences dialog
      showNotification(iphra_txt("Preferences functionality coming soon"), type = "message")
      iphra_message("Preferences button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Preferences")
  })

  observeEvent(input$debug_toggle, {
    iphra_try({
      # Show Debug Console modal
      showModal(mod_debug_console_ui("debug_console"))
      mod_debug_console_server("debug_console", parent_session = session)
      iphra_message("Debug Console opened", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Debug Console")
  })

  # View Menu Observers
  observeEvent(input$toggle_sidebar_btn, {
    iphra_try({
      # TODO: Implement sidebar toggle
      showNotification(iphra_txt("Toggle Sidebar functionality coming soon"), type = "message")
      iphra_message("Toggle Sidebar button clicked (placeholder)", origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Toggle Sidebar")
  })

  observeEvent(input$toggle_status_bar_btn, {
    iphra_try({
      # TODO: Implement status bar toggle
      showNotification(iphra_txt("Toggle Status Bar functionality coming soon"), type = "message")
      iphra_message("Toggle Status Bar button clicked (placeholder)", origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Toggle Status Bar")
  })

  observeEvent(input$theme_select, {
    iphra_try({
      # TODO: Implement theme switching
      theme <- input$theme_select
      showNotification(paste(iphra_txt("Theme:"), theme, iphra_txt("(coming soon)")), type = "message")
      iphra_message(paste("Theme selected:", theme, "(placeholder)"), origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Theme")
  })

  # Export Menu Observers
  observeEvent(input$export_doc, {
    iphra_try({
      # TODO: Implement document export
      doc_type <- input$export_doc
      showNotification(paste(iphra_txt("Export"), doc_type, iphra_txt("coming soon")), type = "message")
      iphra_message(paste("Export document:", doc_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Document")
  })

  observeEvent(input$export_data, {
    iphra_try({
      # TODO: Implement data export
      data_type <- input$export_data
      showNotification(paste(iphra_txt("Export"), data_type, iphra_txt("data coming soon")), type = "message")
      iphra_message(paste("Export data:", data_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Data")
  })

  observeEvent(input$export_analysis, {
    iphra_try({
      # TODO: Implement analysis export
      analysis_type <- input$export_analysis
      showNotification(paste(iphra_txt("Export"), analysis_type, iphra_txt("analysis coming soon")), type = "message")
      iphra_message(paste("Export analysis:", analysis_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Analysis")
  })

  observeEvent(input$export_ppt, {
    iphra_try({
      # TODO: Implement PPT export
      showNotification(iphra_txt("PowerPoint export coming soon"), type = "message")
      iphra_message("Export PPT button clicked (placeholder)", origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: PPT")
  })

  observeEvent(input$export_all_btn, {
    iphra_try({
      # TODO: Implement export all
      showNotification(iphra_txt("Export All functionality coming soon"), type = "message")
      iphra_message("Export All button clicked (placeholder)", origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Export All")
  })

  observeEvent(input$export_format, {
    iphra_try({
      # TODO: Implement export format selection
      format <- input$export_format
      showNotification(paste(iphra_txt("Export format set to:"), format), type = "message")
      iphra_message(paste("Export format set:", format, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Format")
  })

  # Help Menu Observers
  observeEvent(input$documentation_btn, {
    iphra_try({
      # TODO: Open documentation
      showNotification(iphra_txt("Documentation coming soon"), type = "message")
      iphra_message("Documentation button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Documentation")
  })

  observeEvent(input$tutorials_btn, {
    iphra_try({
      # TODO: Open tutorials
      showNotification(iphra_txt("Tutorials coming soon"), type = "message")
      iphra_message("Tutorials button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Tutorials")
  })

  observeEvent(input$keyboard_shortcuts_btn, {
    iphra_try({
      # TODO: Show keyboard shortcuts modal
      showNotification(iphra_txt("Keyboard Shortcuts coming soon"), type = "message")
      iphra_message("Keyboard Shortcuts button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Keyboard Shortcuts")
  })

  observeEvent(input$report_issue_btn, {
    iphra_try({
      # TODO: Open issue reporting dialog or link
      showNotification(iphra_txt("Report Issue functionality coming soon"), type = "message")
      iphra_message("Report Issue button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Report Issue")
  })

  observeEvent(input$check_updates_btn, {
    iphra_try({
      # TODO: Check for updates
      showNotification(iphra_txt("Check for Updates functionality coming soon"), type = "message")
      iphra_message("Check Updates button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Check Updates")
  })

  observeEvent(input$about_btn, {
    iphra_try({
      # Show about dialog
      showModal(modalDialog(
        title = iphra_txt("About IPHRA"),
        tagList(
          h3("IPHRA - Integrated Public Health Risk Assessment"),
          p(iphra_txt("Version: 0.0.0.9000")),
          p(iphra_txt("A Shiny application for conducting Integrated Public Health Risk Assessments.")),
          hr(),
          p(tags$strong(iphra_txt("Author:")), " Saeed Rahman"),
          p(tags$strong(iphra_txt("License:")), " CC BY 4.0"),
          hr(),
          p(iphra_txt("Provides tools for survey design, data collection monitoring, quality assurance, data cleaning, analysis, and reporting for multi-sectoral humanitarian health assessments."))
        ),
        footer = modalButton(iphra_txt("Close")),
        easyClose = TRUE
      ))
      iphra_message("About dialog opened", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: About")
  })

  # ────────────────────────────────────────────────────────────────────────────
  # MODULE SERVERS
  # ────────────────────────────────────────────────────────────────────────────

  # Your application server logic

  # GLOBAL ####

  # ---- Initialize Session State ----
  iphra_session <- init_session(session, project_name = "IPHRA")

  set_module(module_name = "protocol",
             module_object = phr::IPHRAProtocol$new(
               assessment_title = "IPHRA",
               country_name = "TBD",
               month_year = "2026-01-01"
               ),
             session = session)

  protocol_r <- phr_get_module_reactive("protocol", session)


  # ────────────────────────────────────────────────────────────────────────────
  # REACH Terms of Reference — Export via generate_quarto_doc()
  # ────────────────────────────────────────────────────────────────────────────
  # The "REACH Terms of Reference" menu item in the Export dropdown (see
  # app_ui.R) clicks a hidden shinyFiles::shinySaveButton with id = "save_tor"
  # directly. That opens the shinyFiles save dialog. When the user confirms
  # a save location, `input$save_tor` transitions to a completed state and
  # this observer runs generate_quarto_doc() writing to that location.
  #
  # NOTE: previously this was wrapped in a modalDialog containing the
  # shinySaveButton. Nesting the shinyFiles dialog inside another modalDialog
  # caused z-index / focus conflicts that prevented the "Save" click from
  # committing the selection, so parseSavePath() always returned 0 rows and
  # generate_quarto_doc() was never called.

  observeEvent(input$save_tor, {

    iphra_try({

      save_path <- shinyFiles::parseSavePath(
        volumes,
        input$save_tor
      )

      # Ignore the initial / "select" state emitted by shinySaveButton before
      # the user has actually confirmed a filename in the save dialog.
      req(nrow(save_path) > 0)

      outfile <- save_path$datapath[1]

      protocol <- protocol_r()
      if (is.null(protocol)) {
        iphra_warning(
          iphra_txt("Protocol object is not available — cannot export."),
          origin = "Export: REACH Terms of Reference"
        )
        return(NULL)
      }

      iphra_message(
        paste("Rendering REACH Terms of Reference to:", outfile),
        origin = "Export: REACH Terms of Reference"
      )

      # Quarto rendering can take several seconds; show a progress indicator
      # so the user knows the export is running.
      withProgress(
        message = iphra_txt("Generating REACH Terms of Reference..."),
        value = 0.1,
        {
          protocol$generate_quarto_doc(
            output_file = outfile
          )
          setProgress(value = 1)
        }
      )

      iphra_message(
        paste("REACH Terms of Reference saved to:", outfile),
        origin = "Export: REACH Terms of Reference"
      )

      showNotification(
        paste(iphra_txt("REACH Terms of Reference saved to:"), outfile),
        type = "message",
        duration = 6
      )

    }, on_error = "warn", origin = "Export: REACH Terms of Reference")

  })



  # NOTE: `set_module()` above registers the protocol's reactive
  # "version" signal (`session$userData$modules_version[["protocol"]]`).
  # Any code that mutates the protocol object (e.g. `protocol$add_tools()`)
  # must call `iphra_touch_module("protocol", session)` afterwards so
  # dependent modules re-evaluate. Read the protocol reactively via
  # `iphra_get_module_reactive("protocol", session)` instead of caching a
  # one-time snapshot of the object. See `R/utils_session.R` for details.

  # iphra_get_log_store(session)

  # ---- Global Language Selection ---
  observeEvent(input$global_language, {
    iphra_try({

      # 1️⃣ VALIDATION
      if (is.null(input$global_language)) {
        iphra_warning(
          iphra_txt("Language input is NULL — skipping update."),
          origin = iphra_txt("Global Language Selector")
        )
        return(NULL)
      }

      # 2️⃣ CORE LOGIC
      # ---- Future: this will set the session-wide language ---
      # session$userData$lang(input$global_language)
      # iphra_current_lang <<- input$global_language  # (temporary fallback until session connected)

      iphra_message(
        paste(
          iphra_txt("Language selection changed to:"),
          input$global_language
        ),
        origin = iphra_txt("Global Language Selector")
      )

      # ---- Dummy placeholder behavior for now ---
      # (In future: trigger UI text refresh or reactive translation re-render)
      iphra_message(
        iphra_txt("Dummy mode: UI text translations not yet reactive."),
        origin = iphra_txt("Global Language Selector")
      )

      # 3️⃣ RESULT HANDLING
      iphra_message(
        iphra_txt("Language selection update processed successfully."),
        origin = iphra_txt("Global Language Selector")
      )

    },
    on_error = "warn",
    origin = iphra_txt("Global Language Selector"),
    hint = iphra_txt("Verify input ID or future session$userData$lang connection if this fails.")
    )
  })

  # GOALS AND OBJECTIVES ####

  mod_goals_server("goals")

  # TOOLS ####

  mod_tools_master_server("tools_master")

  mod_tools_household_server("tools_household")

  mod_tools_community_kii_server("tools_community_kii")
  mod_tools_community_obs_server("tools_community_obs")

  mod_tools_fsl_kii_server("tools_fsl_kii")
  mod_tools_market_vendor_kii_server("tools_market_vendor_kii")
  mod_tools_crop_livestock_obs_server("tools_crop_livestock_obs")

  mod_tools_health_kii_server("tools_health")
  mod_tools_health_obs_server("tools_health")
  mod_tools_nutrition_kii_server("tools_nutrition_kii")

  mod_tools_wash_kii_server("tools_wash_kii")
  mod_tools_water_point_obs_server("tools_water_point_obs")
  mod_tools_latrine_obs_server("tools_latrine_obs")





  # PLANNING ####

  mod_planning_sample_size_server("planning_sample_size")

  # TOOLs TAB ####

  output$tool_tabs <- renderUI({

    protocol <- protocol_r()

    tabs <- list()

    if (isTRUE(protocol$.tool_household_iphra)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Household Survey"),
            mod_tools_household_ui("tools_household")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_community_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Community Key Informant"),
            mod_tools_community_kii_ui("tools_community_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_community_observation)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Community Observation"),
            mod_tools_community_obs_ui("tools_community_obs")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_fsl_provider_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("FSL Provider Key Informant"),
            mod_tools_fsl_kii_ui("tools_fsl_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_market_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Market Vendor Key Informant"),
            mod_tools_market_vendor_kii_ui("tools_market_vendor_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_crops_livestock_observation)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Crop and Livestock Observation"),
            mod_tools_crop_livestock_obs_ui("tools_crop_livestock_obs")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_health_facility_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Health Facility Key Informant"),
            mod_tools_health_kii_ui("tools_health_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_health_facility_observation)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Health Facility Observation"),
            mod_tools_health_obs_ui("tools_health_obs")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_nutrition_facility_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Nutrition Facility Key Informant"),
            mod_tools_nutrition_kii_ui("tools_nutrition_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_wash_provider_kii)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("WASH Provider Key Informant"),
            mod_tools_wash_kii_ui("tools_wash_kii")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_water_point_observation)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Water Point Observation"),
            mod_tools_water_point_obs_ui("tools_water_point_obs")
          )
        )
      )
    }

    if (isTRUE(protocol$.tool_latrine_observation)) {
      tabs <- c(
        tabs,
        list(
          tabPanel(
            iphra_txt("Latrine Observation"),
            mod_tools_latrine_obs_ui("tools_latrine_obs")
          )
        )
      )
    }

    do.call(
      tabsetPanel,
      c(
        list(id = "tool_tabs", selected = selected_tool_tab()),
        tabs
      )
    )

  })

  selected_tool_tab <- reactiveVal(NULL)

  observeEvent(input$tool_tabs, {
    selected_tool_tab(input$tool_tabs)
  })

  # PROJECT FILE MANAGEMENT ####

  # ---- Save Project Handler ----
  observeEvent(input$save_project_btn, {
    iphra_try({
      # ────────────────────────────────────────────────
      # 1️⃣ VALIDATION & PRECONDITIONS
      # ────────────────────────────────────────────────
      iphra_message(
        iphra_txt("Save project initiated."),
        origin = iphra_txt("Project File Manager")
      )

      # ────────────────────────────────────────────────
      # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
      # ────────────────────────────────────────────────
      # In production, this would trigger a download dialog
      # For now, save to a temp file for demonstration
      temp_path <- tempfile(
        pattern = paste0(iphra_session$get_project_name(), "_"),
        fileext = ".rds"
      )
      iphra_session$save_project(temp_path)

      iphra_message(
        paste(iphra_txt("Project saved to:"), temp_path),
        origin = iphra_txt("Project File Manager")
      )

      # ────────────────────────────────────────────────
      # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
      # ────────────────────────────────────────────────
      iphra_message(
        iphra_txt("Project save completed successfully."),
        origin = iphra_txt("Project File Manager")
      )
    },
    on_error = "warn",
    origin = iphra_txt("Project File Manager: Save"),
    hint = iphra_txt("Check file permissions and session state if save fails.")
    )
  })

  # ---- Load Project Handler ----
  observeEvent(input$load_project_file, {
    iphra_try({
      # ────────────────────────────────────────────────
      # 1️⃣ VALIDATION & PRECONDITIONS
      # ────────────────────────────────────────────────
      req(input$load_project_file)

      iphra_message(
        iphra_txt("Load project initiated."),
        origin = iphra_txt("Project File Manager")
      )

      file_path <- input$load_project_file$datapath

      if (!file.exists(file_path)) {
        iphra_error(
          iphra_txt("Selected file does not exist."),
          origin = iphra_txt("Project File Manager")
        )
        return(NULL)
      }

      # ────────────────────────────────────────────────
      # 2️⃣ CORE LOGIC / MAIN FUNCTIONALITY
      # ────────────────────────────────────────────────
      iphra_session$load_project(file_path)

      iphra_message(
        paste(iphra_txt("Project loaded:"), iphra_session$get_project_name()),
        origin = iphra_txt("Project File Manager")
      )

      # ────────────────────────────────────────────────
      # 3️⃣ RESULT HANDLING / OUTPUT ACTIONS
      # ────────────────────────────────────────────────
      iphra_message(
        iphra_txt("Project load completed successfully."),
        origin = iphra_txt("Project File Manager")
      )
    },
    on_error = "warn",
    origin = iphra_txt("Project File Manager: Load"),
    hint = iphra_txt("Ensure file is a valid IPHRA project file (.rds or .json).")
    )
  })

  # LOGGING ####

  # output$log_table <- renderTable({
  #   iphra_get_logs()()
  # })



}
