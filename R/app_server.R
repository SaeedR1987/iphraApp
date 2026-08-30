 options(warn = -1) # suppress warnings globally

 suppressPackageStartupMessages({
   library(shiny)
   # other libraries
 })

 # Load translations at package load time
 phr_translations <- phrutils::phr_load_translations()

 # Placeholder for current language (can later live in session$userData)
 phr_current_lang <- "en"

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  session$userData$lang <- reactiveVal("en")

  observeEvent(input$global_language, {
    phrutils::set_phr_language(
      input$global_language,
      session
    )
  })

  volumes <- c(Home = fs::path_home())

  shinyFiles::shinyFileSave(
    input,
    "save_tor",
    roots = volumes,
    session = session
  )

  shinyFiles::shinyFileSave(
    input,
    "save_project",
    roots = volumes,
    session = session
  )



  # ────────────────────────────────────────────────────────────────────────────
  # PLACEHOLDER OBSERVERS FOR NEW NAVBAR ITEMS
  # ────────────────────────────────────────────────────────────────────────────
  # These observers handle the new navbar menu items.
  # Most are placeholders for future functionality.

  # File Menu Observers
  observeEvent(input$new_project_btn, {
    iphra_try({
      # TODO: Implement new project creation
      showNotification(phrutils::phr_txt("New Project functionality coming soon"), type = "message")
      iphra_message("New Project button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: New Project")
  })

  observeEvent(input$save_project_as_btn, {
    # The "Save Project As..." menu item opens the hidden shinyFiles save
    # dialog directly via onclick (see app_ui.R); the actual save is handled
    # by the input$save_project observer below. This observer is intentionally
    # a no-op and only exists to swallow any legacy click events.
  })

  observeEvent(input$global_language, {

    phrutils::set_phr_language(
      input$global_language,
      session
    )

  })

  # ---- Quick Save Handler ----
  # The "Save Project" menu item sets `input$save_project_btn` on click. If
  # the project has already been saved to a known path (recorded in
  # `session$userData$project$path`), we quick-save silently to that path
  # without prompting. Otherwise we open the shinyFiles save dialog exactly
  # as "Save Project As..." would, so the user can pick a destination.
  observeEvent(input$save_project_btn, {
    iphra_try({
      current_path <- shiny::isolate(session$userData$project$path)

      is_writable_dir <- function(p) {
        d <- dirname(p)
        dir.exists(d) && file.access(d, mode = 2) == 0
      }

      if (is.character(current_path) &&
          length(current_path) == 1 &&
          nzchar(current_path) &&
          is_writable_dir(current_path)) {

        written_path <- iphra_save_project_file(current_path, session = session)

        iphra_message(
          paste(phrutils::phr_txt("Project saved to:"), written_path),
          origin = phrutils::phr_txt("Project File Manager")
        )

        showNotification(
          paste(phrutils::phr_txt("Project saved to:"), written_path),
          type = "message",
          duration = 6
        )
      } else {
        # No known path yet — fall back to opening the shinyFiles save
        # dialog by clicking the hidden shinySaveButton client-side.
        session$sendCustomMessage("iphra_click_element", "save_project")
      }
    },
    on_error = "warn",
    origin = phrutils::phr_txt("Project File Manager: Save"),
    hint = phrutils::phr_txt("Check file permissions and session state if save fails.")
    )
  })

  observeEvent(input$import_data_type, {
    iphra_try({
      # TODO: Implement data import dialogs
      showNotification(paste(phrutils::phr_txt("Import"), input$import_data_type, phrutils::phr_txt("functionality coming soon")), type = "message")
      iphra_message(paste("Import data type:", input$import_data_type, "(placeholder)"), origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Import Data")
  })

  observeEvent(input$project_properties_btn, {
    iphra_try({
      # TODO: Implement project properties dialog
      showNotification(phrutils::phr_txt("Project Properties functionality coming soon"), type = "message")
      iphra_message("Project Properties button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Project Properties")
  })

  observeEvent(input$exit_app_btn, {
    iphra_try({
      # TODO: Implement exit confirmation
      showNotification(phrutils::phr_txt("Exit functionality coming soon - close browser tab to exit"), type = "message")
      iphra_message("Exit button clicked (placeholder)", origin = "File Menu")
    }, on_error = "warn", origin = "File Menu: Exit")
  })

  # Edit Menu Observers
  observeEvent(input$undo_btn, {
    iphra_try({
      # TODO: Implement undo functionality
      showNotification(phrutils::phr_txt("Undo functionality coming soon"), type = "message")
      iphra_message("Undo button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Undo")
  })

  observeEvent(input$redo_btn, {
    iphra_try({
      # TODO: Implement redo functionality
      showNotification(phrutils::phr_txt("Redo functionality coming soon"), type = "message")
      iphra_message("Redo button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Redo")
  })

  observeEvent(input$find_replace_btn, {
    iphra_try({
      # TODO: Implement find and replace dialog
      showNotification(phrutils::phr_txt("Find and Replace functionality coming soon"), type = "message")
      iphra_message("Find and Replace button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Find Replace")
  })

  observeEvent(input$clear_all_data_btn, {
    iphra_try({
      # TODO: Implement clear all data with confirmation
      showNotification(phrutils::phr_txt("Clear All Data functionality coming soon"), type = "warning")
      iphra_message("Clear All Data button clicked (placeholder)", origin = "Edit Menu")
    }, on_error = "warn", origin = "Edit Menu: Clear All Data")
  })

  observeEvent(input$preferences_btn, {
    iphra_try({
      # TODO: Implement preferences dialog
      showNotification(phrutils::phr_txt("Preferences functionality coming soon"), type = "message")
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
      showNotification(phrutils::phr_txt("Toggle Sidebar functionality coming soon"), type = "message")
      iphra_message("Toggle Sidebar button clicked (placeholder)", origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Toggle Sidebar")
  })

  observeEvent(input$toggle_status_bar_btn, {
    iphra_try({
      # TODO: Implement status bar toggle
      showNotification(phrutils::phr_txt("Toggle Status Bar functionality coming soon"), type = "message")
      iphra_message("Toggle Status Bar button clicked (placeholder)", origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Toggle Status Bar")
  })

  observeEvent(input$theme_select, {
    iphra_try({
      # TODO: Implement theme switching
      theme <- input$theme_select
      showNotification(paste(phrutils::phr_txt("Theme:"), theme, phrutils::phr_txt("(coming soon)")), type = "message")
      iphra_message(paste("Theme selected:", theme, "(placeholder)"), origin = "View Menu")
    }, on_error = "warn", origin = "View Menu: Theme")
  })

  # Export Menu Observers
  observeEvent(input$export_doc, {
    iphra_try({
      # TODO: Implement document export
      doc_type <- input$export_doc
      showNotification(paste(phrutils::phr_txt("Export"), doc_type, phrutils::phr_txt("coming soon")), type = "message")
      iphra_message(paste("Export document:", doc_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Document")
  })

  observeEvent(input$export_data, {
    iphra_try({
      # TODO: Implement data export
      data_type <- input$export_data
      showNotification(paste(phrutils::phr_txt("Export"), data_type, phrutils::phr_txt("data coming soon")), type = "message")
      iphra_message(paste("Export data:", data_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Data")
  })

  observeEvent(input$export_analysis, {
    iphra_try({
      # TODO: Implement analysis export
      analysis_type <- input$export_analysis
      showNotification(paste(phrutils::phr_txt("Export"), analysis_type, phrutils::phr_txt("analysis coming soon")), type = "message")
      iphra_message(paste("Export analysis:", analysis_type, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Analysis")
  })

  observeEvent(input$export_ppt, {
    iphra_try({
      # TODO: Implement PPT export
      showNotification(phrutils::phr_txt("PowerPoint export coming soon"), type = "message")
      iphra_message("Export PPT button clicked (placeholder)", origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: PPT")
  })

  observeEvent(input$export_all_btn, {
    iphra_try({
      # TODO: Implement export all
      showNotification(phrutils::phr_txt("Export All functionality coming soon"), type = "message")
      iphra_message("Export All button clicked (placeholder)", origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Export All")
  })

  observeEvent(input$export_format, {
    iphra_try({
      # TODO: Implement export format selection
      format <- input$export_format
      showNotification(paste(phrutils::phr_txt("Export format set to:"), format), type = "message")
      iphra_message(paste("Export format set:", format, "(placeholder)"), origin = "Export Menu")
    }, on_error = "warn", origin = "Export Menu: Format")
  })

  # Help Menu Observers
  observeEvent(input$documentation_btn, {
    iphra_try({
      # TODO: Open documentation
      showNotification(phrutils::phr_txt("Documentation coming soon"), type = "message")
      iphra_message("Documentation button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Documentation")
  })

  observeEvent(input$tutorials_btn, {
    iphra_try({
      # TODO: Open tutorials
      showNotification(phrutils::phr_txt("Tutorials coming soon"), type = "message")
      iphra_message("Tutorials button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Tutorials")
  })

  observeEvent(input$keyboard_shortcuts_btn, {
    iphra_try({
      # TODO: Show keyboard shortcuts modal
      showNotification(phrutils::phr_txt("Keyboard Shortcuts coming soon"), type = "message")
      iphra_message("Keyboard Shortcuts button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Keyboard Shortcuts")
  })

  observeEvent(input$report_issue_btn, {
    iphra_try({
      # TODO: Open issue reporting dialog or link
      showNotification(phrutils::phr_txt("Report Issue functionality coming soon"), type = "message")
      iphra_message("Report Issue button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Report Issue")
  })

  observeEvent(input$check_updates_btn, {
    iphra_try({
      # TODO: Check for updates
      showNotification(phrutils::phr_txt("Check for Updates functionality coming soon"), type = "message")
      iphra_message("Check Updates button clicked (placeholder)", origin = "Help Menu")
    }, on_error = "warn", origin = "Help Menu: Check Updates")
  })

  observeEvent(input$about_btn, {
    iphra_try({
      # Show about dialog
      showModal(modalDialog(
        title = phrutils::phr_txt("About IPHRA"),
        tagList(
          h3("IPHRA - Integrated Public Health Risk Assessment"),
          p(phrutils::phr_txt("Version: 0.0.0.9000")),
          p(phrutils::phr_txt("A Shiny application for conducting Integrated Public Health Risk Assessments.")),
          hr(),
          p(tags$strong(phrutils::phr_txt("Author:")), " Saeed Rahman"),
          p(tags$strong(phrutils::phr_txt("License:")), " CC BY 4.0"),
          hr(),
          p(phrutils::phr_txt("Provides tools for survey design, data collection monitoring, quality assurance, data cleaning, analysis, and reporting for multi-sectoral humanitarian health assessments."))
        ),
        footer = modalButton(phrutils::phr_txt("Close")),
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
             module_object = IPHRAProtocol$new(
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

    phrutils::phr_try({

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
        phrutils::phr_warning(
          phrutils::phr_txt("Protocol object is not available — cannot export."),
          origin = "Export: REACH Terms of Reference"
        )
        return(NULL)
      }

      phrutils::phr_message(
        paste("Rendering REACH Terms of Reference to:", outfile),
        origin = "Export: REACH Terms of Reference"
      )

      # Quarto rendering can take several seconds; show a progress indicator
      # so the user knows the export is running.
      showModal(
        modalDialog(
          title = phrutils::phr_txt("Generating REACH Terms of Reference"),
          tagList(
            tags$p(phrutils::phr_txt("Please wait while the document is generated.")),
            tags$div(
              style = "text-align:center; padding:20px;",
              tags$i(
                class = "fa fa-spinner fa-spin fa-3x"
              )
            )
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )

      on.exit(removeModal(), add = TRUE)

      protocol$generate_quarto_doc(
        output_file = outfile
      )

      phrutils::phr_message(
        paste("REACH Terms of Reference saved to:", outfile),
        origin = "Export: REACH Terms of Reference"
      )

      showNotification(
        paste(phrutils::phr_txt("REACH Terms of Reference saved to:"), outfile),
        type = "message",
        duration = 6
      )

    }, on_error = "warn", origin = "Export: REACH Terms of Reference")

  })



  # NOTE: `set_module()` above registers the protocol's reactive
  # "version" signal (`session$userData$modules_version[["protocol"]]`).
  # Any code that mutates the protocol object (e.g. `protocol$add_tools()`)
  # must call `phr_touch_module("protocol", session)` afterwards so
  # dependent modules re-evaluate. Read the protocol reactively via
  # `phr_get_module_reactive("protocol", session)` instead of caching a
  # one-time snapshot of the object. See `R/utils_session.R` for details.

  # phr_get_log_store(session)

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

  mod_tools_health_kii_server("tools_health_kii")
  mod_tools_health_obs_server("tools_health_obs")
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
            phrutils::phr_txt("Household Survey"),
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
            phrutils::phr_txt("Community Key Informant"),
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
            phrutils::phr_txt("Community Observation"),
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
            phrutils::phr_txt("FSL Provider Key Informant"),
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
            phrutils::phr_txt("Market Vendor Key Informant"),
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
            phrutils::phr_txt("Crop and Livestock Observation"),
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
            phrutils::phr_txt("Health Facility Key Informant"),
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
            phrutils::phr_txt("Health Facility Observation"),
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
            phrutils::phr_txt("Nutrition Facility Key Informant"),
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
            phrutils::phr_txt("WASH Provider Key Informant"),
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
            phrutils::phr_txt("Water Point Observation"),
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
            phrutils::phr_txt("Latrine Observation"),
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
  # The "Save Project" and "Save Project As..." menu items in app_ui.R click
  # a hidden shinyFiles::shinySaveButton (id = "save_project"), which opens
  # the shinyFiles save dialog. When the user confirms a destination,
  # `input$save_project` transitions to a completed state and this observer
  # writes the current session state to a `.iphra` file at that path via
  # iphra_save_project_file().
  observeEvent(input$save_project, {
    iphra_try({
      save_path <- shinyFiles::parseSavePath(
        volumes,
        input$save_project
      )

      # Ignore the initial / "select" state emitted by shinySaveButton before
      # the user has actually confirmed a filename in the save dialog.
      req(nrow(save_path) > 0)

      outfile <- save_path$datapath[1]

      iphra_message(
        phrutils::phr_txt("Save project initiated."),
        origin = phrutils::phr_txt("Project File Manager")
      )

      written_path <- iphra_save_project_file(outfile, session = session)

      iphra_message(
        paste(phrutils::phr_txt("Project saved to:"), written_path),
        origin = phrutils::phr_txt("Project File Manager")
      )

      showNotification(
        paste(phrutils::phr_txt("Project saved to:"), written_path),
        type = "message",
        duration = 6
      )
    },
    on_error = "warn",
    origin = phrutils::phr_txt("Project File Manager: Save"),
    hint = phrutils::phr_txt("Check file permissions and session state if save fails.")
    )
  })

  # ---- Load Project Handler ----
  # The "Open Project" menu item is a shiny::fileInput (id =
  # "load_project_file") restricted to `.iphra` files. When the user picks a
  # file, `input$load_project_file` becomes a data frame with a `datapath`
  # column pointing to the uploaded copy; iphra_load_project_file() reads
  # that copy and reinitializes the current session's serializable state.
  observeEvent(input$load_project_file, {
    iphra_try({
      req(input$load_project_file)

      iphra_message(
        phrutils::phr_txt("Load project initiated."),
        origin = phrutils::phr_txt("Project File Manager")
      )

      file_path <- input$load_project_file$datapath

      if (!file.exists(file_path)) {
        iphra_error(
          phrutils::phr_txt("Selected file does not exist."),
          origin = phrutils::phr_txt("Project File Manager")
        )
        return(NULL)
      }

      iphra_load_project_file(file_path, session = session)

      iphra_message(
        paste(phrutils::phr_txt("Project loaded:"), iphra_session$get_project_name()),
        origin = phrutils::phr_txt("Project File Manager")
      )

      showNotification(
        paste(phrutils::phr_txt("Project loaded:"), iphra_session$get_project_name()),
        type = "message",
        duration = 6
      )
    },
    on_error = "warn",
    origin = phrutils::phr_txt("Project File Manager: Load"),
    hint = phrutils::phr_txt("Ensure file is a valid IPHRA project file (.iphra).")
    )
  })

  # LOGGING ####

  # output$log_table <- renderTable({
  #   iphra_get_logs()()
  # })



}
