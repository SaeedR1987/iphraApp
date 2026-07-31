#' Debug Console Module
#'
#' @description A Shiny module for displaying debug information and exporting debug bundles.
#' Provides functionality to view logs, download snapshots, export inputs, and create debug bundles.
#'
#' @param id Character string. Namespace ID for the module.
#'
#' @rdname mod_debug_console
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS modalDialog downloadButton downloadHandler textAreaInput
mod_debug_console_ui <- function(id) {
  ns <- NS(id)
  
  modalDialog(
    title = iphra_txt("debug_console"),
    size = "l",
    tagList(
      h4(iphra_txt("application_logs")),
      p(iphra_txt("view_recent_application_logs_and_export_debug_information")),
      
      # Log preview area
      div(
        style = "margin-bottom: 20px;",
        textAreaInput(
          ns("log_preview"),
          label = iphra_txt("log_preview_last_100_entries"),
          value = "",
          width = "100%",
          height = "300px",
          resize = "vertical"
        )
      ),
      
      # Action buttons
      div(
        style = "margin-top: 15px;",
        downloadButton(ns("download_logs"), iphra_txt("download_logs"), class = "btn-primary"),
        tags$span(style = "margin-left: 10px;"),
        downloadButton(ns("download_snapshot"), iphra_txt("download_snapshot_rds"), class = "btn-info"),
        tags$span(style = "margin-left: 10px;"),
        downloadButton(ns("download_inputs"), iphra_txt("download_inputs_json"), class = "btn-info"),
        tags$span(style = "margin-left: 10px;"),
        downloadButton(ns("download_bundle"), iphra_txt("download_debug_bundle_zip"), class = "btn-success")
      ),
      
      hr(),
      
      div(
        style = "font-size: 0.9em; color: #666;",
        p(iphra_txt("session_information")),
        verbatimTextOutput(ns("session_info"), placeholder = TRUE)
      )
    ),
    footer = modalButton(iphra_txt("close")),
    easyClose = TRUE
  )
}

#' Debug Console Module Server Function
#'
#' @rdname mod_debug_console
#'
#' @param id Character string. Namespace ID for the module.
#' @param input,output,session Internal Shiny parameters.
#'
#' @keywords internal
#' @export
#' @importFrom shiny moduleServer reactive observe updateTextAreaInput
mod_debug_console_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    # Update log preview when modal opens
    observe({
      iphra_try({
        # Get logs from parent session
        if (!is.null(parent_session$userData$iphra_log_store)) {
          log_store <- parent_session$userData$iphra_log_store
          logs <- log_store()
          
          if (nrow(logs) > 0) {
            # Get last 100 entries
            n_logs <- min(100, nrow(logs))
            recent_logs <- tail(logs, n_logs)
            
            # Format logs as text
            log_text <- paste(
              apply(recent_logs, 1, function(row) {
                sprintf("[%s] [%s] %s: %s", 
                        row["time"], 
                        row["level"], 
                        row["origin"], 
                        row["message"])
              }),
              collapse = "\n"
            )
            
            updateTextAreaInput(session, "log_preview", value = log_text)
          } else {
            updateTextAreaInput(session, "log_preview", value = iphra_txt("no_logs_available"))
          }
        } else {
          updateTextAreaInput(session, "log_preview", value = iphra_txt("no_logs_available"))
        }
      }, on_error = "warn", origin = "Debug Console: Update Logs")
    })
    
    # Session info output
    output$session_info <- renderText({
      iphra_try({
        session_id <- parent_session$userData$session_id %||% "Unknown"
        session_start <- parent_session$userData$session_start %||% "Unknown"
        
        paste0(
          "Session ID: ", session_id, "\n",
          "Started: ", session_start, "\n",
          "R Version: ", R.version.string
        )
      }, on_error = "warn", origin = "Debug Console: Session Info", default = "Session information unavailable")
    })
    
    # Download logs handler
    output$download_logs <- downloadHandler(
      filename = function() {
        paste0("iphra_logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      },
      content = function(file) {
        iphra_try({
          if (!is.null(parent_session$userData$iphra_log_store)) {
            log_store <- parent_session$userData$iphra_log_store
            logs <- log_store()
            
            if (nrow(logs) > 0) {
              # Write logs as formatted text
              log_text <- paste(
                apply(logs, 1, function(row) {
                  sprintf("[%s] [%s] %s: %s", 
                          row["time"], 
                          row["level"], 
                          row["origin"], 
                          row["message"])
                }),
                collapse = "\n"
              )
              writeLines(log_text, file)
              iphra_message("Logs downloaded successfully", origin = "Debug Console")
            } else {
              writeLines("No logs available.", file)
            }
          } else {
            writeLines("No logs available.", file)
          }
        }, on_error = "warn", origin = "Debug Console: Download Logs")
      }
    )
    
    # Download snapshot handler
    output$download_snapshot <- downloadHandler(
      filename = function() {
        paste0("iphra_snapshot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        iphra_try({
          # Create snapshot of session state
          snapshot <- list(
            session_id = parent_session$userData$session_id,
            session_start = parent_session$userData$session_start,
            timestamp = Sys.time(),
            iphra_session = if (!is.null(parent_session$userData$iphra_session)) {
              # Serialize IPHRASession object
              parent_session$userData$iphra_session$serialize()
            } else {
              NULL
            },
            settings = if (!is.null(parent_session$userData$settings)) {
              reactiveValuesToList(parent_session$userData$settings)
            } else {
              NULL
            },
            project = if (!is.null(parent_session$userData$project)) {
              reactiveValuesToList(parent_session$userData$project)
            } else {
              NULL
            },
            flags = if (!is.null(parent_session$userData$flags)) {
              reactiveValuesToList(parent_session$userData$flags)
            } else {
              NULL
            }
          )
          
          saveRDS(snapshot, file)
          iphra_message("Snapshot saved successfully", origin = "Debug Console")
        }, on_error = "warn", origin = "Debug Console: Download Snapshot")
      }
    )
    
    # Download inputs handler
    output$download_inputs <- downloadHandler(
      filename = function() {
        paste0("iphra_inputs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      },
      content = function(file) {
        iphra_try({
          # Get all input values from parent session
          inputs_list <- reactiveValuesToList(parent_session$input)
          
          # Convert to JSON
          json_content <- jsonlite::toJSON(inputs_list, pretty = TRUE, auto_unbox = TRUE)
          
          writeLines(as.character(json_content), file)
          iphra_message("Inputs exported to JSON successfully", origin = "Debug Console")
        }, on_error = "warn", origin = "Debug Console: Download Inputs")
      }
    )
    
    # Download debug bundle handler
    output$download_bundle <- downloadHandler(
      filename = function() {
        paste0("iphra_debug_bundle_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        iphra_try({
          # Create temporary directory for bundle files
          temp_dir <- tempdir()
          bundle_dir <- file.path(temp_dir, paste0("iphra_debug_", format(Sys.time(), "%Y%m%d_%H%M%S")))
          dir.create(bundle_dir, showWarnings = FALSE, recursive = TRUE)
          
          # 1. Session info
          session_info_file <- file.path(bundle_dir, "sessionInfo.txt")
          sink(session_info_file)
          cat("IPHRA Debug Bundle\n")
          cat("==================\n\n")
          cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
          cat("Session ID:", parent_session$userData$session_id %||% "Unknown", "\n")
          cat("Session Start:", as.character(parent_session$userData$session_start %||% "Unknown"), "\n\n")
          cat("R Session Information:\n")
          cat("=====================\n")
          print(sessionInfo())
          sink()
          
          # 2. Logs
          if (!is.null(parent_session$userData$iphra_log_store)) {
            logs_file <- file.path(bundle_dir, "logs.txt")
            log_store <- parent_session$userData$iphra_log_store
            logs <- log_store()
            
            if (nrow(logs) > 0) {
              log_text <- paste(
                apply(logs, 1, function(row) {
                  sprintf("[%s] [%s] %s: %s", 
                          row["time"], 
                          row["level"], 
                          row["origin"], 
                          row["message"])
                }),
                collapse = "\n"
              )
              writeLines(log_text, logs_file)
            }
          }
          
          # 3. Inputs JSON
          inputs_file <- file.path(bundle_dir, "inputs.json")
          inputs_list <- reactiveValuesToList(parent_session$input)
          json_content <- jsonlite::toJSON(inputs_list, pretty = TRUE, auto_unbox = TRUE)
          writeLines(as.character(json_content), inputs_file)
          
          # 4. Reactive values snapshot
          snapshot_file <- file.path(bundle_dir, "reactive_values.rds")
          snapshot <- list(
            session_id = parent_session$userData$session_id,
            session_start = parent_session$userData$session_start,
            timestamp = Sys.time(),
            iphra_session = if (!is.null(parent_session$userData$iphra_session)) {
              parent_session$userData$iphra_session$serialize()
            } else {
              NULL
            },
            settings = if (!is.null(parent_session$userData$settings)) {
              reactiveValuesToList(parent_session$userData$settings)
            } else {
              NULL
            },
            project = if (!is.null(parent_session$userData$project)) {
              reactiveValuesToList(parent_session$userData$project)
            } else {
              NULL
            }
          )
          saveRDS(snapshot, snapshot_file)
          
          # Create zip file
          current_wd <- getwd()
          setwd(temp_dir)
          files_to_zip <- list.files(basename(bundle_dir), full.names = TRUE, recursive = TRUE)
          zip::zip(file, files = files_to_zip, mode = "cherry-pick")
          setwd(current_wd)
          
          # Clean up
          unlink(bundle_dir, recursive = TRUE)
          
          iphra_message("Debug bundle created successfully", origin = "Debug Console")
        }, on_error = "warn", origin = "Debug Console: Download Bundle")
      }
    )
    
  })
}
