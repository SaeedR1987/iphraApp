# ────────────────────────────────────────────────────────────────────────────────
# Master Table Helper Functions
# ────────────────────────────────────────────────────────────────────────────────
#
# Utility functions for creating and rendering master status tables
# These tables provide summary views of step completion status
# ────────────────────────────────────────────────────────────────────────────────

#' @title Render Master Status Table
#' @description
#' Renders an HTML table showing the status of various steps.
#' Used by _master modules to display completion status.
#'
#' @param status_df A data frame with Step, Status, and Icon columns.
#' @param title Optional title for the table.
#' @return HTML table output suitable for use in renderUI.
#' @export
render_master_status_table <- function(status_df, title = NULL) {
  if (nrow(status_df) == 0) {
    return(shiny::tags$p("No status information available."))
  }

  rows <- lapply(seq_len(nrow(status_df)), function(i) {
    shiny::tags$tr(
      shiny::tags$td(status_df$Step[i]),
      shiny::tags$td(status_df$Status[i]),
      shiny::tags$td(shiny::HTML(status_df$Icon[i]))
    )
  })

  table <- shiny::tags$table(
    class = "table table-striped table-bordered",
    style = "width: 100%;",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Step"),
        shiny::tags$th("Status"),
        shiny::tags$th("Indicator")
      )
    ),
    shiny::tags$tbody(rows)
  )

  if (!is.null(title)) {
    shiny::tagList(
      shiny::tags$h4(title),
      table
    )
  } else {
    table
  }
}

#' @title Render Matrix Master Status Table
#' @description
#' Renders an HTML table showing the status of various steps across multiple modules/data types.
#' This creates a matrix where rows are steps and columns are modules/data types.
#' Used by _master modules to display completion status in a transposed format.
#'
#' @param status_matrix A data frame or matrix where:
#'   - First column is "Step" (row labels)
#'   - Remaining columns are module/data type names
#'   - Cell values are logical (TRUE/FALSE) or numeric (0/1) status indicators
#' @param title Optional title for the table.
#' @return HTML table output suitable for use in renderUI.
#' @export
render_matrix_master_status_table <- function(status_matrix, title = NULL) {
  if (is.null(status_matrix) || nrow(status_matrix) == 0) {
    return(shiny::tags$p("No status information available."))
  }

  # Get column names (first is Step, rest are data types/modules)
  col_names <- colnames(status_matrix)
  step_col <- col_names[1]
  data_cols <- col_names[-1]

  # Create header row
  header_cells <- lapply(col_names, function(name) {
    shiny::tags$th(name, style = "text-align: center;")
  })

  # Create data rows
  rows <- lapply(seq_len(nrow(status_matrix)), function(i) {
    # First cell is the step name
    cells <- list(shiny::tags$td(status_matrix[i, step_col]))

    # Remaining cells are status icons
    for (col in data_cols) {
      value <- status_matrix[i, col]
      icon <- status_icon(value)
      cells <- c(cells, list(shiny::tags$td(shiny::HTML(icon), style = "text-align: center;")))
    }

    shiny::tags$tr(cells)
  })

  table <- shiny::tags$table(
    class = "table table-striped table-bordered",
    style = "width: 100%;",
    shiny::tags$thead(
      shiny::tags$tr(header_cells)
    ),
    shiny::tags$tbody(rows)
  )

  if (!is.null(title)) {
    shiny::tagList(
      shiny::tags$h4(title),
      table
    )
  } else {
    table
  }
}

#' @title Get Data Master Status
#' @description
#' Retrieves the current status of data import steps for the master table.
#'
#' @param session Shiny session object.
#' @return Named list of logical values representing data import status.
#' @export
get_data_master_status <- function(session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  data_status <- checkbox_status$data

  list(
    household_data_imported = isTRUE(shiny::isolate(data_status$household_data_imported)),
    roster_data_imported = isTRUE(shiny::isolate(data_status$roster_data_imported)),
    mortality_data_imported = isTRUE(shiny::isolate(data_status$mortality_data_imported)),
    nutrition_data_imported = isTRUE(shiny::isolate(data_status$nutrition_data_imported)),
    woman_data_imported = isTRUE(shiny::isolate(data_status$woman_data_imported)),
    health_data_imported = isTRUE(shiny::isolate(data_status$health_data_imported)),
    water_data_imported = isTRUE(shiny::isolate(data_status$water_data_imported)),
    fsl_kii_data_imported = isTRUE(shiny::isolate(data_status$fsl_kii_data_imported)),
    health_kii_data_imported = isTRUE(shiny::isolate(data_status$health_kii_data_imported)),
    nut_kii_data_imported = isTRUE(shiny::isolate(data_status$nut_kii_data_imported)),
    wash_kii_data_imported = isTRUE(shiny::isolate(data_status$wash_kii_data_imported)),
    community_kii_data_imported = isTRUE(shiny::isolate(data_status$community_kii_data_imported)),
    community_obs_data_imported = isTRUE(shiny::isolate(data_status$community_obs_data_imported)),
    healthfacility_obs_data_imported = isTRUE(shiny::isolate(data_status$healthfacility_obs_data_imported)),
    nutfacility_obs_data_imported = isTRUE(shiny::isolate(data_status$nutfacility_obs_data_imported)),
    water_obs_data_imported = isTRUE(shiny::isolate(data_status$water_obs_data_imported)),
    latrine_obs_data_imported = isTRUE(shiny::isolate(data_status$latrine_obs_data_imported)),
    livelihoods_obs_data_imported = isTRUE(shiny::isolate(data_status$livelihoods_obs_data_imported))
  )
}

#' @title Get Cleaning Master Status
#' @description
#' Retrieves the current status of data cleaning steps for the master table.
#'
#' @param session Shiny session object.
#' @return Named list of logical values representing cleaning status.
#' @export
get_cleaning_master_status <- function(session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  cleaning_status <- checkbox_status$cleaning

  list(
    main_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$main_cleaning_complete)),
    roster_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$roster_cleaning_complete)),
    mortality_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$mortality_cleaning_complete)),
    nutrition_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$nutrition_cleaning_complete)),
    woman_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$woman_cleaning_complete)),
    health_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$health_cleaning_complete)),
    water_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$water_cleaning_complete)),
    fsl_kii_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$fsl_kii_cleaning_complete)),
    health_kii_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$health_kii_cleaning_complete)),
    nut_kii_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$nut_kii_cleaning_complete)),
    wash_kii_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$wash_kii_cleaning_complete)),
    community_kii_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$community_kii_cleaning_complete)),
    community_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$community_obs_cleaning_complete)),
    healthfacility_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$healthfacility_obs_cleaning_complete)),
    nutfacility_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$nutfacility_obs_cleaning_complete)),
    water_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$water_obs_cleaning_complete)),
    latrine_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$latrine_obs_cleaning_complete)),
    livelihoods_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$livelihoods_obs_cleaning_complete)),
    graves_obs_cleaning_complete = isTRUE(shiny::isolate(cleaning_status$graves_obs_cleaning_complete))
  )
}

#' @title Get Quality Master Status
#' @description
#' Retrieves the current status of quality check steps for the master table.
#'
#' @param session Shiny session object.
#' @return Named list of logical values representing quality check status.
#' @export
get_quality_master_status <- function(session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  quality_status <- checkbox_status$quality

  list(
    general_quality_complete = isTRUE(shiny::isolate(quality_status$general_quality_complete)),
    nutrition_quality_complete = isTRUE(shiny::isolate(quality_status$nutrition_quality_complete)),
    muac_quality_complete = isTRUE(shiny::isolate(quality_status$muac_quality_complete)),
    mortality_quality_complete = isTRUE(shiny::isolate(quality_status$mortality_quality_complete)),
    health_quality_complete = isTRUE(shiny::isolate(quality_status$health_quality_complete)),
    fsl_quality_complete = isTRUE(shiny::isolate(quality_status$fsl_quality_complete)),
    wash_quality_complete = isTRUE(shiny::isolate(quality_status$wash_quality_complete)),
    other_quality_complete = isTRUE(shiny::isolate(quality_status$other_quality_complete))
  )
}

#' @title Get Analysis Master Status
#' @description
#' Retrieves the current status of analysis steps for the master table.
#'
#' @param session Shiny session object.
#' @return Named list of logical values representing analysis status.
#' @export
get_analysis_master_status <- function(session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  analysis_status <- checkbox_status$analysis

  list(
    demographics_analysis_complete = isTRUE(shiny::isolate(analysis_status$demographics_analysis_complete)),
    nutrition_analysis_complete = isTRUE(shiny::isolate(analysis_status$nutrition_analysis_complete)),
    muac_analysis_complete = isTRUE(shiny::isolate(analysis_status$muac_analysis_complete)),
    mortality_analysis_complete = isTRUE(shiny::isolate(analysis_status$mortality_analysis_complete)),
    health_analysis_complete = isTRUE(shiny::isolate(analysis_status$health_analysis_complete)),
    fsl_analysis_complete = isTRUE(shiny::isolate(analysis_status$fsl_analysis_complete)),
    wash_analysis_complete = isTRUE(shiny::isolate(analysis_status$wash_analysis_complete)),
    other_analysis_complete = isTRUE(shiny::isolate(analysis_status$other_analysis_complete)),
    summary_analysis_complete = isTRUE(shiny::isolate(analysis_status$summary_analysis_complete))
  )
}

#' @title Get Tools Master Status
#' @description
#' Retrieves the current status of tools/DAP steps for the master table.
#'
#' @param session Shiny session object.
#' @return Named list of logical values representing tools status.
#' @export
get_tools_master_status <- function(session = shiny::getDefaultReactiveDomain()) {
  checkbox_status <- iphra_get_checkbox_status(session)
  tools_status <- checkbox_status$tools

  list(
    household_tools_complete = isTRUE(shiny::isolate(tools_status$household_tools_complete)),
    community_tools_complete = isTRUE(shiny::isolate(tools_status$community_tools_complete)),
    fsl_tools_complete = isTRUE(shiny::isolate(tools_status$fsl_tools_complete)),
    health_tools_complete = isTRUE(shiny::isolate(tools_status$health_tools_complete)),
    wash_tools_complete = isTRUE(shiny::isolate(tools_status$wash_tools_complete))
  )
}

#' @title Data Status Labels
#' @description
#' Returns display labels for data import status items.
#' @return Named list of display labels.
#' @export
data_status_labels <- function() {
  list(
    household_data_imported = iphra_txt("Household Data"),
    roster_data_imported = iphra_txt("Roster Data"),
    mortality_data_imported = iphra_txt("Mortality Data"),
    nutrition_data_imported = iphra_txt("Nutrition Data"),
    woman_data_imported = iphra_txt("Woman Data"),
    health_data_imported = iphra_txt("Health Data"),
    water_data_imported = iphra_txt("Water Data"),
    fsl_kii_data_imported = iphra_txt("FSL KII Data"),
    health_kii_data_imported = iphra_txt("Health KII Data"),
    nut_kii_data_imported = iphra_txt("Nutrition KII Data"),
    wash_kii_data_imported = iphra_txt("WASH KII Data"),
    community_kii_data_imported = iphra_txt("Community KII Data"),
    community_obs_data_imported = iphra_txt("Community Observation Data"),
    healthfacility_obs_data_imported = iphra_txt("Health Facility Observation Data"),
    nutfacility_obs_data_imported = iphra_txt("Nutrition Facility Observation Data"),
    water_obs_data_imported = iphra_txt("Water Point Observation Data"),
    latrine_obs_data_imported = iphra_txt("Latrine Observation Data"),
    livelihoods_obs_data_imported = iphra_txt("Livelihoods Observation Data")
  )
}

#' @title Cleaning Status Labels
#' @description
#' Returns display labels for cleaning status items.
#' @return Named list of display labels.
#' @export
cleaning_status_labels <- function() {
  list(
    main_cleaning_complete = iphra_txt("Main Household"),
    roster_cleaning_complete = iphra_txt("Roster"),
    mortality_cleaning_complete = iphra_txt("Mortality"),
    nutrition_cleaning_complete = iphra_txt("Nutrition"),
    woman_cleaning_complete = iphra_txt("Woman"),
    health_cleaning_complete = iphra_txt("Health"),
    water_cleaning_complete = iphra_txt("Water Container"),
    fsl_kii_cleaning_complete = iphra_txt("FSL KII"),
    health_kii_cleaning_complete = iphra_txt("Health Facility KII"),
    nut_kii_cleaning_complete = iphra_txt("Nutrition Facility KII"),
    wash_kii_cleaning_complete = iphra_txt("WASH KII"),
    community_kii_cleaning_complete = iphra_txt("Community KII"),
    community_obs_cleaning_complete = iphra_txt("Community Observation"),
    healthfacility_obs_cleaning_complete = iphra_txt("Health Facility Observation"),
    nutfacility_obs_cleaning_complete = iphra_txt("Nutrition Facility Observation"),
    water_obs_cleaning_complete = iphra_txt("Water Point Observation"),
    latrine_obs_cleaning_complete = iphra_txt("Latrine Observation"),
    livelihoods_obs_cleaning_complete = iphra_txt("Livelihoods Observation"),
    graves_obs_cleaning_complete = iphra_txt("Grave Counting")
  )
}

#' @title Quality Status Labels
#' @description
#' Returns display labels for quality check status items.
#' @return Named list of display labels.
#' @export
quality_status_labels <- function() {
  list(
    general_quality_complete = iphra_txt("General Quality Checks"),
    nutrition_quality_complete = iphra_txt("Nutrition Quality Checks"),
    muac_quality_complete = iphra_txt("MUAC Quality Checks"),
    mortality_quality_complete = iphra_txt("Mortality Quality Checks"),
    health_quality_complete = iphra_txt("Health Quality Checks"),
    fsl_quality_complete = iphra_txt("FSL Quality Checks"),
    wash_quality_complete = iphra_txt("WASH Quality Checks"),
    other_quality_complete = iphra_txt("Other Quality Checks")
  )
}

#' @title Analysis Status Labels
#' @description
#' Returns display labels for analysis status items.
#' @return Named list of display labels.
#' @export
analysis_status_labels <- function() {
  list(
    demographics_analysis_complete = iphra_txt("Demographics Analysis"),
    nutrition_analysis_complete = iphra_txt("Nutrition Analysis"),
    muac_analysis_complete = iphra_txt("MUAC Analysis"),
    mortality_analysis_complete = iphra_txt("Mortality Analysis"),
    health_analysis_complete = iphra_txt("Health Analysis"),
    fsl_analysis_complete = iphra_txt("FSL Analysis"),
    wash_analysis_complete = iphra_txt("WASH Analysis"),
    other_analysis_complete = iphra_txt("Other Analysis"),
    summary_analysis_complete = iphra_txt("Summary Analysis")
  )
}

#' @title Tools Status Labels
#' @description
#' Returns display labels for tools status items.
#' @return Named list of display labels.
#' @export
tools_status_labels <- function() {
  list(
    household_tools_complete = iphra_txt("Household Tools"),
    community_tools_complete = iphra_txt("Community Tools"),
    fsl_tools_complete = iphra_txt("FSL Tools"),
    health_tools_complete = iphra_txt("Health Tools"),
    wash_tools_complete = iphra_txt("WASH Tools")
  )
}
#' @title Build Status Matrix
#' @description
#' Creates a status matrix data frame from a status list and labels.
#' Converts the status list into a matrix format suitable for rendering
#' with render_matrix_master_status_table.
#'
#' @param status_list Named list of logical/numeric values representing status.
#' @param labels Named list of display labels for each key in status_list.
#' @param steps Character vector of step names (row labels).
#' @param step_index Integer indicating which step (row) contains the actual status data.
#'   Other rows will be filled with NA. Default is the last step.
#' @return Data frame with Step column and additional columns for each status item.
#' @export
build_status_matrix <- function(status_list, labels, steps, step_index = length(steps)) {
  # Create base matrix with steps
  status_matrix <- data.frame(
    Step = steps,
    stringsAsFactors = FALSE
  )
  
  # Add each item as a column
  for (key in names(status_list)) {
    col_name <- labels[[key]]
    # Create a vector with NA for all steps
    values <- rep(NA, length(steps))
    # Set the tracked step to the actual status
    values[step_index] <- as.numeric(status_list[[key]])
    status_matrix[[col_name]] <- values
  }
  
  status_matrix
}

#' @title Status Icon Helper
#' @description
#' Returns a Unicode symbol representing the status of a step.
#' Used internally by status table rendering functions.
#'
#' @param x Numeric or logical value. 1 or TRUE for complete, 
#'   0 or FALSE for incomplete, NA for not tracked/unknown.
#' @return Character string with Unicode symbol: 
#'   ✔ (U+2714) for complete, ❌ (U+274C) for incomplete, 
#'   ⚠ (U+26A0) for not tracked/unknown.
#' @keywords internal
status_icon <- function(x) {
  if (is.na(x)) {
    return("\u26A0")   # ⚠️
  } else if (x == 1) {
    return("\u2714")   # ✔️
  } else {
    return("\u274C")   # ❌
  }
}
