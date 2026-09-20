# ---- Table Utility Functions ----
# Provides robust helper functions for rhandsontable operations including:
# - Row insertion/removal within groups while preserving merged cells
# - Computing merge specifications for Handsontable
# - Summary value caching and restoration

# ---- Constants for internal use ----
# Used to handle NA and empty values uniquely in group merging
.MERGE_NA_PREFIX <- "__NA__"
.MERGE_EMPTY_PREFIX <- "__EMPTY__"

# --- Safe insertion helper ---
#' Insert a row after the last row of a group in a data frame
#'
#' @description
#' Safely inserts a new row into a data frame after the last occurrence of a
#' specified group value. Handles edge cases including empty data frames,
#' non-existent groups, and column mismatches.
#'
#' @param df A data frame.
#' @param group_value The value in the group column to insert after.
#' @param new_row A single-row data frame to insert.
#' @param group_col Name of the column containing group values (default: "Dimension").
#'
#' @return A data frame with the new row inserted.
#' @keywords internal
safe_insert_after_group <- function(df, group_value, new_row, group_col = "Dimension") {
  # Input validation
 if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (!is.data.frame(new_row)) {
    stop("Input 'new_row' must be a data frame.")
  }
  if (!group_col %in% names(df)) {
    stop(sprintf("Group column '%s' not found in data frame.", group_col))
  }
  if (nrow(new_row) != 1) {
    stop("new_row must contain exactly one row.")
  }

  # Handle column alignment - ensure new_row has same columns as df
  missing_cols <- setdiff(names(df), names(new_row))
  extra_cols <- setdiff(names(new_row), names(df))

  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      new_row[[col]] <- if (is.numeric(df[[col]])) NA_real_ else ""
    }
  }

  # Align column order and drop extra columns
  new_row <- new_row[, names(df), drop = FALSE]

  # Find insertion point
  idx <- which(df[[group_col]] == group_value)

  # If group not found, append at end
  if (length(idx) == 0) {
    result <- rbind(df, new_row)
    rownames(result) <- NULL
    return(as.data.frame(result, stringsAsFactors = FALSE))
  }

  last_index <- max(idx)
  n <- nrow(df)

  # If last row of group is also last row of df, simply append
  if (last_index >= n) {
    result <- rbind(df, new_row)
    rownames(result) <- NULL
    return(as.data.frame(result, stringsAsFactors = FALSE))
  }

  # Insert between rows
  top <- df[seq_len(last_index), , drop = FALSE]
  bottom <- df[seq(last_index + 1L, n), , drop = FALSE]

  result <- rbind(top, new_row, bottom)
  rownames(result) <- NULL
  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Compute cell merge specifications for Handsontable
#'
#' @description
#' Computes merge cell specifications for rhandsontable based on consecutive
#' groups in a data frame. Uses run-length encoding (RLE) to identify
#' consecutive rows with the same group value and generates merge specs
#' for both the group column and any additional specified columns.
#'
#' @param df A data frame with grouped data.
#' @param group_col Name of the column containing group values (default: "Dimension").
#' @param merge_cols Character vector of additional column names to merge within groups
#'   (default: "Summary"). Columns not found in df are silently ignored.
#'
#' @return JSON string containing merge specifications compatible with
#'   Handsontable's mergeCells option.
#' @keywords internal
compute_merges <- function(df, group_col = "Dimension", merge_cols = "Summary") {
  # Handle empty data frame
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(jsonlite::toJSON(list(), auto_unbox = TRUE))
  }

  # Validate group column exists
  if (!group_col %in% names(df)) {
    stop(sprintf("Group column '%s' not found in data frame.", group_col))
  }

  # Filter merge_cols to only those present in df
  merge_cols <- merge_cols[merge_cols %in% names(df)]

  # Handle case where no mergeable columns exist
  if (length(merge_cols) == 0) {
    # Still merge the group column itself
    merge_cols <- character(0)
  }

  # Precompute column indexes (0-based for JavaScript)
  group_col0 <- as.integer(which(names(df) == group_col) - 1L)
  merge_col0 <- if (length(merge_cols) > 0) {
    as.integer(which(names(df) %in% merge_cols) - 1L)
  } else {
    integer(0)
  }

  # Handle NA/empty values in group column - treat them as unique
  group_values <- df[[group_col]]
  na_count <- sum(is.na(group_values))
  empty_count <- sum(group_values == "", na.rm = TRUE)
  if (na_count > 0) {
    group_values[is.na(group_values)] <- paste0(.MERGE_NA_PREFIX, seq_len(na_count))
  }
  if (empty_count > 0) {
    group_values[group_values == ""] <- paste0(.MERGE_EMPTY_PREFIX, seq_len(empty_count))
  }

  # RLE grouping to find consecutive runs
  runs <- rle(group_values)
  ends <- cumsum(runs$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)

  # Pre-allocate merge list
  num_merges <- length(starts) * (1L + length(merge_col0))
  merges <- vector("list", num_merges)
  k <- 1L

  for (i in seq_along(starts)) {
    s <- starts[i]
    e <- ends[i]
    rowspan <- e - s + 1L
    row0 <- as.integer(s - 1L)

    # Merge group column itself
    merges[[k]] <- list(row = row0, col = group_col0, rowspan = rowspan, colspan = 1L)
    k <- k + 1L

    # Merge additional columns
    for (mc in merge_col0) {
      merges[[k]] <- list(row = row0, col = mc, rowspan = rowspan, colspan = 1L)
      k <- k + 1L
    }
  }

  jsonlite::toJSON(merges, auto_unbox = TRUE)
}

#' Get row indices for each group in a data frame
#'
#' @description
#' Identifies the row indices belonging to each group based on consecutive
#' runs of the same value in the group column. Returns a named list where
#' names are group values and values are integer vectors of row indices.
#'
#' @param df A data frame with a group column.
#' @param group_col Name of the column containing group values (default: "Dimension").
#'
#' @return A named list mapping group values to their row indices.
#' @keywords internal
get_group_rows <- function(df, group_col = "Dimension") {
  if (!is.data.frame(df) || nrow(df) == 0) return(list())

  if (!group_col %in% names(df)) {
    stop(sprintf("Group column '%s' not found in data frame.", group_col))
  }

  runs <- rle(df[[group_col]])
  ends <- cumsum(runs$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)
  stats::setNames(Map(function(a, b) a:b, starts, ends), runs$values)
}

#' Restore cached summary values to a data frame
#'
#' @description
#' Restores cached summary values to the first row of each group in a data frame.
#' This is used to preserve user-entered summary text when rows are added or
#' removed from groups in a rhandsontable.
#'
#' @param rv A reactiveValues object containing a `df` element (data frame).
#' @param summary_cache A reactiveValues or list object containing cached summary
#'   values keyed by group dimension name.
#' @param group_col Name of the column containing group values (default: "Dimension").
#' @param summary_col Name of the column containing summary text (default: "Summary").
#'
#' @return The rv object (modified in place for reactiveValues).
#' @keywords internal
restore_cached_summaries <- function(rv, summary_cache, group_col = "Dimension",
                                     summary_col = "Summary") {
  df <- rv$df

  if (!is.data.frame(df) || nrow(df) == 0) return(rv)
  if (!group_col %in% names(df)) return(rv)
  if (!summary_col %in% names(df)) return(rv)

  changed <- FALSE

  for (dim in names(summary_cache)) {
    val <- summary_cache[[dim]]
    if (is.null(val) || val == "") next
    idx <- which(df[[group_col]] == dim)
    if (length(idx)) {
      top <- idx[1]
      current_val <- df[[summary_col]][top]
      if (is.na(current_val) || current_val == "") {
        df[[summary_col]][top] <- val
        changed <- TRUE
      }
    }
  }

  if (changed) rv$df <- df
  return(rv)
}

#' Add a row to a specific group in a rhandsontable data frame
#'
#' @description
#' Adds a new blank row to a specific group in a data frame used with rhandsontable.
#' Handles merged cells by properly positioning the new row at the end of the group
#' and preserving summary/synthesis values through caching.
#'
#' @param rv A reactiveValues object containing a `df` element (data frame).
#' @param group_value The value identifying which group to add a row to.
#' @param summary_cache A reactiveValues object for caching summary text.
#' @param suspend_hot A reactiveVal function to suspend table updates during modification.
#' @param restore_cached_summaries A function to restore cached summaries after modification.
#'   (Alias: restore_cached_summaries_fn for backwards compatibility)
#' @param session The Shiny session object.
#' @param redraw_trigger A reactiveVal to trigger table redraw.
#' @param group_col Name of the column containing group values (default: "Dimension").
#' @param summary_cols Character vector of columns containing summary text that
#'   should be preserved (default: c("Summary", "Synthesis")).
#'
#' @return The rv object (modified in place for reactiveValues).
#' @keywords internal
add_row <- function(rv,
                    group_value,
                    summary_cache,
                    suspend_hot,
                    restore_cached_summaries = NULL,
                    session,
                    redraw_trigger,
                    group_col = "Dimension",
                    summary_cols = c("Summary", "Synthesis"),
                    restore_cached_summaries_fn = NULL) {

  # Backwards compatibility: accept either parameter name
  restore_fn <- restore_cached_summaries %||% restore_cached_summaries_fn

  df <- rv$df
  all_cols <- names(df)

  # Validate inputs
  if (!is.data.frame(df)) {
    stop("rv$df must be a data frame.")
  }
  if (!group_col %in% all_cols) {
    stop(sprintf("Group column '%s' not found.", group_col))
  }

  # Find which summary columns exist in df
  valid_summary_cols <- summary_cols[summary_cols %in% all_cols]

  # Cache current summary value(s) before modification
  summary_col <- if (length(valid_summary_cols) > 0) valid_summary_cols[1] else NULL
  summary_val <- ""

  if (!is.null(summary_col)) {
    tryCatch({
      group_rows <- which(df[[group_col]] == group_value)
      if (length(group_rows) > 0) {
        val <- df[[summary_col]][group_rows[1]]
        summary_val <- if (length(val) == 0 || is.na(val)) "" else val
      }
    }, error = function(e) {
      # Silently continue with empty summary
    })
  }

  # Build a blank new row dynamically matching all columns
  new_row <- as.data.frame(
    lapply(all_cols, function(col_name) {
      col_data <- df[[col_name]]
      if (col_name == group_col) {
        return(group_value)
      } else if (is.numeric(col_data)) {
        return(NA_real_)
      } else if (is.logical(col_data)) {
        return(NA)
      } else if (inherits(col_data, "Date")) {
        return(as.Date(NA))
      } else {
        return("")
      }
    }),
    stringsAsFactors = FALSE
  )
  names(new_row) <- all_cols

  # Suspend table updates during modification
  suspend_hot(TRUE)

  # Insert the new row
  rv$df <- safe_insert_after_group(df, group_value, new_row, group_col = group_col)

  # Restore cached summaries if applicable
  if (!is.null(summary_col)) {
    top_idx <- which(rv$df[[group_col]] == group_value)[1]
    if (length(top_idx) == 1) {
      # First try the cache, then the original value
      cache_val <- summary_cache[[group_value]]
      if (!is.null(cache_val) && is.character(cache_val) && nzchar(cache_val)) {
        rv$df[[summary_col]][top_idx] <- cache_val
      } else if (is.character(summary_val) && nzchar(summary_val)) {
        rv$df[[summary_col]][top_idx] <- summary_val
      }
    }
  }

  # Call restore function if provided
  if (is.function(restore_fn)) {
    rv <- restore_fn(rv, summary_cache)
  }

  # Schedule re-enabling table updates after Shiny flushes
  session$onFlushed(function() suspend_hot(FALSE), once = TRUE)

  # Trigger redraw
  redraw_trigger(shiny::isolate(redraw_trigger()) + 1)

  invisible(rv)
}

#' Remove a row from a specific group in a rhandsontable data frame
#'
#' @description
#' Removes the last row from a specific group in a data frame used with rhandsontable.
#' Will not remove the last remaining row in a group (minimum one row per group).
#' Handles merged cells by preserving summary/synthesis values through caching.
#'
#' @param rv A reactiveValues object containing a `df` element (data frame).
#' @param group_value The value identifying which group to remove a row from.
#' @param summary_cache A reactiveValues object for caching summary text.
#' @param suspend_hot A reactiveVal function to suspend table updates during modification.
#' @param restore_cached_summaries A function to restore cached summaries after modification.
#'   (Alias: restore_cached_summaries_fn for backwards compatibility)
#' @param session The Shiny session object.
#' @param redraw_trigger A reactiveVal to trigger table redraw.
#' @param group_col Name of the column containing group values (default: "Dimension").
#' @param summary_cols Character vector of columns containing summary text that
#'   should be preserved (default: c("Summary", "Synthesis")).
#'
#' @return The rv object (modified in place for reactiveValues).
#' @keywords internal
remove_row <- function(rv,
                       group_value,
                       summary_cache,
                       suspend_hot,
                       restore_cached_summaries = NULL,
                       session,
                       redraw_trigger,
                       group_col = "Dimension",
                       summary_cols = c("Summary", "Synthesis"),
                       restore_cached_summaries_fn = NULL) {

  # Backwards compatibility: accept either parameter name
  restore_fn <- restore_cached_summaries %||% restore_cached_summaries_fn

  df <- rv$df

  # Validate inputs
  if (!is.data.frame(df)) {
    stop("rv$df must be a data frame.")
  }
  if (!group_col %in% names(df)) {
    stop(sprintf("Group column '%s' not found.", group_col))
  }

  idx <- which(df[[group_col]] == group_value)

  # Don't remove if only one row remains for this group
  if (length(idx) <= 1) {
    # Log skip only if verbose/debug logging is enabled
    if (isTRUE(getOption("iphRa.verbose", FALSE))) {
      message(sprintf("[remove_row] Skipped - only one row remains for '%s'", group_value))
    }
    return(invisible(rv))
  }

  # Remove the last row in the group
  last_idx <- idx[length(idx)]
  df <- df[-last_idx, , drop = FALSE]
  rownames(df) <- NULL

  # Suspend table updates during modification
  suspend_hot(TRUE)

  rv$df <- df

  # Call restore function if provided
  if (is.function(restore_fn)) {
    rv <- restore_fn(rv, summary_cache)
  }

  # Schedule re-enabling table updates after Shiny flushes
  session$onFlushed(function() suspend_hot(FALSE), once = TRUE)

  # Trigger redraw
  redraw_trigger(shiny::isolate(redraw_trigger()) + 1)

  invisible(rv)
}

#' Add rows to a rhandsontable-compatible data frame
#'
#' @description
#' Safely adds a specified number of rows to a data frame backing a rhandsontable.
#' Rows can be added at a specific index or appended to the end (default).
#' New rows are initialized with appropriate default values based on column types.
#'
#' @param data A data frame compatible with rhandsontable.
#' @param n Integer. Number of new rows to add (default = 1).
#' @param at Integer. Optional row index position at which to insert the new rows.
#'   If NULL or greater than nrow(data), rows are appended at the end.
#'   If <= 0, rows are prepended at the beginning.
#'
#' @return A data frame with the new rows added.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ID = 1:3, Name = c("A", "B", "C"))
#' add_rows_to_rhot(df, n = 2)
#' }
#'
#' @keywords internal
add_rows_to_rhot <- function(data, n = 1, at = NULL) {
  # Input validation
  if (is.null(data)) {
    stop("Input data must not be NULL.")
  }
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1 || !is.finite(n)) {
    stop("Number of rows to add must be a positive integer.")
  }
  n <- as.integer(n)

  # Create blank rows matching existing structure
  blank_row <- as.data.frame(
    lapply(data, function(col) {
      if (is.numeric(col)) {
        return(NA_real_)
      } else if (is.logical(col)) {
        return(NA)
      } else if (is.integer(col)) {
        return(NA_integer_)
      } else if (inherits(col, "Date")) {
        return(as.Date(NA))
      } else if (inherits(col, "POSIXt")) {
        return(as.POSIXct(NA))
      } else if (is.factor(col)) {
        return(factor(NA, levels = levels(col)))
      } else {
        return("")
      }
    }),
    stringsAsFactors = FALSE
  )

  new_rows <- blank_row[rep(1, n), , drop = FALSE]
  rownames(new_rows) <- NULL

  # Determine insertion point and combine
  total_rows <- nrow(data)

  if (is.null(at) || at > total_rows) {
    # Append at end
    combined <- rbind(data, new_rows)
  } else if (at <= 0) {
    # Prepend at beginning
    combined <- rbind(new_rows, data)
  } else {
    # Insert at specified position
    at <- as.integer(at)
    top_part <- data[seq_len(at), , drop = FALSE]
    bottom_part <- if (at < total_rows) data[seq(at + 1L, total_rows), , drop = FALSE] else data[0, , drop = FALSE]
    combined <- rbind(top_part, new_rows, bottom_part)
  }

  rownames(combined) <- NULL

  # Reindex IDs if applicable
  if ("ID" %in% names(combined)) {
    combined$ID <- seq_len(nrow(combined))
  }

  return(combined)
}

#' Remove rows from a rhandsontable-compatible data frame
#'
#' @description
#' Safely removes a specified number of rows from a data frame backing a rhandsontable.
#' Can remove from a specific row index or from the end (default).
#' Will not remove more rows than available (silently limits to available rows).
#'
#' @param data A data frame compatible with rhandsontable.
#' @param at Integer. Row index to start removing from (default = last row).
#'   If NULL, removes from the last row. If out of bounds, it's adjusted to valid range.
#' @param n Integer. Number of rows to remove (default = 1).
#'   Will not remove more rows than available.
#'
#' @return A data frame with the specified rows removed. Returns empty data frame
#'   with preserved column structure if all rows are removed.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ID = 1:5, Name = letters[1:5])
#' remove_rows_to_rhot(df, n = 2)
#' }
#'
#' @keywords internal
remove_rows_to_rhot <- function(data, at = NULL, n = 1) {
  # Input validation
  if (is.null(data)) {
    stop("Input data must not be NULL.")
  }
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1 || !is.finite(n)) {
    stop("Number of rows to remove must be a positive integer.")
  }
  n <- as.integer(n)

  total_rows <- nrow(data)

  # Handle empty data frame
  if (total_rows == 0) {
    return(data)
  }

  # Default to removing from last row if not specified
  if (is.null(at)) {
    at <- total_rows
  }

  # Clamp 'at' to valid range
  at <- as.integer(at)
  if (at > total_rows) at <- total_rows
  if (at <= 0) at <- 1L

  # Calculate indices to remove (don't go beyond data frame bounds)
  remove_indices <- seq(from = at, length.out = min(n, total_rows - at + 1L))
  remove_indices <- remove_indices[remove_indices >= 1L & remove_indices <= total_rows]

  if (length(remove_indices) == 0) {
    return(data)
  }

  remaining <- data[-remove_indices, , drop = FALSE]
  rownames(remaining) <- NULL

  # Reindex IDs if applicable
  if ("ID" %in% names(remaining) && nrow(remaining) > 0) {
    remaining$ID <- seq_len(nrow(remaining))
  }

  return(remaining)
}
