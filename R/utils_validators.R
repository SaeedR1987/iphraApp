#' @title Validate That an Object Is Not NULL or NA
#' @description
#' Checks that an object exists and is not missing. Handles all object types safely.
#'
#' @param x Object to check.
#' @param origin Optional name of the calling function for context.
#'
#' @return Invisibly TRUE if valid; otherwise throws [iphra_error()].
#' @export
iphra_validate_not_null <- function(x, origin = NULL) {
  if (is.null(x)) {
    iphra_error(
      "Received NULL input, expected a valid object.",
      origin = origin,
      hint = "Ensure the object exists before validation."
    )
  }

  # Only check for NA if x is atomic (vector, not list/data.frame)
  if (is.atomic(x) && length(x) == 1 && is.na(x)) {
    iphra_error(
      "Received NA input, expected a non-missing value.",
      origin = origin,
      hint = "Ensure missing values are handled before validation."
    )
  }

  invisible(TRUE)
}


#' @title Validate Numeric Input (Strict)
#' @description
#' Checks that `x` is strictly numeric (not logical, character, or other type).
#' Accepts vectors of numeric values.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_numeric <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (!is.numeric(x) || is.logical(x)) {
    iphra_error(
      "Expected a strictly numeric value.",
      origin = origin,
      hint = hint %||% "Ensure input is of type 'numeric'. Logical or character values are not accepted."
    )
  }
  message(">>> ENTERED iphra_validate_numeric()")
  invisible(TRUE)
}


#' @title Validate Character Input (Strict)
#' @description
#' Ensures that `x` is a character vector (not factor, numeric, or logical).
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_character <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (!is.character(x)) {
    iphra_error(
      "Expected a character input (string).",
      origin = origin,
      hint = hint %||% "Ensure the input variable is of type 'character'. Factors or numerics are not allowed."
    )
  }
  invisible(TRUE)
}


#' @title Validate Logical Input (Strict)
#' @description
#' Checks that `x` is strictly logical. Does not allow numeric 0/1 substitutes.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_logical <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (!is.logical(x)) {
    iphra_error(
      "Expected a logical (TRUE/FALSE) value.",
      origin = origin,
      hint = hint %||% "Convert numeric indicators (0/1) to TRUE/FALSE explicitly."
    )
  }
  invisible(TRUE)
}


#' @title Validate Date or Date-Time Input (Strict Validation Only)
#' @description
#' Checks whether `x` is a valid date or datetime representation that can be safely
#' converted to a standard `Date` ("YYYY-MM-DD") format.
#'
#' The function performs format recognition without returning the converted value.
#' Supported inputs include:
#' - `Date` objects
#' - `POSIXct` or `POSIXlt` (considered valid)
#' - Character strings that can be parsed into valid dates using common formats
#'   such as `"2025-10-16"`, `"16/10/2025"`, `"2025-10-16T14:32:00Z"`, etc.
#'
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint to display on failure.
#'
#' @return Invisibly returns TRUE if valid. Triggers `iphra_error()` if invalid.
#' @export
iphra_validate_date <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)

  # Accept existing Date or POSIX objects outright
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(invisible(TRUE))
  }

  # Attempt parsing if character vector
  if (is.character(x)) {
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
      "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ"
    )

    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(x, format = fmt))
      if (!any(is.na(parsed))) {
        return(invisible(TRUE))
      }
    }
  }

  # If nothing matched, raise structured error
  iphra_error(
    "Expected a valid date or datetime convertible to 'YYYY-MM-DD'.",
    origin = origin,
    hint = hint %||% "Ensure input is a Date, POSIX, or correctly formatted string (e.g. '2025-10-16')."
  )
}

# ---- Validate Factor ------------------------------------------------

#' @title Validate Factor Input
#' @description
#' Ensures that `x` is a factor variable.
#' @param x Object to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_factor <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (!is.factor(x)) {
    iphra_error(
      "Expected a factor variable.",
      origin = origin,
      hint = hint %||% "Use factor() to convert character or numeric inputs before validation."
    )
  }
  invisible(TRUE)
}

# ---- Validate Columns (new) -----------------------------------------

#' @title Validate Presence of Required Columns
#' @description
#' Checks whether specified column names exist within a data frame.
#' @param df The data frame to check.
#' @param required_cols Character vector of required column names.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if all columns are present; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_columns <- function(df, required_cols, origin = NULL, hint = NULL) {
  iphra_validate_not_null(df, origin)
  iphra_validate_not_null(required_cols, origin)
  if (!is.data.frame(df)) {
    iphra_error("Input must be a data frame for column validation.", origin = origin)
  }
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    iphra_error(
      paste0("Missing required columns: ", paste(missing, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Ensure the data frame includes all required fields."
    )
  }
  invisible(TRUE)
}

#' @title Validate Data Frame Input
#' @description
#' Checks that the provided object is a `data.frame` or tibble.
#' @param x Object to test.
#' @param origin Optional name of originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_dataframe <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (!is.data.frame(x)) {
    iphra_error(
      "Expected a data frame (or tibble).",
      origin = origin,
      hint = hint %||% "Ensure the object is created with data.frame(), tibble(), or similar."
    )
  }
  invisible(TRUE)
}

# ---- Validate List --------------------------------------------------

#' @title Validate That an Object Is a True List (Not a Data Frame)
#' @description
#' Ensures the input object is a plain list. Rejects data frames, tibbles,
#' and other list-like objects used to represent tabular data.
#'
#' @param x Object to validate.
#' @param origin Optional character string indicating where validation was called.
#' @param hint Optional hint to include in the error message for user guidance.
#'
#' @return Invisibly returns TRUE if the object is a list (and not a data frame).
#' Otherwise, triggers [iphra_error()].
#' @export
iphra_validate_list <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)

  # Reject anything that isn't a list, or that is a data.frame
  if (!is.list(x) || is.data.frame(x)) {
    iphra_error(
      "Expected a list (not a data frame or tibble).",
      origin = origin,
      hint = hint %||% "Ensure the input is a plain list structure (not a data.frame)."
    )
  }

  invisible(TRUE)
}


# ---- Validate Vector Length -----------------------------------------

#' @title Validate Vector Length
#' @description
#' Ensures a vector has a specified minimum or exact length.
#' @param x The vector to test.
#' @param min_length Minimum allowed length (default = 1).
#' @param exact_length Optional integer specifying exact expected length.
#' @param origin Optional name of the originating function.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_vector_length <- function(x, min_length = 1, exact_length = NULL, origin = NULL) {
  iphra_validate_not_null(x, origin)
  len <- length(x)
  if (!is.null(exact_length) && len != exact_length) {
    iphra_error(
      paste0("Expected vector of length ", exact_length, ", got length ", len, "."),
      origin = origin
    )
  } else if (len < min_length) {
    iphra_error(
      paste0("Expected vector of length ≥ ", min_length, ", got ", len, "."),
      origin = origin
    )
  }
  invisible(TRUE)
}

# ---- Validate Choice Membership -------------------------------------

#' @title Validate Choice Membership
#' @description
#' Ensures that the input value(s) belong to a defined set of allowed options.
#' @param x Value or vector to test.
#' @param choices Character vector of allowed values.
#' @param origin Optional name of the originating function.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_choice <- function(x, choices, origin = NULL) {
  iphra_validate_not_null(x, origin)
  iphra_validate_not_null(choices, origin)
  bad <- setdiff(x, choices)
  if (length(bad) > 0) {
    iphra_error(
      paste0("Invalid choice(s): ", paste(bad, collapse = ", ")),
      origin = origin,
      hint = paste0("Allowed values are: ", paste(choices, collapse = ", "))
    )
  }
  invisible(TRUE)
}

# ---- Utility: Internal coercion check --------------------------------

.is_safely_coercible <- function(x, to_type) {
  # returns TRUE if all elements of x can be safely coerced to the target type
  suppressWarnings({
    if (to_type == "numeric") {
      coerced <- suppressWarnings(as.numeric(x))
      return(!any(is.na(coerced) & !is.na(x)))
    } else if (to_type == "character") {
      coerced <- suppressWarnings(as.character(x))
      return(TRUE)  # as.character() never fails
    } else if (to_type == "logical") {
      valid_vals <- c("TRUE", "FALSE", "T", "F", "1", "0")
      return(all(toupper(as.character(x)) %in% valid_vals | is.na(x)))
    } else if (to_type == "Date") {
      formats <- c(
        "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d%m%Y", "%m%d%Y",
        "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %I:%M:%S %p",
        "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
        "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M",
        "%Y-%m", "%m/%Y"
      )

      # Check if all elements can be parsed by at least one format
      is_convertible <- vapply(x, function(val) {
        any(!is.na(sapply(formats, function(fmt) {
          suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
        })))
      }, logical(1))

      return(all(is_convertible))

    } else if (to_type == "factor") {
      return(is.character(x) || is.factor(x))
    } else {
      return(FALSE)
    }
  })
}


# ---- All Numeric ----------------------------------------------------

#' @title Validate Entire Vector is Numeric or Coercible to Numeric
#' @description
#' Checks that all values in a vector (or column) are numeric or can be safely
#' converted to numeric without introducing NAs.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_all_numeric <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (is.numeric(x)) return(invisible(TRUE))
  if (.is_safely_coercible(x, "numeric")) return(invisible(TRUE))
  iphra_error(
    "Expected all values to be numeric or safely coercible to numeric.",
    origin = origin,
    hint = hint %||% "Ensure values contain only digits and valid numeric strings."
  )
}

# ---- All Character ----------------------------------------------------

#' @title Validate Entire Vector is Character or Coercible to Character
#' @description
#' Checks that all values in a vector (or column) are character strings or can be
#' represented as character safely. Optionally enforces that all values belong to a
#' specified set of allowable options.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_values Optional character vector of allowed values. If provided,
#'   validation will fail if any element of `x` is not found within this set.
#' @param origin Optional name of the originating function (for tracing errors).
#' @param hint Optional corrective hint.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_all_character <- function(x, allowed_values = NULL, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)

  # Type or coercion check
  if (!is.character(x) && !.is_safely_coercible(x, "character")) {
    iphra_error(
      "Expected all values to be character or coercible to character.",
      origin = origin,
      hint = hint %||% "Use as.character() or convert factors/integers before passing."
    )
  }

  # Allowed values check
  if (!is.null(allowed_values)) {
    # Coerce to character for comparison safety
    x_chr <- as.character(x)
    invalid <- setdiff(unique(x_chr), allowed_values)
    if (length(invalid) > 0) {
      iphra_error(
        paste0(
          "Invalid values detected: ", paste(invalid, collapse = ", "),
          ". Expected values: ", paste(allowed_values, collapse = ", ")
        ),
        origin = origin,
        hint = hint %||% "Ensure all character entries match allowed options."
      )
    }
  }

  invisible(TRUE)
}


# ---- All Logical ----------------------------------------------------

#' @title Validate Entire Vector is Logical or Coercible to Logical
#' @description
#' Checks that all values in a vector are logical (TRUE/FALSE) or coercible from
#' standard encodings such as "TRUE"/"FALSE", "T"/"F", or 0/1.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_all_logical <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (is.logical(x)) return(invisible(TRUE))
  if (.is_safely_coercible(x, "logical")) return(invisible(TRUE))
  iphra_error(
    "Expected all values to be logical (TRUE/FALSE) or safely coercible.",
    origin = origin,
    hint = hint %||% "Ensure inputs are TRUE/FALSE, 1/0, or equivalent character codes."
  )
}


# ---- All Date -------------------------------------------------------

#' @title Validate Entire Vector is Date or Coercible to Date
#' @description
#' Checks that all values in a vector (or column) are Date, POSIX, or can be safely
#' converted to a Date representation using common formats.
#' @param x Vector or data frame column to test.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_all_date <- function(x, origin = NULL, hint = NULL) {
  iphra_validate_not_null(x, origin)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(invisible(TRUE))
  if (.is_safely_coercible(x, "Date")) return(invisible(TRUE))
  iphra_error(
    "Expected all values to be Date, POSIX, or coercible to Date format.",
    origin = origin,
    hint = hint %||% "Ensure date strings follow standard formats like 'YYYY-MM-DD' or 'DD/MM/YYYY'."
  )
}

# ---- All Factor -----------------------------------------------------

#' @title Validate Entire Vector is Factor or Coercible to Factor
#' @description
#' Checks that all values are factors or coercible (e.g., from character strings).
#' Optionally enforces that all factor levels belong to a specified set of
#' allowed levels, and optionally verifies that the factor order matches an
#' expected ordering.
#'
#' @param x Vector or data frame column to test.
#' @param allowed_levels Optional character vector specifying the allowed factor levels.
#'   Validation fails if any observed level is not in this set.
#' @param expected_order Optional character vector specifying the correct ordering
#'   of factor levels. If provided, validation fails if the factor's level order
#'   does not exactly match.
#' @param origin Optional name of the originating function.
#' @param hint Optional corrective hint.
#'
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
#' @title Validate That All Values Are Factors
#' @description
#' Validates that all values in a vector are factor type.
#' Optionally checks for allowed levels and correct ordering.
#' Strict: will not coerce from character or numeric values.
#'
#' @param x Vector to validate.
#' @param allowed_levels Optional character vector of valid factor levels.
#' @param expected_order Optional character vector specifying expected order of levels.
#' @param origin Optional name of the function or module calling this validator.
#' @param hint Optional suggestion or guidance for fixing detected issues.
#'
#' @return Invisibly returns TRUE if valid; otherwise throws an [iphra_error()] or [iphra_warning()].
#' @export
iphra_validate_all_factor <- function(
    x,
    allowed_levels = NULL,
    expected_order = NULL,
    origin = NULL,
    hint = NULL
) {
  iphra_validate_not_null(x, origin)

  # ---- Type check ----
  if (!is.factor(x)) {
    iphra_error(
      "Expected a factor vector, but received a non-factor type.",
      origin = origin,
      hint = hint %||% "Convert variable to factor before validation."
    )
  }

  # ---- Check allowed levels ----
  if (!is.null(allowed_levels)) {
    levels_present <- levels(x)
    invalid <- setdiff(levels_present, allowed_levels)
    if (length(invalid) > 0) {
      iphra_error(
        paste0(
          "Invalid factor levels detected: ", paste(invalid, collapse = ", "),
          ". Allowed levels are: ", paste(allowed_levels, collapse = ", ")
        ),
        origin = origin,
        hint = hint %||% "Check factor level definitions or category spelling."
      )
    }
  }

  # ---- Check expected order ----
  if (!is.null(expected_order)) {
    actual_order <- levels(x)
    if (!identical(actual_order, expected_order)) {
      iphra_error(
        paste0(
          "Incorrect factor level order. Actual: ",
          paste(actual_order, collapse = " > "),
          ". Expected: ",
          paste(expected_order, collapse = " > ")
        ),
        origin = origin,
        hint = hint %||% "Ensure factor levels are defined in the correct order."
      )
    }
  }

  invisible(TRUE)
}


#' @title Validate Column Types
#' @description
#' Ensures specified columns in a data frame match expected R classes.
#' @param df Data frame to validate.
#' @param expected_types Named list of expected types, e.g. `list(age = "numeric", name = "character")`.
#' @param origin Optional name of the calling function.
#' @param hint Optional hint for correction.
#' @return Invisibly returns TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_column_types <- function(df, expected_types, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  missing <- setdiff(names(expected_types), names(df))
  if (length(missing) > 0) {
    iphra_error(
      paste("Missing expected columns:", paste(missing, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Ensure all required columns are present."
    )
  }
  for (col in names(expected_types)) {
    actual <- class(df[[col]])[1]
    expected <- expected_types[[col]]
    if (actual != expected) {
      iphra_error(
        paste0("Column '", col, "' is of type '", actual, "' but expected '", expected, "'."),
        origin = origin,
        hint = hint %||% "Check data import or preprocessing steps."
      )
    }
  }
  invisible(TRUE)
}


#' @title Validate No Missing Values
#' @description
#' Checks that specified columns contain no missing (NA) values.
#' @param df Data frame to test.
#' @param cols Character vector of column names to check.
#' @param origin Optional function name.
#' @param hint Optional correction hint.
#' @return Invisibly TRUE if valid, otherwise triggers `iphra_error()`.
#' @export
iphra_validate_no_missing <- function(df, cols, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  for (col in cols) {
    if (any(is.na(df[[col]]))) {
      iphra_error(
        paste0("Column '", col, "' contains missing (NA) values."),
        origin = origin,
        hint = hint %||% "Fill or remove missing data before proceeding."
      )
    }
  }
  invisible(TRUE)
}


#' @title Validate Column Uniqueness
#' @description
#' Ensures one or more columns (or their combination) form a unique identifier.
#' @param df Data frame to validate.
#' @param cols Character vector of columns whose combination must be unique.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_unique <- function(df, cols, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  dupes <- duplicated(df[cols])
  if (any(dupes)) {
    iphra_error(
      paste("Duplicate entries detected for key columns:", paste(cols, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Ensure unique identifiers or combination keys."
    )
  }
  invisible(TRUE)
}


#' @title Validate Non-Negative Values
#' @description
#' Ensures numeric columns have no negative values.
#' @param df Data frame to validate.
#' @param cols Character vector of numeric columns to check.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_non_negative <- function(df, cols, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  for (col in cols) {
    if (any(df[[col]] < 0, na.rm = TRUE)) {
      iphra_error(
        paste0("Column '", col, "' contains negative values."),
        origin = origin,
        hint = hint %||% "Ensure counts or quantities are zero or positive."
      )
    }
  }
  invisible(TRUE)
}


#' @title Validate Range of Numeric Values
#' @description
#' Checks that values in a numeric column fall within a specified inclusive range.
#' @param df Data frame to validate.
#' @param col Column name to check.
#' @param min Minimum acceptable value.
#' @param max Maximum acceptable value.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_range <- function(df, col, min, max, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  vals <- df[[col]]
  out_of_range <- vals < min | vals > max
  if (any(out_of_range, na.rm = TRUE)) {
    iphra_error(
      paste0("Values in '", col, "' are out of range [", min, ", ", max, "]."),
      origin = origin,
      hint = hint %||% "Check data entry or range filters."
    )
  }
  invisible(TRUE)
}


#' @title Validate Pattern Match for Character Columns
#' @description
#' Ensures all non-missing values in a character column match a regular expression.
#' @param df Data frame to validate.
#' @param col Character column name.
#' @param pattern Regular expression pattern (e.g., `"^[0-9]{10}$"`).
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_pattern <- function(df, col, pattern, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  vals <- df[[col]]
  bad <- !grepl(pattern, vals[!is.na(vals)])
  if (any(bad)) {
    iphra_error(
      paste0("Some values in '", col, "' do not match the required pattern: ", pattern),
      origin = origin,
      hint = hint %||% "Verify field formatting and encoding."
    )
  }
  invisible(TRUE)
}


#' @title Validate Date Order
#' @description
#' Ensures that all rows have start_date ≤ end_date.
#' @param df Data frame containing the two date columns.
#' @param start_col Name of the start date column.
#' @param end_col Name of the end date column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_date_order <- function(df, start_col, end_col, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  if (any(df[[start_col]] > df[[end_col]], na.rm = TRUE)) {
    iphra_error(
      paste0("Start date in column '", start_col, "' exceeds end date in '", end_col, "'."),
      origin = origin,
      hint = hint %||% "Ensure start and end dates are in logical order."
    )
  }
  invisible(TRUE)
}


#' @title Validate Column Dependency
#' @description
#' Ensures if one column has a non-missing value, another dependent column is also filled.
#' @param df Data frame to test.
#' @param col_a Independent column (trigger).
#' @param col_b Dependent column (must be filled if A is non-missing).
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_dependency <- function(df, col_a, col_b, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  missing_dep <- !is.na(df[[col_a]]) & is.na(df[[col_b]])
  if (any(missing_dep)) {
    iphra_error(
      paste0("Rows where '", col_a, "' is filled but '", col_b, "' is missing."),
      origin = origin,
      hint = hint %||% paste0("Ensure '", col_b, "' is provided when '", col_a, "' is non-missing.")
    )
  }
  invisible(TRUE)
}


#' @title Validate Mutually Exclusive Columns
#' @description
#' Ensures only one of several related indicator columns is TRUE (or 1) per row.
#' @param df Data frame to test.
#' @param cols Character vector of logical or numeric indicator columns.
#' @param origin Optional origin.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_mutually_exclusive <- function(df, cols, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  count_true <- rowSums(df[cols] == TRUE | df[cols] == 1, na.rm = TRUE)
  if (any(count_true > 1)) {
    iphra_error(
      paste0("Multiple TRUE/1 values found across mutually exclusive columns: ", paste(cols, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Ensure only one column is TRUE per record."
    )
  }
  invisible(TRUE)
}


#' @title Validate No Duplicate Rows
#' @description
#' Ensures there are no fully duplicated rows in the data frame.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_no_duplicates <- function(df, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  if (any(duplicated(df))) {
    iphra_error(
      "Duplicate rows detected in the dataset.",
      origin = origin,
      hint = hint %||% "Use distinct() or unique() to remove duplicates."
    )
  }
  invisible(TRUE)
}


#' @title Validate No Constant Columns
#' @description
#' Ensures no column in a data frame has the same value for all rows.
#' @param df Data frame to check.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_no_constant_columns <- function(df, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  const_cols <- names(df)[sapply(df, function(x) length(unique(na.omit(x))) <= 1)]
  if (length(const_cols) > 0) {
    iphra_error(
      paste0("Constant (uninformative) columns detected: ", paste(const_cols, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Consider removing or reviewing these columns."
    )
  }
  invisible(TRUE)
}


#' @title Validate Reference Values
#' @description
#' Ensures all entries in a column exist within a reference vector or table.
#' @param df Data frame to check.
#' @param col Column name.
#' @param ref_values Vector of reference or lookup values.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_reference_values <- function(df, col, ref_values, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  invalid <- setdiff(unique(df[[col]]), ref_values)
  if (length(invalid) > 0) {
    iphra_error(
      paste0("Values in '", col, "' not found in reference list: ", paste(invalid, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Check code lists or join keys for consistency."
    )
  }
  invisible(TRUE)
}


#' @title Validate Join Keys
#' @description
#' Ensures all key values from one data frame exist in a reference data frame before joining.
#' @param df Data frame containing key column to check.
#' @param ref_df Reference data frame containing valid key column.
#' @param key_col Name of the key column.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_join_keys <- function(df, ref_df, key_col, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  iphra_validate_dataframe(ref_df, origin)
  missing_keys <- setdiff(unique(df[[key_col]]), unique(ref_df[[key_col]]))
  if (length(missing_keys) > 0) {
    iphra_error(
      paste0("Join key values in '", key_col, "' not found in reference data: ", paste(missing_keys, collapse = ", ")),
      origin = origin,
      hint = hint %||% "Check for mismatched or missing join identifiers."
    )
  }
  invisible(TRUE)
}


#' @title Validate Full Data Frame Schema
#' @description
#' Validates a data frame against a declarative schema specification.
#' Each element of the schema list can include:
#'   - `type`: Expected R class (e.g. `"numeric"`, `"character"`, `"Date"`)
#'   - `allowed_values`: Optional allowed set
#'   - `range`: Optional numeric vector of length 2 for min/max
#'
#' @param df Data frame to validate.
#' @param schema Named list specifying expected structure.
#' @param origin Optional origin function.
#' @param hint Optional hint.
#' @return Invisibly TRUE if valid; otherwise triggers `iphra_error()`.
#' @export
iphra_validate_schema <- function(df, schema, origin = NULL, hint = NULL) {
  iphra_validate_dataframe(df, origin)
  for (col in names(schema)) {
    if (!col %in% names(df)) {
      iphra_error(paste("Missing expected column:", col), origin = origin)
    }
    spec <- schema[[col]]
    if (!is.null(spec$type)) {
      iphra_validate_column_types(df[, col, drop = FALSE], setNames(list(spec$type), col), origin = origin)
    }
    if (!is.null(spec$allowed_values)) {
      iphra_validate_all_character(df[[col]], allowed_values = spec$allowed_values, origin = origin)
    }
    if (!is.null(spec$range) && length(spec$range) == 2 && is.numeric(df[[col]])) {
      iphra_validate_range(df, col, spec$range[1], spec$range[2], origin = origin)
    }
  }
  invisible(TRUE)
}

