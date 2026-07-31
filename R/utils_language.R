# ────────────────────────────────────────────────
# IPHRA Text Translation / Localization Utility
# ────────────────────────────────────────────────

# ---- Load Translations from JSON Files ----
#' Load translation dictionaries from JSON files in inst/app/www/i18n/
#' @noRd
iphra_load_translations <- function() {
  translations <- list()

  # Try to load from installed package location first, then development location
  i18n_paths <- c(
    system.file("app/www/i18n", package = "iphRa"),
    file.path(getwd(), "inst/app/www/i18n")
  )

  i18n_dir <- NULL
  for (path in i18n_paths) {
    if (dir.exists(path)) {
      i18n_dir <- path
      break
    }
  }

  if (is.null(i18n_dir)) {
    warning("i18n directory not found. Using fallback translations.")
    return(list(
      en = list(
        export_tor = "Export ToR",
        validation_passed = "Validation checks passed (dummy mode).",
        tor_export_success = "ToR export simulated successfully"
      ),
      fr = list(
        export_tor = "Exporter les TdR",
        validation_passed = "Verifications terminees avec succes (mode fictif).",
        tor_export_success = "Exportation simulee des TdR reussie"
      )
    ))
  }

  # Load each language file
  json_files <- list.files(i18n_dir, pattern = "\\.json$", full.names = TRUE)
  for (json_file in json_files) {
    lang_code <- tools::file_path_sans_ext(basename(json_file))
    tryCatch({
      translations[[lang_code]] <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
    }, error = function(e) {
      warning(paste("Failed to load translation file:", json_file, "-", e$message))
    })
  }

  if (length(translations) == 0) {
    warning("No translation files loaded. Using fallback translations.")
    return(list(
      en = list(validation_passed = "Validation checks passed (dummy mode)."),
      fr = list(validation_passed = "Verifications terminees avec succes (mode fictif).")
    ))
  }

  return(translations)
}

# Load translations at package load time
iphra_translations <- iphra_load_translations()

# Placeholder for current language (can later live in session$userData)
iphra_current_lang <- "en"

# ---- Safe Translation Lookup ----
#' Get translated text for a given key
#'
#' @param key The translation key (string). The key can be a full text string
#'   which will be converted to a key format, or a pre-defined key from the
#'   translation files.
#' @param lang Optional language code (e.g., "en", "fr"). If NULL, attempts to
#'   get from session or falls back to iphra_current_lang.
#' @param default Optional default value if key is not found.
#' @param session Shiny session object for reactive language selection.
#' @return The translated text string.
#' @noRd
iphra_txt <- function(key, lang = NULL, default = NULL, session = shiny::getDefaultReactiveDomain()) {
  # [FUTURE] When language reactivity is connected, use session$userData$lang()
  # If no reactive session available, fallback to iphra_current_lang or "en"

  if (is.null(lang)) {
    if (!is.null(session) && !is.null(session$userData$lang)) {
      # Safe reactive access - only works once session$userData$lang is defined
      lang <- tryCatch(session$userData$lang(), error = function(e) NULL)
    }
  }

  # Fallback chain
  if (is.null(lang) || !lang %in% names(iphra_translations)) {
    if (exists("iphra_current_lang", envir = .GlobalEnv)) {
      lang <- get("iphra_current_lang", envir = .GlobalEnv)
    } else {
      lang <- "en"
    }
  }

  # Convert text to key format if it's a full text string
  lookup_key <- iphra_text_to_key(key)

  # ---- Lookup ----
  value <- iphra_translations[[lang]][[lookup_key]]

  # If not found in target language and not English, try English as fallback
  if (is.null(value) && lang != "en") {
    value <- iphra_translations[["en"]][[lookup_key]]
  }

  # ---- Fallback logic ----

  if (is.null(value) || value == "") {
    if (!is.null(default)) return(default)
    # Return the original key as-is (useful during development)
    return(key)
  }

  return(value)
}

#' Convert a text string to a translation key format
#'
#' @param text The text string to convert
#' @return A lowercase key with underscores instead of spaces
#' @noRd
iphra_text_to_key <- function(text) {
  if (is.null(text) || text == "") return("")

  # Convert to lowercase
  key <- tolower(text)
  # Replace special characters with spaces first (preserve word boundaries)
  key <- gsub("[^a-z0-9]", " ", key)
  # Collapse multiple spaces and replace with underscores
  key <- gsub("\\s+", "_", trimws(key))
  # Remove leading/trailing underscores
  key <- gsub("^_+|_+$", "", key)
  # Handle keys that start with numbers
  if (nchar(key) > 0 && grepl("^[0-9]", key)) {
    key <- paste0("txt_", key)
  }
  # Limit key length to 100 characters for longer descriptive text
  if (nchar(key) > 100) {
    # Use a simple hash based on string length and character sum for uniqueness
    char_sum <- sum(utf8ToInt(text))
    hash_suffix <- sprintf("%x", char_sum %% 65536)
    key <- paste0(substr(key, 1, 90), "_", hash_suffix)
  }

  return(key)
}

#' Reload translations from JSON files
#'
#' Useful for development when translation files are updated.
#' @noRd
iphra_reload_translations <- function() {
  iphra_translations <<- iphra_load_translations()
  invisible(iphra_translations)
}
