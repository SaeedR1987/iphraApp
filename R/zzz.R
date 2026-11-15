.onLoad <- function(libname, pkgname) {
  # `system.file()` is called here (not inside phrutils) so that
  # `devtools::load_all()` can map it to `inst/resources/`.
  translations_path <- system.file(
    "resources", "translations.json",
    package = "iphraApp"
  )

  if (nzchar(translations_path)) {
    tryCatch(
      phrutils::phr_load_translations(path = translations_path),
      error = function(e) {
        warning(
          "[iphraApp] Failed to load translations: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
}
