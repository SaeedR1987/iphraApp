#' Shared UI components
#'
#' Reusable, module-independent UI building blocks styled by
#' `inst/app/www/css/phr-components.css` (classes prefixed `phr-`). They contain no
#' logic; callers pass already-translated text (e.g. via `phrutils::phr_txt()`).
#'
#' @name phr_components
#' @param ... Content.
#' @param title,label,value,subtitle,text Already-translated strings.
#' @param class Additional CSS classes.
#' @return A [shiny::tag] object.
#' @import shiny
#' @noRd
NULL

phr_cls <- function(base, class = NULL) paste(c(base, class), collapse = " ")

#' @noRd
phr_card <- function(..., title = NULL, footer = NULL, class = NULL) {
  div(
    class = phr_cls("phr-card", class),
    if (!is.null(title)) div(class = "phr-card__header", title),
    div(class = "phr-card__body", ...),
    if (!is.null(footer)) div(class = "phr-card__footer", footer)
  )
}

#' @noRd
phr_metric_card <- function(label, value, subtitle = NULL, class = NULL) {
  div(
    class = phr_cls("phr-card phr-metric", class),
    div(class = "phr-metric__label", label),
    div(class = "phr-metric__value", value),
    if (!is.null(subtitle)) div(class = "phr-metric__sub", subtitle)
  )
}

#' Status badge. Status is conveyed by an icon and text, not colour alone.
#' @param status One of "success", "warning", "error", "info", "neutral".
#' @noRd
phr_badge <- function(text, status = c("neutral", "success", "warning", "error", "info"),
                      class = NULL) {
  status <- match.arg(status)
  icon <- switch(status,
    success = "\u2713", warning = "!", error = "\u2715", info = "i", neutral = "\u2022"
  )
  span(
    class = phr_cls(paste0("phr-badge phr-badge--", status), class),
    span(class = "phr-badge__icon", `aria-hidden` = "true", icon),
    text
  )
}

#' @noRd
phr_page_header <- function(title, subtitle = NULL, actions = NULL, class = NULL) {
  div(
    class = phr_cls("phr-page-header", class),
    div(
      class = "phr-page-header__text",
      h2(class = "phr-page-header__title", title),
      if (!is.null(subtitle)) p(class = "phr-page-header__sub", subtitle)
    ),
    if (!is.null(actions)) div(class = "phr-page-header__actions", actions)
  )
}

#' @noRd
phr_toolbar <- function(..., class = NULL) {
  div(class = phr_cls("phr-toolbar", class), role = "toolbar", ...)
}

#' @param status One of "info", "success", "warning", "error".
#' @noRd
phr_alert <- function(..., status = c("info", "success", "warning", "error"),
                      title = NULL, class = NULL) {
  status <- match.arg(status)
  div(
    class = phr_cls(paste0("phr-alert phr-alert--", status), class),
    role = if (status %in% c("warning", "error")) "alert" else "status",
    if (!is.null(title)) div(class = "phr-alert__title", title),
    div(class = "phr-alert__body", ...)
  )
}

#' @noRd
phr_info_panel <- function(..., title = NULL, class = NULL) {
  div(
    class = phr_cls("phr-info-panel", class),
    if (!is.null(title)) div(class = "phr-info-panel__title", title),
    div(...)
  )
}

#' Contextual help icon with a native tooltip (no extra dependencies).
#' @param text Help text shown on hover/focus.
#' @noRd
phr_help_icon <- function(text, class = NULL) {
  span(
    class = phr_cls("phr-help", class),
    tabindex = "0", role = "img", title = text, `aria-label` = text,
    "i"
  )
}

#' Static progress indicator; update the value from the server as needed.
#' @param value Percentage 0-100.
#' @noRd
phr_progress <- function(value, label = NULL, class = NULL) {
  value <- max(0, min(100, value))
  div(
    class = phr_cls("phr-progress", class),
    if (!is.null(label)) div(class = "phr-progress__label", label),
    div(
      class = "phr-progress__track", role = "progressbar",
      `aria-valuemin` = "0", `aria-valuemax` = "100", `aria-valuenow` = value,
      div(class = "phr-progress__bar", style = paste0("width:", value, "%;"))
    )
  )
}

#' @param variant One of "primary", "secondary", "ghost".
#' @noRd
phr_button <- function(inputId, label, variant = c("primary", "secondary", "ghost"),
                       class = NULL, ...) {
  variant <- match.arg(variant)
  actionButton(inputId, label,
    class = phr_cls(paste0("phr-btn phr-btn--", variant), class), ...
  )
}
