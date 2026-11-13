# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Make errors visible instead of a blank "Error: "
options(
  warn = 1,
  shiny.fullstacktrace = TRUE,
  shiny.error = function() {
    cat("\n[shiny.error] Unhandled error in app:\n")
    print(rlang::trace_back())
  },
  rlang_backtrace_on_error = "full",
  rlang_backtrace_on_warning = "branch"
)

# Run the application
tryCatch(
  withCallingHandlers(
    run_app(),
    error = function(e) {
      cat("\n[run_dev] Error caught:\n")
      cat("class:  ", paste(class(e), collapse = " / "), "\n")
      cat("message:", conditionMessage(e), "\n")
      cat("cnd_message:", tryCatch(rlang::cnd_message(e), error = function(...) ""), "\n")
      cat("call:   ", paste(deparse(conditionCall(e)), collapse = " "), "\n")
      print(sys.calls())
    }
  ),
  error = function(e) stop(e)
)
