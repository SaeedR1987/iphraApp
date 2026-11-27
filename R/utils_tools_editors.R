# -----------------------------------------------------------------------------
# Tool editors: "Update Core Survey Questions" and "Contextualize Sectoral
# Questions" modals of the Tool Design action card.
# -----------------------------------------------------------------------------
#
# `iphra_tools_editors_server()` is called from `mod_tools_master_server()` and
# registers the modal UIs, the rhandsontable renderers and the save handlers on
# the master module's namespace. Edits go through the Tool API:
#   * choice lists  -> Tool$update_choice_list()
#   * settings      -> revised_settings (+ Tool$change_default_language())
#   * question text -> revised_survey label::<language> columns

iphra_core_choice_lists <- function() {
  list(
    admin = c("admin1", "admin2", "admin3", "admin4", "cluster", "residency"),
    team = c("team", "enum_id")
  )
}

iphra_sectoral_specs <- function() {
  list(
    lcsi = list(
      title = "LCSI",
      pattern = "^fsl_lcsi_",
      names = NULL,
      lists = "coping_choice"
    ),
    water = list(
      title = "Improved Water Sources",
      pattern = NULL,
      names = "wash_water_source",
      lists = "source_water"
    ),
    sanit = list(
      title = "Improved Sanitation Facilities",
      pattern = NULL,
      names = "wash_toilet_facility",
      lists = "toilet_type"
    )
  )
}

iphra_tool_field <- function(tool, field) {
  tryCatch(tool$get(field = field), error = function(e) tryCatch(tool[[field]], error = function(e2) NULL))
}

iphra_blank_to_na <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[!nzchar(trimws(x))] <- NA_character_
    x
  })
  df
}

iphra_tool_languages <- function(tool) {
  sv <- iphra_tool_field(tool, "revised_survey")
  if (!is.data.frame(sv) || !ncol(sv)) sv <- iphra_tool_field(tool, "survey")
  sub("^label::", "", grep("^label::", names(sv), value = TRUE))
}

iphra_tool_choices <- function(tool) {
  ch <- iphra_tool_field(tool, "revised_choices")
  if (!is.data.frame(ch) || !nrow(ch)) ch <- iphra_tool_field(tool, "choices")
  ch
}

# Choice list as an editable data frame (name + label::<language> columns).
iphra_choice_list_df <- function(tool, list_name) {
  cols <- c("name", paste0("label::", iphra_tool_languages(tool)))
  ch <- iphra_tool_choices(tool)
  df <- NULL
  if (is.data.frame(ch) && "list_name" %in% names(ch)) {
    df <- ch[ch$list_name %in% list_name, intersect(cols, names(ch)), drop = FALSE]
  }
  if (is.null(df) || !nrow(df)) {
    df <- as.data.frame(
      stats::setNames(rep(list(NA_character_), length(cols)), cols),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  df[] <- lapply(df, as.character)
  rownames(df) <- NULL
  df
}

# Returns NULL on success, or a message describing why the list was rejected.
iphra_save_choice_list <- function(tool, list_name, df) {
  df <- iphra_blank_to_na(df)
  df <- df[!is.na(df$name), , drop = FALSE]
  if (anyDuplicated(df$name)) {
    return(paste0("List '", list_name, "' has duplicated names: ",
                  paste(unique(df$name[duplicated(df$name)]), collapse = ", ")))
  }
  ch <- iphra_tool_choices(tool)
  code <- NA
  if (is.data.frame(ch) && all(c("list_name", "indicator_code") %in% names(ch))) {
    prev <- ch$indicator_code[ch$list_name %in% list_name & !is.na(ch$indicator_code)]
    if (length(prev)) code <- prev[[1]]
  }
  if (is.na(code)) code <- 10000
  df$indicator_code <- code
  tool$update_choice_list(list_name = list_name, new_choices = df)
  NULL
}

iphra_survey_rows <- function(tool, qname) {
  sv <- iphra_tool_field(tool, "revised_survey")
  list(survey = sv, idx = which(sv$name %in% qname))
}

iphra_hot <- function(df, readonly = character(0), height = 260) {
  if (!nrow(df)) {
    df[1, ] <- NA_character_
  }
  h <- rhandsontable::rhandsontable(
    df, stretchH = "all", rowHeaders = NULL, height = height, useTypes = FALSE
  )
  for (cl in intersect(readonly, names(df))) {
    h <- rhandsontable::hot_col(h, cl, readOnly = TRUE)
  }
  h
}

iphra_tools_editors_server <- function(input, output, session, get_tool, active_role, label_of, touch) {
  ns <- session$ns
  notify <- function(msg, type = "message") shiny::showNotification(msg, type = type)

  edit_role <- shiny::reactiveVal(NULL)
  version <- shiny::reactiveVal(0L)
  tool_now <- function() {
    version()
    r <- edit_role()
    if (is.null(r)) NULL else get_tool(r)
  }

  core <- iphra_core_choice_lists()
  specs <- iphra_sectoral_specs()

  # ---- Core survey questions ---------------------------------------------------
  list_tabs <- function(lists) {
    do.call(shiny::tabsetPanel, c(
      list(id = ns(paste0("core_sub_", lists[[1]]))),
      lapply(lists, function(l) {
        shiny::tabPanel(l, shiny::br(), rhandsontable::rHandsontableOutput(ns(paste0("core_", l))))
      })
    ))
  }

  shiny::observeEvent(input$core_update, {
    r <- active_role()
    if (is.null(r)) return()
    edit_role(r)
    version(version() + 1L)
    tool <- get_tool(r)
    langs <- if (is.null(tool)) character(0) else iphra_tool_languages(tool)
    cur_lang <- tryCatch(as.character(iphra_tool_field(tool, "revised_settings")$default_language[1]), error = function(e) NA)
    st <- tryCatch(iphra_tool_field(tool, "revised_settings"), error = function(e) data.frame())
    val <- function(col) if (is.data.frame(st) && col %in% names(st) && nrow(st)) as.character(st[[col]][1]) else ""
    shiny::showModal(shiny::modalDialog(
      title = paste("Update Core Survey Questions -", label_of(r)),
      size = "l", easyClose = FALSE,
      shiny::p(class = "text-muted", "Edit the cells directly; right-click a table to insert or remove rows."),
      shiny::tabsetPanel(
        shiny::tabPanel("Admin and Strata", shiny::br(), list_tabs(core$admin)),
        shiny::tabPanel("Team and Enumerator", shiny::br(), list_tabs(core$team)),
        shiny::tabPanel(
          "Settings", shiny::br(),
          shiny::textInput(ns("set_form_title"), "form_title", value = val("form_title"), width = "100%"),
          shiny::textInput(ns("set_version"), "version", value = val("version"), width = "100%"),
          shiny::selectInput(
            ns("set_default_language"), "default_language",
            choices = langs,
            selected = if (!is.na(cur_lang) && cur_lang %in% langs) cur_lang else langs[1]
          )
        )
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("core_save"), "Save", class = "btn-primary", icon = shiny::icon("save"))
      )
    ))
  })

  for (l in unlist(core)) {
    local({
      lname <- l
      output[[paste0("core_", lname)]] <- rhandsontable::renderRHandsontable({
        tool <- tool_now()
        shiny::req(tool)
        iphra_hot(iphra_choice_list_df(tool, lname))
      })
    })
  }

  shiny::observeEvent(input$core_save, {
    r <- edit_role()
    tool <- if (is.null(r)) NULL else get_tool(r)
    if (is.null(tool)) return()
    phrutils::phr_try({
      problems <- character(0)
      saved <- character(0)
      for (l in unlist(core)) {
        hot <- input[[paste0("core_", l)]]
        if (is.null(hot)) next
        msg <- iphra_save_choice_list(tool, l, rhandsontable::hot_to_r(hot))
        if (is.null(msg)) saved <- c(saved, l) else problems <- c(problems, msg)
      }

      if (!is.null(input$set_form_title)) {
        st <- iphra_tool_field(tool, "revised_settings")
        if (!is.data.frame(st) || !nrow(st)) {
          st <- data.frame(form_title = NA_character_, version = NA_character_, default_language = NA_character_, stringsAsFactors = FALSE)
        }
        st[["form_title"]] <- input$set_form_title
        st[["version"]] <- input$set_version
        tool$revised_settings <- st
        if (nzchar(input$set_default_language %||% "")) {
          tool$change_default_language(language = input$set_default_language)
        }
        saved <- c(saved, "settings")
      }

      touch()
      if (length(problems)) {
        notify(paste(problems, collapse = " "), "warning")
      } else {
        shiny::removeModal()
        notify(if (length(saved)) paste0("Updated: ", paste(saved, collapse = ", "), ".") else "Nothing to update.")
      }
    },
    on_error = "warn",
    origin = "Tools Master: Update Core Survey Questions",
    hint = "Check the edited choice lists and settings.")
  })

  # ---- Contextualize sectoral questions -----------------------------------------
  shiny::observeEvent(input$ctx_update, {
    r <- active_role()
    if (is.null(r)) return()
    edit_role(r)
    version(version() + 1L)
    tool <- get_tool(r)
    sv <- if (is.null(tool)) NULL else iphra_tool_field(tool, "revised_survey")
    qnames <- if (is.data.frame(sv) && "name" %in% names(sv)) {
      unique(sv$name[!is.na(sv$name) & !is.na(sv$type) & grepl("^(select_|text|integer|decimal)", sv$type)])
    } else character(0)

    sector_tab <- function(key) {
      shiny::tabPanel(
        specs[[key]]$title, shiny::br(),
        shiny::h5("Questions"), rhandsontable::rHandsontableOutput(ns(paste0("ctx_", key, "_q"))),
        shiny::h5("Choices"),
        do.call(shiny::tabsetPanel, lapply(specs[[key]]$lists, function(l) {
          shiny::tabPanel(l, shiny::br(), rhandsontable::rHandsontableOutput(ns(paste0("ctx_", key, "_c_", l))))
        }))
      )
    }

    shiny::showModal(shiny::modalDialog(
      title = paste("Contextualize Sectoral Questions -", label_of(r)),
      size = "l", easyClose = FALSE,
      shiny::p(class = "text-muted", "Edit question text and choices directly; right-click a choices table to insert or remove rows."),
      shiny::tabsetPanel(
        id = ns("ctx_tabs"),
        shiny::tabPanel(
          "Question", shiny::br(),
          shiny::selectizeInput(ns("ctx_question"), "Question (survey name)", choices = qnames, width = "100%"),
          shiny::h5("Question text by language"),
          rhandsontable::rHandsontableOutput(ns("ctx_generic_q")),
          shiny::h5("Choices"),
          shiny::uiOutput(ns("ctx_generic_list_ui")),
          rhandsontable::rHandsontableOutput(ns("ctx_generic_c"))
        ),
        sector_tab("lcsi"), sector_tab("water"), sector_tab("sanit")
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("ctx_save"), "Save", class = "btn-primary", icon = shiny::icon("save"))
      )
    ))
  })

  # Generic tab: one question, rows = languages.
  generic_df <- shiny::reactive({
    tool <- tool_now()
    q <- input$ctx_question
    shiny::req(tool, q, nzchar(q))
    rows <- iphra_survey_rows(tool, q)
    sv <- rows$survey
    langs <- iphra_tool_languages(tool)
    i <- rows$idx[1]
    get <- function(col) if (col %in% names(sv) && !is.na(i)) as.character(sv[[col]][i]) else NA_character_
    data.frame(
      Language = langs,
      Label = vapply(langs, function(l) get(paste0("label::", l)), character(1)),
      Hint = vapply(langs, function(l) get(paste0("hint::", l)), character(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  })

  generic_list <- shiny::reactive({
    tool <- tool_now()
    q <- input$ctx_question
    shiny::req(tool, q, nzchar(q))
    rows <- iphra_survey_rows(tool, q)
    ty <- if (length(rows$idx)) as.character(rows$survey$type[rows$idx[1]]) else NA_character_
    if (is.na(ty) || !grepl("^select_(one|multiple)\\s+", ty)) return(NA_character_)
    strsplit(trimws(sub("^select_(one|multiple)\\s+", "", ty)), "\\s+")[[1]][1]
  })

  output$ctx_generic_q <- rhandsontable::renderRHandsontable({
    iphra_hot(generic_df(), readonly = "Language", height = 150)
  })
  output$ctx_generic_list_ui <- shiny::renderUI({
    l <- generic_list()
    if (is.na(l)) shiny::p(class = "text-muted", "This question has no choice list.") else shiny::p(class = "text-muted", paste("Choice list:", l))
  })
  output$ctx_generic_c <- rhandsontable::renderRHandsontable({
    tool <- tool_now()
    l <- generic_list()
    shiny::req(tool, !is.na(l))
    iphra_hot(iphra_choice_list_df(tool, l))
  })

  # Sector tabs: rows = questions, columns = label per language.
  sector_q_df <- function(tool, spec) {
    sv <- iphra_tool_field(tool, "revised_survey")
    nm <- unique(sv$name[!is.na(sv$name)])
    keep <- if (!is.null(spec$pattern)) nm[grepl(spec$pattern, nm)] else intersect(spec$names, nm)
    cols <- paste0("label::", iphra_tool_languages(tool))
    cols <- intersect(cols, names(sv))
    df <- sv[match(keep, sv$name), c("name", cols), drop = FALSE]
    df[] <- lapply(df, as.character)
    rownames(df) <- NULL
    df
  }

  for (k in names(specs)) {
    local({
      key <- k
      spec <- specs[[key]]
      output[[paste0("ctx_", key, "_q")]] <- rhandsontable::renderRHandsontable({
        tool <- tool_now()
        shiny::req(tool)
        iphra_hot(sector_q_df(tool, spec), readonly = "name", height = 220)
      })
      for (l in spec$lists) {
        local({
          lname <- l
          output[[paste0("ctx_", key, "_c_", lname)]] <- rhandsontable::renderRHandsontable({
            tool <- tool_now()
            shiny::req(tool)
            iphra_hot(iphra_choice_list_df(tool, lname))
          })
        })
      }
    })
  }

  write_survey_text <- function(tool, qname, values) {
    rows <- iphra_survey_rows(tool, qname)
    sv <- rows$survey
    if (!length(rows$idx)) return(invisible(NULL))
    for (col in intersect(names(values), names(sv))) {
      sv[[col]][rows$idx] <- values[[col]]
    }
    tool$revised_survey <- sv
  }

  shiny::observeEvent(input$ctx_save, {
    r <- edit_role()
    tool <- if (is.null(r)) NULL else get_tool(r)
    if (is.null(tool)) return()
    phrutils::phr_try({
      problems <- character(0)
      saved <- character(0)

      gq <- input$ctx_generic_q
      q <- input$ctx_question
      if (!is.null(gq) && !is.null(q) && nzchar(q)) {
        df <- rhandsontable::hot_to_r(gq)
        vals <- list()
        for (i in seq_len(nrow(df))) {
          vals[[paste0("label::", df$Language[i])]] <- df$Label[i]
          vals[[paste0("hint::", df$Language[i])]] <- df$Hint[i]
        }
        write_survey_text(tool, q, vals)
        saved <- c(saved, q)
      }
      gc <- input$ctx_generic_c
      gl <- tryCatch(generic_list(), error = function(e) NA_character_)
      if (!is.null(gc) && !is.na(gl)) {
        msg <- iphra_save_choice_list(tool, gl, rhandsontable::hot_to_r(gc))
        if (is.null(msg)) saved <- c(saved, gl) else problems <- c(problems, msg)
      }

      for (key in names(specs)) {
        sq <- input[[paste0("ctx_", key, "_q")]]
        if (!is.null(sq)) {
          df <- rhandsontable::hot_to_r(sq)
          for (i in seq_len(nrow(df))) {
            vals <- as.list(df[i, setdiff(names(df), "name"), drop = FALSE])
            write_survey_text(tool, df$name[i], lapply(vals, as.character))
          }
          saved <- c(saved, specs[[key]]$title)
        }
        for (l in specs[[key]]$lists) {
          sc <- input[[paste0("ctx_", key, "_c_", l)]]
          if (is.null(sc)) next
          msg <- iphra_save_choice_list(tool, l, rhandsontable::hot_to_r(sc))
          if (is.null(msg)) saved <- c(saved, l) else problems <- c(problems, msg)
        }
      }

      touch()
      if (length(problems)) {
        notify(paste(problems, collapse = " "), "warning")
      } else {
        shiny::removeModal()
        notify(if (length(saved)) paste0("Updated: ", paste(unique(saved), collapse = ", "), ".") else "Nothing to update.")
      }
    },
    on_error = "warn",
    origin = "Tools Master: Contextualize Sectoral Questions",
    hint = "Check the edited questions and choice lists.")
  })
}
