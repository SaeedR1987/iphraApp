#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Everything translatable is rendered server-side (see app_server.R), so
  # switching language never reloads the page or drops session state.
  lang <- "en"

  fluidPage(

    tags$head(
      # Hide all Shiny notifications
      tags$style(HTML("
      .shiny-output-error-validation {
      display: none !important;
      }
                      ")),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('highlightBlock', function(message) {
        var el = document.getElementById(message.id);
        if (el) {
          el.setAttribute('fill', message.fill);
        }
      });
    ")),
      tags$style(HTML("
          .language-switcher .form-group {
            margin-bottom: 0;
          }
          .language-switcher select {
            padding: 2px 6px;
          }
          /* Dropdown submenu styles for nested menus */
          .dropdown-submenu {
            position: relative;
          }
          .dropdown-submenu > .dropdown-menu {
            top: 0;
            left: 100%;
            margin-top: -6px;
            margin-left: -1px;
            border-radius: 0 6px 6px 6px;
          }
          .dropdown-submenu:hover > .dropdown-menu {
            display: block;
          }
          .dropdown-submenu > a:after {
            display: block;
            content: ' ';
            float: right;
            width: 0;
            height: 0;
            border-color: transparent;
            border-style: solid;
            border-width: 5px 0 5px 5px;
            border-left-color: #ccc;
            margin-top: 5px;
            margin-right: -10px;
          }
          .dropdown-submenu:hover > a:after {
            border-left-color: #fff;
          }
          .dropdown-submenu.pull-left {
            float: none;
          }
          .dropdown-submenu.pull-left > .dropdown-menu {
            left: -100%;
            margin-left: 10px;
            border-radius: 6px 0 6px 6px;
          }
          /* Caret styling for right-facing arrows */
          .caret-right {
            border-left: 4px solid;
            border-top: 4px solid transparent;
            border-bottom: 4px solid transparent;
          }
          /* --- REPLACE/UPDATE NAVBAR / DROPDOWN Z-INDEX RULES WITH THIS --- */
/* Keep the navbar positioned above other content */
.navbar, .navbar.navbar-default {
  position: relative !important;
  z-index: 2050 !important; /* navbar base layer */
}

/* Dropdown menus (including nested submenus) must be above navbar and content */
.navbar .dropdown-menu,
.navbar .dropdown-menu .dropdown-menu,
.dropdown-submenu > .dropdown-menu {
  position: absolute !important;
  min-width: 200px; /* keep your min-width */
  z-index: 2060 !important; /* dropdowns above the navbar */
}

/* Make sure tab panels / main content sit underneath the dropdowns */
.tab-content, .tab-pane, .container-fluid {
  position: relative;
  z-index: 1;
}



          .navbar .dropdown-menu .glyphicon {
            margin-right: 8px;
            color: #666;
          }
          .navbar .dropdown-menu a:hover .glyphicon {
            color: #333;
          }
        "))

    ),

    tags$script(HTML("
      $(document).on('click', '#debug_toggle', function(e) {
        e.preventDefault();
        $('#debug_panel').toggle();
      });
    ")),

    # Top header: brand and global controls
    tags$header(
      class = "phr-header",
      tags$img(src = "www/img/iphra_logo.png", alt = "IPHRA", class = "phr-header__logo"),
      textOutput("app_title", container = tags$h1, inline = FALSE) |>
        htmltools::tagAppendAttributes(class = "phr-header__title"),
      div(
        class = "phr-header__right",
        uiOutput("phr_project_status", inline = TRUE),
        actionButton("phr_activity_btn", NULL,
                     tags$span(class = "glyphicon glyphicon-time", `aria-hidden` = "true"),
                     class = "btn-default btn-sm phr-header__activity",
                     title = "Recent activity", `aria-label` = "Recent activity"),
        div(
          class = "language-switcher",
          selectInput(
            inputId = "global_language",
            label = NULL,
            choices = c(
              "English"  = "en",
              "Fran\u00e7ais" = "fr",
              "Espa\u00f1ol"  = "es",
              "\u0627\u0644\u0639\u0631\u0628\u064a\u0629" = "ar"
            ),
            selected = lang,
            selectize = FALSE,
            width = "120px"
          )
        )
      )
    ),

    # Horizontal menu bar (File, Edit, ...) directly under the header
    tags$nav(class = "navbar navbar-default phr-menubar",
             div(class = "container-fluid",
                 uiOutput("app_menu", container = tags$ul, class = "nav navbar-nav")
             )
    ),

    golem_add_external_resources(),

    # Assessment-step sidebar. It drives the (visually hidden) workflow_nav
    # tabset below, so tab values and server behaviour are unchanged.
    div(
      class = "phr-shell",
      tags$nav(
        id = "phr_sidebar", class = "phr-sidebar", `aria-label` = "Assessment steps",
        tags$button(
          type = "button", id = "phr_sidebar_toggle", class = "phr-sidebar__toggle",
          `aria-label` = "Collapse or expand sidebar", `aria-expanded` = "true",
          tags$span(class = "glyphicon glyphicon-menu-hamburger", `aria-hidden` = "true")
        ),
        tags$ul(
          class = "phr-sidebar__list",
          lapply(iphra_sidebar_items(), function(item) {
            steps <- iphra_workflow_steps()
            link <- function(v, icon = steps[[v]]$icon) {
              tags$a(
                href = "#", class = "phr-sidebar__link",
                `data-phr-tab` = v, `data-phr-label-from` = paste0("tab_title_", v),
                tags$span(class = paste("glyphicon", icon), `aria-hidden` = "true"),
                tags$span(class = "phr-sidebar__label")
              )
            }
            if (is.null(item$children)) {
              tags$li(link(item$value))
            } else {
              tags$li(
                class = "phr-sidebar__group open",
                tags$button(
                  type = "button", class = "phr-sidebar__link phr-sidebar__group-toggle",
                  `data-phr-label-from` = paste0("tab_title_", item$value),
                  `aria-expanded` = "true",
                  tags$span(class = paste("glyphicon", item$icon), `aria-hidden` = "true"),
                  tags$span(class = "phr-sidebar__label"),
                  tags$span(class = "glyphicon glyphicon-chevron-down phr-sidebar__chevron",
                            `aria-hidden` = "true")
                ),
                tags$span(class = "sr-only", textOutput(paste0("tab_title_", item$value), inline = TRUE)),
                tags$ul(class = "phr-sidebar__sub", lapply(item$children, function(v) tags$li(link(v))))
              )
            }
          })
        )
      ),
      div(class = "phr-workspace",
    fluidPage(
      tags$head(tags$title("IPHRA")),
      uiOutput("phr_guidance"),

      navbarPage(
        title = NULL,
        id = "workflow_nav",
        collapsible = TRUE,
        fluid = TRUE,

        # ── 1. Goals ────────────────────────────────────────────────────
        tabPanel(textOutput("tab_title_goals", inline = TRUE), value = "goals",
                 mod_goals_ui("goals")),

        # ── 2. Tools ────────────────────────────────────────────────
        tabPanel(textOutput("tab_title_tools", inline = TRUE), value = "tools",

                 mod_tools_master_ui("tools_master"),
                 shiny::fluidRow(
                   shiny::column(7, uiOutput("tool_tabs")),
                   shiny::column(5, mod_tools_preview_ui("tools_preview"))
                 ),

        ),

        # ── 3. Sample Size and Planning ─────────────────────────────────────────────────
        # Planning ####
        tabPanel(textOutput("tab_title_planning", inline = TRUE), value = "planning",
                 mod_planning_sample_size_ui("planning_sample_size")
        ),

        # ── 4+. Placeholder steps (modules to be added) ─────────────────
        tabPanel(textOutput("tab_title_import", inline = TRUE), value = "import",
                 mod_data_import_ui("data_import")),
        tabPanel(textOutput("tab_title_standardization", inline = TRUE), value = "standardization",
                 mod_data_standardization_ui("data_standardization")),
        tabPanel(textOutput("tab_title_quality", inline = TRUE), value = "quality",
                 mod_data_quality_ui("data_quality")),
        tabPanel(textOutput("tab_title_cleaning", inline = TRUE), value = "cleaning",
                 mod_data_cleaning_ui("data_cleaning")),
        tabPanel(textOutput("tab_title_analysis_quant", inline = TRUE), value = "analysis_quant",
                 mod_analysis_quant_ui("analysis_quant")),
        iphra_placeholder_tab("analysis_qual"),
        iphra_placeholder_tab("analysis_integrated"),
        iphra_placeholder_tab("reporting")

      )
    )
      )
    ),

    # Hidden shinyFiles save button used by the "REACH Terms of Reference"
    # export menu item. The menu item's onclick handler clicks this button
    # programmatically so the shinyFiles save dialog opens directly — this
    # avoids nesting the dialog inside a modalDialog, which prevented the
    # save from completing (and therefore prevented generate_quarto_doc()
    # from running). See the input$save_tor observer in app_server.R.
    tags$div(
      style = "display: none;",
      shinyFiles::shinySaveButton(
        id = "save_tor",
        label = "Save REACH Terms of Reference",
        title = "Save REACH Terms of Reference",
        filename = "REACH_Terms_of_Reference",
        filetype = list(docx = "docx")
      )
    ),

    # Hidden shinyFiles save button used by the "Save Project" / "Save
    # Project As..." menu items. Their onclick handlers click this button
    # programmatically so the shinyFiles save dialog opens directly and the
    # user can choose where the `.iphra` project file is written. See the
    # input$save_project observer in app_server.R.
    tags$div(
      style = "display: none;",
      shinyFiles::shinySaveButton(
        id = "save_project",
        label = "Save Project",
        title = "Save IPHRA Project",
        filename = "IPHRA_Project",
        filetype = list(iphra = "iphra")
      )
    ),

    # Custom message handler used by the server-side "Save Project"
    # observer to programmatically open the hidden shinyFiles save dialog
    # (id = "save_project") when the current project has no known path yet,
    # falling back to a "Save Project As..." style flow.
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('phr_click_element', function(id) { var el = document.getElementById(id); if (el) { el.click(); } });"
    ))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "iphraApp"
		)
		# Add here other external resources
		# for example, you can add shinyalert::useShinyalert()
	)
}


#' Navbar menu items
#'
#' Rendered server-side (`output$app_menu`) so its labels follow the selected
#' language without reloading the page.
#'
#' @param lang Language code.
#' @noRd
iphra_menu_ui <- function(lang = "en") {
  shiny::tagList(

                         # ════════════════════════════════════════════════════════════════
                         # FILE MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("File", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # New Project
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "new_project_btn",
                                             onclick = "Shiny.setInputValue('new_project_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-file"),
                                             " ", phrutils::phr_txt("New Project", lang = lang)
                                           )
                                         ),
                                         # Open Project (file input)
                                         tags$li(
                                           tags$div(
                                             style = "padding: 5px 15px;",
                                             shiny::fileInput(
                                               "load_project_file",
                                               label = NULL,
                                               accept = c(".iphra"),
                                               buttonLabel = phrutils::phr_txt("Open Project", lang = lang),
                                               placeholder = phrutils::phr_txt("No file selected", lang = lang)
                                             )
                                           )
                                         ),
                                         # Recent Projects submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-folder-open"),
                                                        " ", phrutils::phr_txt("Recent Projects", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        id = "recent_project_1",
                                                                        onclick = "Shiny.setInputValue('recent_project_select', 1);",
                                                                        tags$em(phrutils::phr_txt("No recent projects", lang = lang))
                                                         ))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Save Project — performs a quick
                                         # save to the previously used path if
                                         # one exists (see the server-side
                                         # observer). Falls back to opening
                                         # the shinyFiles save dialog when the
                                         # project has never been saved.
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "save_project_btn",
                                             onclick = "Shiny.setInputValue('save_project_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-floppy-disk"),
                                             " ", phrutils::phr_txt("Save Project", lang = lang)
                                           )
                                         ),
                                         # Save Project As — always opens the
                                         # hidden shinyFiles save dialog
                                         # (id = "save_project") so the user
                                         # can pick a new destination.
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "save_project_as_btn",
                                             onclick = "document.getElementById('save_project').click();",
                                             tags$span(class = "glyphicon glyphicon-floppy-save"),
                                             " ", phrutils::phr_txt("Save Project As...", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Import Data submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-import"),
                                                        " ", phrutils::phr_txt("Import Data", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_csv_btn",
                                                                        onclick = "iphraImportFile('.csv,.txt'); return false;",
                                                                        phrutils::phr_txt("Import CSV", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_xlsx_btn",
                                                                        onclick = "iphraImportFile('.xlsx,.xls,.xlsm'); return false;",
                                                                        phrutils::phr_txt("Import Excel", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_kobo_btn",
                                                                        onclick = "Shiny.setInputValue('import_data_type', 'kobo');",
                                                                        phrutils::phr_txt("Import from KoBoToolbox", lang = lang)))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Project Properties
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "project_properties_btn",
                                             onclick = "Shiny.setInputValue('project_properties_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-cog"),
                                             " ", phrutils::phr_txt("Project Properties", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Debug Console (change logs viewer)
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "debug_toggle",
                                             onclick = "Shiny.setInputValue('debug_toggle', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-console"),
                                             " ", phrutils::phr_txt("Debug Console", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Exit
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "exit_app_btn",
                                             onclick = "Shiny.setInputValue('exit_app_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-log-out"),
                                             " ", phrutils::phr_txt("Exit", lang = lang)
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # EDIT MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("Edit", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "undo_btn",
                                             onclick = "Shiny.setInputValue('undo_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-arrow-left"),
                                             " ", phrutils::phr_txt("Undo", lang = lang)
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "redo_btn",
                                             onclick = "Shiny.setInputValue('redo_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-arrow-right"),
                                             " ", phrutils::phr_txt("Redo", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "find_replace_btn",
                                             onclick = "Shiny.setInputValue('find_replace_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-search"),
                                             " ", phrutils::phr_txt("Find and Replace", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "clear_all_data_btn",
                                             onclick = "Shiny.setInputValue('clear_all_data_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-trash"),
                                             " ", phrutils::phr_txt("Clear All Data", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "preferences_btn",
                                             onclick = "Shiny.setInputValue('preferences_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-wrench"),
                                             " ", phrutils::phr_txt("Preferences", lang = lang)
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # VIEW MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("View", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Zoom submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-zoom-in"),
                                                        " ", phrutils::phr_txt("Zoom", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "document.body.style.zoom='50%';",
                                                                        "50%")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "document.body.style.zoom='75%';",
                                                                        "75%")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "document.body.style.zoom='100%';",
                                                                        "100%")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "document.body.style.zoom='125%';",
                                                                        "125%")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "document.body.style.zoom='150%';",
                                                                        "150%"))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "fullscreen_btn",
                                             onclick = "Shiny.setInputValue('fullscreen_btn', Date.now()); if(document.fullscreenElement){document.exitFullscreen();}else{document.documentElement.requestFullscreen();}",
                                             tags$span(class = "glyphicon glyphicon-fullscreen"),
                                             " ", phrutils::phr_txt("Toggle Fullscreen", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Panel visibility toggles
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "toggle_sidebar_btn",
                                             onclick = "Shiny.setInputValue('toggle_sidebar_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-th-list"),
                                             " ", phrutils::phr_txt("Toggle Sidebar", lang = lang)
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "toggle_status_bar_btn",
                                             onclick = "Shiny.setInputValue('toggle_status_bar_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-info-sign"),
                                             " ", phrutils::phr_txt("Toggle Status Bar", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Theme submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-adjust"),
                                                        " ", phrutils::phr_txt("Theme", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'light');",
                                                                        phrutils::phr_txt("Light", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'dark');",
                                                                        phrutils::phr_txt("Dark", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'system');",
                                                                        phrutils::phr_txt("System Default", lang = lang)))
                                                 )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # EXPORT MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("Export", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Documents submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-file"),
                                                        " ", phrutils::phr_txt("Documents", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(
                                                           href = "#",
                                                           # Programmatically click the hidden shinySaveButton
                                                           # (id = "save_tor") so the shinyFiles save dialog
                                                           # opens directly, without an intermediate modalDialog.
                                                           onclick = "document.getElementById('save_tor').click();",
                                                           phrutils::phr_txt("REACH Terms of Reference", lang = lang)
                                                         )),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'protocol');",
                                                                        phrutils::phr_txt("Technical Protocol", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'report');",
                                                                        phrutils::phr_txt("Technical Report", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'brief');",
                                                                        phrutils::phr_txt("Brief", lang = lang)))
                                                 )
                                         ),
                                         # Data submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-list-alt"),
                                                        " ", phrutils::phr_txt("Data", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'raw');",
                                                                        phrutils::phr_txt("Raw Dataset", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'clean');",
                                                                        phrutils::phr_txt("Clean Dataset", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'cleaning_log');",
                                                                        phrutils::phr_txt("Cleaning Log", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'deletion_log');",
                                                                        phrutils::phr_txt("Deletion Log", lang = lang)))
                                                 )
                                         ),
                                         # Analysis Results submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-stats"),
                                                        " ", phrutils::phr_txt("Analysis Results", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'integrated');",
                                                                        phrutils::phr_txt("Integrated Analysis", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'summary');",
                                                                        phrutils::phr_txt("Summary Results", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'tables');",
                                                                        phrutils::phr_txt("Analysis Tables", lang = lang))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'charts');",
                                                                        phrutils::phr_txt("Charts and Figures", lang = lang)))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Presentations
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             onclick = "Shiny.setInputValue('export_ppt', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-blackboard"),
                                             " ", phrutils::phr_txt("Preliminary Powerpoint", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Export All
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "export_all_btn",
                                             onclick = "Shiny.setInputValue('export_all_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-download-alt"),
                                             " ", phrutils::phr_txt("Export All...", lang = lang)
                                           )
                                         ),
                                         # Export Format Options
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-cog"),
                                                        " ", phrutils::phr_txt("Export Format", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_format', 'xlsx');",
                                                                        "Excel (.xlsx)")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_format', 'csv');",
                                                                        "CSV (.csv)")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_format', 'rds');",
                                                                        "R Data (.rds)"))
                                                 )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # SETTINGS MENU (NEW)
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("Settings", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Manage Data Schema (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-th"),
                                                        " ", phrutils::phr_txt("Manage Data Schema", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'FSL', time: Date.now()});",
                                                                        "FSL")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'Health', time: Date.now()});",
                                                                        "Health")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'WASH', time: Date.now()});",
                                                                        "WASH")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'Nutrition', time: Date.now()});",
                                                                        "Nutrition")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'MUAC', time: Date.now()});",
                                                                        "MUAC")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'data', sector: 'Mortality', time: Date.now()});",
                                                                        "Mortality"))
                                                 )
                                         ),
                                         # Manage Quality Schema (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-check"),
                                                        " ", phrutils::phr_txt("Manage Quality Schema", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'FSL', time: Date.now()});",
                                                                        "FSL")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'Health', time: Date.now()});",
                                                                        "Health")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'WASH', time: Date.now()});",
                                                                        "WASH")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'Nutrition', time: Date.now()});",
                                                                        "Nutrition")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'MUAC', time: Date.now()});",
                                                                        "MUAC")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'quality', sector: 'Mortality', time: Date.now()});",
                                                                        "Mortality"))
                                                 )
                                         ),
                                         # Manage Analysis Schema (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-stats"),
                                                        " ", phrutils::phr_txt("Manage Analysis Schema", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'FSL', time: Date.now()});",
                                                                        "FSL")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'Health', time: Date.now()});",
                                                                        "Health")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'WASH', time: Date.now()});",
                                                                        "WASH")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'Nutrition', time: Date.now()});",
                                                                        "Nutrition")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'MUAC', time: Date.now()});",
                                                                        "MUAC")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('schema_trigger', {type: 'analysis', sector: 'Mortality', time: Date.now()});",
                                                                        "Mortality"))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Manage Integrated Analysis Schema (no submenu)
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             onclick = "Shiny.setInputValue('schema_trigger', {type: 'integrated_analysis', sector: null, time: Date.now()});",
                                             tags$span(class = "glyphicon glyphicon-tasks"),
                                             " ", phrutils::phr_txt("Manage Integrated Analysis Schema", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Repair Variable and Value Maps (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-wrench"),
                                                        " ", phrutils::phr_txt("Repair Variable and Value Maps", lang = lang), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         # Household Data Submenu
                                                         tags$li(tags$strong(style = "padding-left: 15px; color: #666;", "Household Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'main_household', time: Date.now()});",
                                                                        "Main Household Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'roster', time: Date.now()});",
                                                                        "Roster Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'health_individual', time: Date.now()});",
                                                                        "Health Individual Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'water_container', time: Date.now()});",
                                                                        "Water Container Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'nutrition_individual', time: Date.now()});",
                                                                        "Nutrition Individual Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'woman_individual', time: Date.now()});",
                                                                        "Woman Individual Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'mortality_death', time: Date.now()});",
                                                                        "Mortality Death Data")),
                                                         tags$li(class = "divider"),
                                                         # KII Data
                                                         tags$li(tags$strong(style = "padding-left: 15px; color: #666;", "KII Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'community_kii', time: Date.now()});",
                                                                        "Community KII Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'fsl_ngo_kii', time: Date.now()});",
                                                                        "FSL NGO KII Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'wash_ngo_kii', time: Date.now()});",
                                                                        "WASH NGO KII Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'health_facility_kii', time: Date.now()});",
                                                                        "Health Facility KII Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'nutrition_facility_kii', time: Date.now()});",
                                                                        "Nutrition Facility KII Data")),
                                                         tags$li(class = "divider"),
                                                         # Observation Data
                                                         tags$li(tags$strong(style = "padding-left: 15px; color: #666;", "Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'community_observation', time: Date.now()});",
                                                                        "Community Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'water_point_observation', time: Date.now()});",
                                                                        "Water Point Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'latrine_observation', time: Date.now()});",
                                                                        "Latrine Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'health_facility_observation', time: Date.now()});",
                                                                        "Health Facility Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'nutrition_facility_observation', time: Date.now()});",
                                                                        "Nutrition Facility Observation Data")),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('repair_maps_trigger', {dataset: 'livelihoods_observation', time: Date.now()});",
                                                                        "Livelihoods Observation Data"))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Customize Livelihoods
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "customize_livelihoods_btn",
                                             onclick = "Shiny.setInputValue('customize_livelihoods_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-grain"),
                                             " ", phrutils::phr_txt("Customize Livelihoods", lang = lang)
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # HELP MENU (NEW)
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", phrutils::phr_txt("Help", lang = lang), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "documentation_btn",
                                             onclick = "Shiny.setInputValue('documentation_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-book"),
                                             " ", phrutils::phr_txt("Documentation", lang = lang)
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "tutorials_btn",
                                             onclick = "Shiny.setInputValue('tutorials_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-education"),
                                             " ", phrutils::phr_txt("Tutorials", lang = lang)
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "keyboard_shortcuts_btn",
                                             onclick = "Shiny.setInputValue('keyboard_shortcuts_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-flash"),
                                             " ", phrutils::phr_txt("Keyboard Shortcuts", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "report_issue_btn",
                                             onclick = "Shiny.setInputValue('report_issue_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-exclamation-sign"),
                                             " ", phrutils::phr_txt("Report an Issue", lang = lang)
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "check_updates_btn",
                                             onclick = "Shiny.setInputValue('check_updates_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-refresh"),
                                             " ", phrutils::phr_txt("Check for Updates", lang = lang)
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "about_btn",
                                             onclick = "Shiny.setInputValue('about_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-info-sign"),
                                             " ", phrutils::phr_txt("About IPHRA", lang = lang)
                                           )
                                         )
                                 )
                         )
  )
}