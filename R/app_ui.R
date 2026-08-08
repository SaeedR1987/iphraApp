#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(

    tags$head(
      tags$style(HTML("
    body {
      font-size: 0.85em; /* shrink base font size */
    }
    .well, .panel, .form-control, .btn {
      transform: scale(0.95);
      transform-origin: top left;
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
          .language-switcher label {
            margin-right: 10px;
            font-weight: 500;
          }
          .language-switcher input[type='radio'] {
            margin-right: 4px;
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

    # Main top navbar
    tags$nav(class = "navbar navbar-default",
             div(class = "container-fluid",
                 # Brand / title
                 div(class = "navbar-header",
                     tags$a(class = "navbar-brand", href = "#", "PHR -- menu")
                 ),
                 # Dropdown menus
                 tags$ul(class = "nav navbar-nav",

                         # ════════════════════════════════════════════════════════════════
                         # FILE MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("File"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # New Project
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "new_project_btn",
                                             onclick = "Shiny.setInputValue('new_project_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-file"),
                                             " ", iphra_txt("New Project")
                                           )
                                         ),
                                         # Open Project (file input)
                                         tags$li(
                                           tags$div(
                                             style = "padding: 5px 15px;",
                                             shiny::fileInput(
                                               "load_project_file",
                                               label = NULL,
                                               accept = c(".rds", ".json"),
                                               buttonLabel = iphra_txt("Open Project"),
                                               placeholder = iphra_txt("No file selected")
                                             )
                                           )
                                         ),
                                         # Recent Projects submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-folder-open"),
                                                        " ", iphra_txt("Recent Projects"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        id = "recent_project_1",
                                                                        onclick = "Shiny.setInputValue('recent_project_select', 1);",
                                                                        tags$em(iphra_txt("No recent projects"))
                                                         ))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Save Project
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "save_project_btn",
                                             onclick = "Shiny.setInputValue('save_project_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-floppy-disk"),
                                             " ", iphra_txt("Save Project")
                                           )
                                         ),
                                         # Save Project As
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "save_project_as_btn",
                                             onclick = "Shiny.setInputValue('save_project_as_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-floppy-save"),
                                             " ", iphra_txt("Save Project As...")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Import Data submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-import"),
                                                        " ", iphra_txt("Import Data"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_csv_btn",
                                                                        onclick = "Shiny.setInputValue('import_data_type', 'csv');",
                                                                        iphra_txt("Import CSV"))),
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_xlsx_btn",
                                                                        onclick = "Shiny.setInputValue('import_data_type', 'xlsx');",
                                                                        iphra_txt("Import Excel"))),
                                                         tags$li(tags$a(href = "#",
                                                                        id = "import_kobo_btn",
                                                                        onclick = "Shiny.setInputValue('import_data_type', 'kobo');",
                                                                        iphra_txt("Import from KoBoToolbox")))
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
                                             " ", iphra_txt("Project Properties")
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
                                             " ", iphra_txt("Exit")
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # EDIT MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("Edit"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "undo_btn",
                                             onclick = "Shiny.setInputValue('undo_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-arrow-left"),
                                             " ", iphra_txt("Undo")
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "redo_btn",
                                             onclick = "Shiny.setInputValue('redo_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-arrow-right"),
                                             " ", iphra_txt("Redo")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "find_replace_btn",
                                             onclick = "Shiny.setInputValue('find_replace_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-search"),
                                             " ", iphra_txt("Find and Replace")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "clear_all_data_btn",
                                             onclick = "Shiny.setInputValue('clear_all_data_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-trash"),
                                             " ", iphra_txt("Clear All Data")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "debug_toggle",
                                             onclick = "Shiny.setInputValue('debug_toggle', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-console"),
                                             " ", iphra_txt("Debug Console")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "preferences_btn",
                                             onclick = "Shiny.setInputValue('preferences_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-wrench"),
                                             " ", iphra_txt("Preferences")
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # VIEW MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("View"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Zoom submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-zoom-in"),
                                                        " ", iphra_txt("Zoom"), " ",
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
                                             " ", iphra_txt("Toggle Fullscreen")
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
                                             " ", iphra_txt("Toggle Sidebar")
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "toggle_status_bar_btn",
                                             onclick = "Shiny.setInputValue('toggle_status_bar_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-info-sign"),
                                             " ", iphra_txt("Toggle Status Bar")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Theme submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-adjust"),
                                                        " ", iphra_txt("Theme"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'light');",
                                                                        iphra_txt("Light"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'dark');",
                                                                        iphra_txt("Dark"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('theme_select', 'system');",
                                                                        iphra_txt("System Default")))
                                                 )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # EXPORT MENU
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("Export"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Documents submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-file"),
                                                        " ", iphra_txt("Documents"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'tor');",
                                                                        iphra_txt("REACH Terms of Reference"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'protocol');",
                                                                        iphra_txt("Technical Protocol"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'report');",
                                                                        iphra_txt("Technical Report"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_doc', 'brief');",
                                                                        iphra_txt("Brief")))
                                                 )
                                         ),
                                         # Data submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-list-alt"),
                                                        " ", iphra_txt("Data"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'raw');",
                                                                        iphra_txt("Raw Dataset"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'clean');",
                                                                        iphra_txt("Clean Dataset"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'cleaning_log');",
                                                                        iphra_txt("Cleaning Log"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_data', 'deletion_log');",
                                                                        iphra_txt("Deletion Log")))
                                                 )
                                         ),
                                         # Analysis Results submenu
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-stats"),
                                                        " ", iphra_txt("Analysis Results"), " ",
                                                        tags$span(class = "caret caret-right")),
                                                 tags$ul(class = "dropdown-menu",
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'integrated');",
                                                                        iphra_txt("Integrated Analysis"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'summary');",
                                                                        iphra_txt("Summary Results"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'tables');",
                                                                        iphra_txt("Analysis Tables"))),
                                                         tags$li(tags$a(href = "#",
                                                                        onclick = "Shiny.setInputValue('export_analysis', 'charts');",
                                                                        iphra_txt("Charts and Figures")))
                                                 )
                                         ),
                                         tags$li(class = "divider"),
                                         # Presentations
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             onclick = "Shiny.setInputValue('export_ppt', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-blackboard"),
                                             " ", iphra_txt("Preliminary Powerpoint")
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
                                             " ", iphra_txt("Export All...")
                                           )
                                         ),
                                         # Export Format Options
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-cog"),
                                                        " ", iphra_txt("Export Format"), " ",
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
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("Settings"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         # Manage Data Schema (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-th"),
                                                        " ", iphra_txt("Manage Data Schema"), " ",
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
                                                        " ", iphra_txt("Manage Quality Schema"), " ",
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
                                                        " ", iphra_txt("Manage Analysis Schema"), " ",
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
                                             " ", iphra_txt("Manage Integrated Analysis Schema")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         # Repair Variable and Value Maps (with submenu)
                                         tags$li(class = "dropdown-submenu",
                                                 tags$a(href = "#", tabindex = "-1",
                                                        tags$span(class = "glyphicon glyphicon-wrench"),
                                                        " ", iphra_txt("Repair Variable and Value Maps"), " ",
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
                                             " ", iphra_txt("Customize Livelihoods")
                                           )
                                         )
                                 )
                         ),

                         # ════════════════════════════════════════════════════════════════
                         # HELP MENU (NEW)
                         # ════════════════════════════════════════════════════════════════
                         tags$li(class = "dropdown",
                                 tags$a(href = "#", class = "dropdown-toggle", iphra_txt("Help"), `data-toggle`="dropdown"),
                                 tags$ul(class = "dropdown-menu",
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "documentation_btn",
                                             onclick = "Shiny.setInputValue('documentation_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-book"),
                                             " ", iphra_txt("Documentation")
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "tutorials_btn",
                                             onclick = "Shiny.setInputValue('tutorials_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-education"),
                                             " ", iphra_txt("Tutorials")
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "keyboard_shortcuts_btn",
                                             onclick = "Shiny.setInputValue('keyboard_shortcuts_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-flash"),
                                             " ", iphra_txt("Keyboard Shortcuts")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "report_issue_btn",
                                             onclick = "Shiny.setInputValue('report_issue_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-exclamation-sign"),
                                             " ", iphra_txt("Report an Issue")
                                           )
                                         ),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "check_updates_btn",
                                             onclick = "Shiny.setInputValue('check_updates_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-refresh"),
                                             " ", iphra_txt("Check for Updates")
                                           )
                                         ),
                                         tags$li(class = "divider"),
                                         tags$li(
                                           tags$a(
                                             href = "#",
                                             id = "about_btn",
                                             onclick = "Shiny.setInputValue('about_btn', Date.now());",
                                             tags$span(class = "glyphicon glyphicon-info-sign"),
                                             " ", iphra_txt("About IPHRA")
                                           )
                                         )
                                 )
                         )
                 )
             )
    ),

    # ---- Global Language Selector (UI only for now) ----
    tags$div(
      class = "language-switcher",
      style = "
        position: absolute;
        top: 8px;
        right: 25px;
        z-index: 1050;
        background: rgba(255,255,255,0.9);
        padding: 3px 10px;
        border-radius: 6px;
        box-shadow: 0 0 4px rgba(0,0,0,0.1);
      ",
      radioButtons(
        inputId = "global_language",
        label = NULL,
        choices = c("English" = "en", "Français" = "fr"),
        selected = "en",
        inline = TRUE
      )
    ),

    golem_add_external_resources(),
    fluidPage(
      titlePanel(iphra_txt("Public Health R Toolkit")),

      navbarPage(
        title = NULL,
        id = "workflow_nav",
        collapsible = TRUE,
        fluid = TRUE,

        # ── 1. Goals ────────────────────────────────────────────────────
        tabPanel(iphra_txt("Goals and Objectives"), mod_goals_ui("goals")),

        # ── 2. Tools ────────────────────────────────────────────────
        tabPanel(iphra_txt("Tool Design"),

                 mod_tools_master_ui("tools_master"),

                 tabsetPanel(
                   tabPanel(iphra_txt("Core Household Tool"), mod_tools_household_ui("tools_household")),
                   tabPanel(iphra_txt("Core Community Tools"), mod_tools_community_ui("tools_community")),
                   tabPanel(iphra_txt("Health and Nutrition Tools"), mod_tools_health_ui("tools_health")),
                   tabPanel(iphra_txt("FSL Tools"), mod_tools_fsl_ui("tools_fsl")),
                   tabPanel(iphra_txt("WASH Tools"), mod_tools_wash_ui("tools_wash")),
                 )
        ),

        # ── 3. Sample Size and Planning ─────────────────────────────────────────────────
        # Planning ####
        tabPanel(iphra_txt("Planning and Sampling"), mod_planning_sample_size_ui("planning_sample_size")
        )

      )
    )
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
