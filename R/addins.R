#' Insert Callout via RStudio Addin
#'
#' Launches a Shiny app as an RStudio addin to insert a Quarto callout at the cursor position.
#' The user selects a callout type from a dropdown, sees a preview color, and inserts formatted
#' callout text into the current script.
#'
#' @return A Shiny application that runs within RStudio.
#'
#' @import shiny shinyFiles
#' @export
#'
#' @import shiny
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   insert_callout()
#' }
#' }
insert_callout <- function() {
  color_map <- c(
    "note"      = "#a1b7d4",
    "tip"       = "#80D1CE",
    "important" = "#FAB580",
    "warning"   = "#F8DA9A"
  )

  callout_button <- function(id, label, color) {
    shiny::actionButton(
      inputId = id,
      label = label,
      class = "btn-callout",
      style = paste0("background-color:", color, ";")
    )
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".btn-callout { width: 100%; font-weight: bold; margin-bottom: 10px; font-size: 16px; border: none; }"))
    ),
    shiny::titlePanel("Insert Callout"),
    shiny::fluidRow(
      shiny::column(12,
        callout_button("note",      "Note",      color_map["note"]),
        callout_button("warning",   "Warning",   color_map["warning"]),
        callout_button("important", "Important", color_map["important"]),
        callout_button("tip",       "Tip",       color_map["tip"])
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$note, {
      rstudioapi::insertText("::: {.callout-note}\n<your text>\n:::\n")
      shiny::stopApp()
    })
    shiny::observeEvent(input$warning, {
      rstudioapi::insertText("::: {.callout-warning}\n<your text>\n:::\n")
      shiny::stopApp()
    })
    shiny::observeEvent(input$important, {
      rstudioapi::insertText("::: {.callout-important}\n<your text>\n:::\n")
      shiny::stopApp()
    })
    shiny::observeEvent(input$tip, {
      rstudioapi::insertText("::: {.callout-tip}\n<your text>\n:::\n")
      shiny::stopApp()
    })
  }

  viewer <- shiny::dialogViewer("Insert Callout", width = 250, height = 250)
  shiny::runGadget(ui, server, viewer = viewer)
}

#' Insert Callout via RStudio Addin
#'
#' Inserts a Quarto callout block with placeholder text directly at the cursor.
#'
#' @return None. Inserts text into the active RStudio document.
#' @export
#'
#' @examples
#' \dontrun{
#' insert_callout()
#' }
insert_callout_2 <- function() {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(
      "::: {.callout-tip}\n#options: tip, note, warning, important\n<your text>\n:::\n"
    )
  } else {
    warning("RStudio API is not available.")
  }
}

#' Insert Margin Block at Cursor
#'
#' Inserts a Quarto column-margin block with placeholder text directly at the cursor.
#'
#' @return None. Inserts text into the active RStudio document.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   insert_margin()
#' }
#' }
insert_margin <- function() {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(
      "::: {.column-margin}\n<your comment>\n:::\n"
    )
  } else {
    warning("RStudio API is not available.")
  }
}

#' Shiny Addin: Create New Project
#'
#' Launches a Shiny app to create a project with folders, report, and control over session state.
#'
#' @export
create_project_addin <- function() {

  ui <- shiny::fluidPage(
    shiny::titlePanel("Create a New Project"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("project_name", "Project Name:", ""),
        shiny::actionButton("browse", "Browse Parent Directory"),
        shiny::textOutput("selected_dir"),
        shiny::br(),
        shiny::checkboxGroupInput("folders", "Folders to Include:",
          choices = c("data-raw", "data", "admin", "docs", "reports"),
          selected = c("data-raw", "data", "admin", "docs", "reports")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::textInput(
              "custom_folders",
              "Custom Folders",
              placeholder = "comma-separated folders"
            )
          ),
          shiny::column(
            width = 2,
            style = "margin-top: 25px;",  # aligns button with text input
            shiny::actionButton("add_custom_folder", "Add", class='btn-primary')
          )
        ),
        shiny::checkboxInput("create_report", "Create report in reports folder?", FALSE),
        shiny::conditionalPanel(
          condition = "input.create_report == true",
          shiny::selectInput("ext_name", "Report Type:",
            choices = list.files(system.file("ext_qmd/_extensions", package = "thekidsbiostats")),
            selected = "html")
        ),

        shiny::actionButton("create", "Create Project", class = "btn-primary")
      ),

      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(
            width = 9,
            shiny::wellPanel(
              shiny::tags$h4("Project Directory Structure"),
              shiny::textOutput("selected_dir"),
              shinyTree::shinyTree("tree", checkbox = FALSE)
            )
          )
        ),
        shiny::verbatimTextOutput("status")
      )
    )
  )

  server <- function(input, output, session) {
    project_path <- shiny::reactiveVal(NULL)

    options(all_folders = c("data-raw", "data", "admin", "docs", "reports"))

    shiny::observeEvent(input$browse, {
      selected <- rstudioapi::selectDirectory("Choose folder")
      if (!is.null(selected)) project_path(selected)
    })

    output$selected_dir <- shiny::renderText({
      req(project_path())
      paste(" ", project_path())
    })

    shiny::observeEvent(input$add_custom_folder, {
      new_folder <- trimws(input$custom_folders)

      if (nzchar(new_folder)) {
        current_choices <- isolate(input$folders)
        all_choices <- isolate(getOption("all_folders", c("data-raw", "data", "admin", "docs", "reports")))

        if (!(new_folder %in% all_choices)) {
          updated_choices <- c(all_choices, new_folder)
          options(all_folders = updated_choices)  # Save new state

          shiny::updateCheckboxGroupInput(
            inputId = "folders",
            choices = updated_choices,
            selected = c(current_choices, new_folder)
          )
        }
      }
    })

    shiny::observeEvent(input$create, {
      req(project_path(), input$project_name)

      tryCatch({
        create_project(
          path = project_path(),
          project_name = input$project_name,
          folders = input$folders,
          create_report = isTRUE(input$create_report),
          ext_name = input$ext_name,
          open_project = FALSE #isTRUE(input$open_project)
        )

        output$status <- shiny::renderText("✅ Project created successfully.")

        shiny::showModal(shiny::modalDialog(
          title = "✅ Project Successfully Created",
          "Would you like to open the project now?",
          footer = shiny::tagList(
            shiny::actionButton("cancel_open", "Close", class = "btn btn-danger"),
            shiny::actionButton("confirm_open", "Open Project", class = "btn-primary")
          )
        ))

      }, error = function(e) {
        shiny::showModal(shiny::modalDialog("Error", e$message, easyClose = TRUE))
        print("Fail")
      })



    })

    output$tree <- shinyTree::renderTree({
      included_folders <- input$folders

      tree <- list()

      proj_name <- if (nzchar(input$project_name)) input$project_name else "[project-name]"
      stats::setNames(list(structure(tree, stopened = TRUE)), paste0(proj_name, "/"))

      # Add folders conditionally
      for (folder in included_folders) {
        if (folder == "reports") {
          if (input$create_report) {
            report_file <- "report.qmd"
            tree[[paste0(folder, "/")]] <- structure(
              stats::setNames(list(structure("", sticon = "file")), report_file),
              stopened = TRUE
            )
          } else {
            tree[[paste0(folder, "/")]] <- structure("", stopened = TRUE)
          }
        } else {
          tree[[paste0(folder, "/")]] <- structure("", stopened = TRUE)
        }
      }

      # Nest under the main project directory
      stats::setNames(list(structure(tree, stopened = TRUE)), paste0(input$project_name, "/"))
    })


    shiny::observeEvent(input$confirm_open, {
      shiny::removeModal()

      rproj_file <- file.path(project_path(),input$project_name, paste0(input$project_name, ".Rproj"))

      if (file.exists(rproj_file) && rstudioapi::isAvailable()) {
        rstudioapi::openProject(path = rproj_file, newSession = TRUE)
      } else {
        output$status <- renderText("⚠️ Could not open project (file missing or RStudio API unavailable).")
      }

      shiny::stopApp()
    })

    observeEvent(input$cancel_open, {
      shiny::removeModal()
      shiny::stopApp()
    })

  }

  shiny::shinyApp(ui, server)
}


#' Shiny Addin: Create Report Template
#'
#' Launches a Shiny app to generate a report template in a user-selected folder.
#'
#' @export
create_template_addin <- function() {

  render_preview <- function(title, subtitle, author, affiliation, include_reproducibility) {
    template_path <- system.file("example_reports", "example.html", package = "thekidsbiostats")
    html_lines <- readLines(template_path, warn = FALSE)

    # Replace the line with the title
    if (!is.null(title)) {
      html_lines <- sub('<h1 class="title">.*?</h1>',
                        sprintf('<h1 class="title">%s</h1>', title),
                        html_lines)
    }

    if (!is.null(subtitle)) {
      html_lines <- sub('<p class="subtitle lead">.*?</p>',
                        sprintf('<p class="subtitle lead">%s</p>', subtitle),
                        html_lines)
    }


    # Replace the line with the author
    if (!is.null(author)) {
      html_lines <- sub(
        pattern = '<p class="author">.*?</p>',
        replacement = sprintf('<p class="author">%s</p>', author),
        html_lines
      )
    }

    # Replace the line with the author
    if (!is.null(affiliation)) {
      html_lines <- sub(
        '<p class="affiliation">.*?</p>',
        sprintf('<p class="affiliation">%s</p>', affiliation),
        html_lines
      )
    }

    if (!include_reproducibility) {
      # Remove reproducible info block (assumes it's marked with <!-- START REPRO --> and <!-- END REPRO -->)
      start <- grep("<!-- START REPRODUCIBILITY -->", html_lines)
      end <- grep("<!-- END REPRODUCIBILITY -->", html_lines)
      if (length(start) == 1 && length(end) == 1 && start < end) {
        html_lines <- html_lines[-(start:end)]
      }
    }

    # Save to temp file
    new_html <- tempfile(fileext = ".html")
    writeLines(html_lines, new_html)

    return(new_html)
  }

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),  # Initialise shinyjs
    shiny::titlePanel("Create Report Template"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(width=3,
        shiny::fluidRow(
          shiny::column(8, style = "padding-right:5px;",
                 shiny::textInput("file_name", "Report File Name:", value = "report")),
          shiny::column(4, style = "padding-left:5px; padding-right:5px;",
                 shiny::selectInput("ext_name", "Format:",
                                        choices = list.files(system.file("ext_qmd/_extensions", package = "thekidsbiostats")),
                                        selected = "html", width='80%'))
        ),
        shiny::actionButton("browse", "Select Folder"),
        shiny::textOutput("selected_dir"),
        shiny::br(),
        shiny::actionLink("toggle_advanced", "▶ More options"),
        shiny::uiOutput("advanced_ui"),
        shiny::br(),
        shiny::tags$div(
          id = 'create_wrap',
          title = "Please select a folder.",
          style = "display: inline-block;",
          shiny::actionButton("create", "Create Template", class = "btn-primary", disabled = "disabled")
        )
      ),
      shiny::mainPanel(width=9,
                       shiny::uiOutput("preview_html")
      )
    ),
    shiny::br(),
    shiny::verbatimTextOutput("status")
  )

  server <- function(input, output, session) {
    output_path <- shiny::reactiveVal(NULL)
    show_advanced <- shiny::reactiveVal(FALSE)

    default_title <- "The Kids Biostats Template"
    default_subtitle <- "A biostatistics report template"
    default_affiliation <- "The Kids Research Institute Australia, Perth, WA, Australia"
    default_name <- Sys.info()[["user"]]
    default_reproducibility <- TRUE

    # Check pattern: first two uppercase, third lowercase
    if (grepl("^[A-Z]{2}[a-z]", default_name)) {
      # Insert space between first and second characters
      default_name <- paste0(substr(default_name, 1, 1), " ", substr(default_name, 2, nchar(default_name)))
    }

    preview_path <- reactive({
      title = if (!is.null(input$title)) input$title else default_title
      subtitle = if (!is.null(input$subtitle)) input$subtitle else default_subtitle
      author <- if (!is.null(input$author)) input$author else default_name
      affiliation <- if (!is.null(input$affiliation)) input$affiliation else default_affiliation
      include_reproducibility = if (!is.null(input$reproducibility)) input$reproducibility else default_reproducibility

      render_preview(title=title, subtitle=subtitle, author=author, affiliation=affiliation, include_reproducibility=include_reproducibility)
    })

    debounced_preview <- shiny::debounce(preview_path, millis = 1000)

    shiny::observe({
      shiny::isolate({
        preview_path()  # force render_preview() once on startup
      })
    })

    output$preview_html <- renderUI({
      path <- debounced_preview()
      shiny::addResourcePath("preview", dirname(path))

      shiny::tags$div(
        style = "height: 600px; overflow: hidden; border: 1px solid #ddd; padding: 0;",
        shiny::tags$iframe(
          src = paste0("preview/", basename(path)),
          style = "width: 100%; height: 100%; border: none;"
        )
      )
    })

    default_name <- Sys.info()[["user"]]
    if (grepl("^[A-Z]{2}[a-z]", default_name)) {
      default_name <- paste0(substr(default_name, 1, 1), " ", substr(default_name, 2, nchar(default_name)))
    } else {
      default_name <- gsub("(?<=[a-z])(?=[A-Z])", " ", default_name, perl = TRUE)
    }

    # Map the resource path for your example_reports folder inside the package
    shiny::addResourcePath(
      prefix = "example_reports",
      directoryPath = system.file("example_reports", package = "thekidsbiostats")
    )

    # Disable the Create Template button initially
    shinyjs::disable("create")

    shiny::observeEvent(input$browse, {
      selected <- rstudioapi::selectDirectory("Choose target directory")
      if (!is.null(selected)) output_path(selected)

      if (!nzchar(input$file_name)) {
        shinyjs::disable("create")
        shinyjs::runjs('document.getElementById("create_wrap").setAttribute("title", "Please enter a valid file name");')
      } else if (is.null(output_path())) {
        shinyjs::disable("create")
        shinyjs::runjs('document.getElementById("create_wrap").setAttribute("title", "Please select a folder");')
      } else {  # Enable the button
        shinyjs::enable("create")
        shinyjs::runjs('document.getElementById("create_wrap").setAttribute("title", "Create template.");')
      }

    })

    output$selected_dir <- shiny::renderText({
      req(output_path())
      output_path()
    })


    shiny::observeEvent(input$toggle_advanced, {
      show_advanced(!show_advanced())
    })

    output$advanced_ui <- shiny::renderUI({
      if (show_advanced()) {
        shiny::updateActionLink(session, "toggle_advanced", label = "▼ More options")
        shiny::tagList(
          shiny::tags$hr(),
          shiny::textInput("title", "Title", value = if (!is.null(input$title)) input$title else default_title),
          shiny::textInput("subtitle", "Subtitle", value = if (!is.null(input$subtitle)) input$subtitle else default_subtitle),
          shiny::textInput("author", "Author Name", value = author <- if (!is.null(input$author)) input$author else default_name),
          shiny::textInput("affiliation", "Affiliation", value = if (!is.null(input$affiliation)) input$affiliation else default_affiliation),
          shiny::checkboxInput("reproducibility", "Include Reproducibility", value = if (!is.null(input$reproducibility)) input$reproducibility else default_reproducibility)
        )
      } else {
        shiny::updateActionLink(session, "toggle_advanced", label = "▶ More options")
        NULL
      }
    })


    shiny::observeEvent(input$create, {
      req(input$file_name, output_path())

      file_name <- ifelse(endsWith(input$file_name, ".qmd"), sub("\\.qmd$", "", input$file_name), input$file_name)

      tryCatch({
        create_template(
          file_name = file_name,
          directory = output_path(),
          ext_name = input$ext_name,
          open_file = TRUE
        )
        output$status <- shiny::renderText("\u2705 Report template created successfully.")
      }, error = function(e) {
        shiny::showModal(shiny::modalDialog("Error", e$message, easyClose = TRUE))
      })
      #shiny::stopApp()
    })

    # Clean up on session end
    # session$onSessionEnded(function() {
    #   unlink(temp_dir, recursive = TRUE)
    # })

  }

  shiny::shinyApp(ui, server)
}


#' RStudio Addin: Insert child Quarto tabset for model output
#'
#' This Shiny gadget inserts a `model_name <- "mod"` line and a child chunk to include
#' a preformatted tabset from an external Quarto file. It also copies the child file
#' to the same directory as the currently open document.
#'
#' @return Inserts code into the active RStudio document and copies a child QMD file.
#' @export
#'
#' @import shiny rstudioapi fs glue
insert_model_tabset <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Insert Model Output Tabs"),

    shiny::textInput("modobj", "Model object name", value = "mod"),

    shiny::actionButton("insert", "Insert into document")
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$insert, {
      model_name_line <- glue::glue('```{{r}}\nmodel_name <- "{input$modobj}"\n```')
      child_chunk <- '{{< include model-tabs-lm.qmd >}}'

      # Get active document path and directory
      doc_path <- rstudioapi::getActiveDocumentContext()$path
      if (doc_path == "") {
        shiny::showNotification("No active file open in RStudio", type = "error")
        return()
      }

      doc_dir <- fs::path_dir(doc_path)
      target_path <- fs::path(doc_dir, "model-tabs-lm.qmd")

      # Copy template child file
      template_path <- system.file("quarto_templates/model-tabs-lm.qmd", package = "thekidsbiostats")
      if (!fs::file_exists(template_path)) {
        shiny::showNotification("Child template file not found in package.", type = "error")
        return()
      }
      fs::file_copy(template_path, target_path, overwrite = FALSE)

      # Insert model_name assignment and child chunk
      insertion_text <- glue::glue("{model_name_line}\n\n{child_chunk}\n")
      rstudioapi::insertText(insertion_text)

      stopApp()
    })
  }

  viewer <- shiny::dialogViewer("Insert Model Tabs", width = 500, height = 250)
  shiny::runGadget(shinyApp(ui, server), viewer = viewer)
}
