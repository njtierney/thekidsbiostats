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
#' if (interactive()) {
#'   insert_callout()
#' }
insert_callout <- function() {

  # Custom color map
  color_map <- c(
    "note"      = "#a1b7d4",
    "tip"       = "#80D1CE",
    "important" = "#FAB580",
    "warning"   = "#F8DA9A"
  )

  # Helper to generate a button for a given type
  callout_button <- function(id, label, color) {
    actionButton(
      inputId = id,
      label = label,
      class = "btn-callout",
      style = paste0("background-color:", color, ";")
    )
  }

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        .btn-callout {
          width: 100%;
          font-weight: bold;
          margin-bottom: 10px;
          font-size: 16px;
          border: none;
        }
      "))
    ),
    titlePanel("Insert Callout"),
    fluidRow(
      column(12,
             callout_button("note",      "Note",      color_map["note"]),
             callout_button("warning",   "Warning",   color_map["warning"]),
             callout_button("important", "Important", color_map["important"]),
             callout_button("tip",       "Tip",       color_map["tip"])
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$note, {
      rstudioapi::insertText("::: {.callout-note}\n<Insert text here>\n:::\n")
      stopApp()
    })

    observeEvent(input$warning, {
      rstudioapi::insertText("::: {.callout-warning}\n<Insert text here>\n:::\n")
      stopApp()
    })

    observeEvent(input$important, {
      rstudioapi::insertText("::: {.callout-important}\n<Insert text here>\n:::\n")
      stopApp()
    })

    observeEvent(input$tip, {
      rstudioapi::insertText("::: {.callout-tip}\n<Insert text here>\n:::\n")
      stopApp()
    })
  }

  viewer <- dialogViewer("Insert Callout", width = 250, height = 250)
  runGadget(ui, server, viewer = viewer)
}

#' Insert Margin Block at Cursor
#'
#' Inserts a Quarto column-margin block with placeholder text directly at the cursor.
#'
#' @return None. Inserts text into the active RStudio document.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   insert_margin()
#' }
insert_margin <- function() {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(
      "::: {.column-margin}\ncomment text\n:::\n"
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
  library(shiny)

  ui <- fluidPage(
    titlePanel("Create a New Project"),

    sidebarLayout(
      sidebarPanel(
        textInput("project_name", "Project Name:", ""),
        actionButton("browse", "Browse Parent Directory"),
        textOutput("selected_dir"),

        checkboxGroupInput("folders", "Folders to Include:",
                           choices = c("data-raw", "data", "admin", "docs", "reports"),
                           selected = c("data-raw", "data", "admin", "docs", "reports")),

        checkboxInput("create_report", "Create report in reports folder?", FALSE),
        conditionalPanel(
          condition = "input.create_report == true",
          selectInput("ext_name", "Report Type:",
                      choices = list.files(system.file("ext_qmd/_extensions", package = "thekidsbiostats")),
                      selected = "html")
        ),

        checkboxInput("open_project", "Open new project", TRUE),

        actionButton("create", "Create Project", class = "btn-primary")
      ),

      mainPanel(
        verbatimTextOutput("status")
      )
    )
  )

  server <- function(input, output, session) {
    project_path <- reactiveVal(NULL)

    observeEvent(input$browse, {
      selected <- rstudioapi::selectDirectory("Choose folder")
      if (!is.null(selected)) project_path(selected)
    })

    output$selected_dir <- renderText({
      req(project_path())
      paste("Selected Directory:", project_path())
    })

    observeEvent(input$create, {
      req(project_path(), input$project_name)

      tryCatch({
        create_project(
          path = project_path(),
          project_name = input$project_name,
          folders = input$folders,
          create_report = isTRUE(input$create_report),
          ext_name = input$ext_name,
          open_project = isTRUE(input$open_project)
        )

        output$status <- renderText("✅ Project created successfully.")

      }, error = function(e) {
        showModal(modalDialog("Error", e$message, easyClose = TRUE))
      })

      stopApp()
    })
  }

  shinyApp(ui, server)
}


#' Shiny Addin: Create Report Template
#'
#' Launches a Shiny app to generate a report template in a user-selected folder.
#'
#' @export
create_template_addin <- function() {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Create Report Template"),

    sidebarLayout(
      sidebarPanel(
        textInput("file_name", "Report File Name:", value = "report"),
        selectInput("ext_name", "Report Type:",
                    choices = list.files(system.file("ext_qmd/_extensions", package = "thekidsbiostats")),
                    selected = "html"),
        actionButton("browse", "Browse Output Folder"),
        textOutput("selected_dir"),
        actionButton("create", "Create Template", class = "btn-success")
      ),
      mainPanel(
        verbatimTextOutput("status")
      )
    )
  )

  server <- function(input, output, session) {
    output_path <- reactiveVal(NULL)

    observeEvent(input$browse, {
      selected <- rstudioapi::selectDirectory("Choose target directory")
      if (!is.null(selected)) output_path(selected)
    })

    output$selected_dir <- renderText({
      req(output_path())
      paste("Selected Directory:", output_path())
    })

    observeEvent(input$create, {
      req(input$file_name, output_path())

      tryCatch({
        create_template(
          file_name = input$file_name,
          directory = output_path(),
          ext_name = input$ext_name,
          open_file = TRUE
        )

        output$status <- renderText("✅ Report template created successfully.")

      }, error = function(e) {
        showModal(modalDialog("Error", e$message, easyClose = TRUE))
      })
      stopApp()
    })
  }

  shinyApp(ui, server)
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
  ui <- fluidPage(
    titlePanel("Insert Model Output Tabs"),

    textInput("modobj", "Model object name", value = "mod"),

    actionButton("insert", "Insert into document")
  )

  server <- function(input, output, session) {
    observeEvent(input$insert, {
      model_name_line <- glue::glue('```{{r}}\nmodel_name <- "{input$modobj}"\n```')
      child_chunk <- '{{< include model-tabs-lm.qmd >}}'

      # Get active document path and directory
      doc_path <- rstudioapi::getActiveDocumentContext()$path
      if (doc_path == "") {
        showNotification("No active file open in RStudio", type = "error")
        return()
      }

      doc_dir <- fs::path_dir(doc_path)
      target_path <- fs::path(doc_dir, "model-tabs-lm.qmd")

      # Copy template child file
      template_path <- system.file("quarto_templates/model-tabs-lm.qmd", package = "thekidsbiostats")
      if (!fs::file_exists(template_path)) {
        showNotification("Child template file not found in package.", type = "error")
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
