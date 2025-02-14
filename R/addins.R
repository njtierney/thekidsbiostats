#' Insert a Quarto Callout Chunk
#'
#' This function allows the user to insert a Quarto callout chunk into the
#' currently active RStudio editor. The user is prompted to choose the type
#' of callout (e.g., note, tip, caution, important, or warning), and the
#' corresponding chunk is generated in the correct Quarto syntax.
#'
#' @return Inserts the selected callout chunk into the RStudio editor.
#' Returns `NULL` invisibly.
#' @examples
#' \dontrun{
#' # Use the addin in RStudio to insert a callout
#' insert_callout()
#' }
#' @details
#' This function uses the `rstudioapi` package to insert text directly
#' into the active editor. It generates Quarto callout chunks in the format:
#'
#' ```
#' ::: {.callout-type}
#' Your content here.
#' :::
#' ```
#' where `type` corresponds to the callout type chosen by the user.
#'
#' The available callout types are:
#' - `note`
#' - `tip`
#' - `caution`
#' - `important`
#' - `warning`
#'
#' @import rstudioapi
#' @importFrom glue glue
#' @export
insert_callout <- function() {
  # Define the available callout types
  callout_types <- c("note", "tip", "caution", "important", "warning")

  # Use a prompt to ask for the callout type
  selected_type <- rstudioapi::showPrompt(
    title = "Choose Callout Type",
    message = "Enter one of the following: note, tip, caution, important, warning.",
    default = "note"
  )

  # If the user cancels or provides an invalid response, stop
  if (is.null(selected_type) || !selected_type %in% callout_types) {
    rstudioapi::showDialog("Invalid Callout Type", "Please select a valid callout type next time.")
    return()
  }

  # Define the Quarto callout chunk template with escaped braces
  callout_chunk <- glue::glue("
::: {{.callout-{selected_type}}}
Your content here.
:::
")

  # Insert the chunk into the active RStudio editor
  rstudioapi::insertText(callout_chunk)
}
