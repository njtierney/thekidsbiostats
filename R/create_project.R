#' Create a New Project Structure with Extension
#'
#' This function creates a directory structure for a new project based on a specified extension.
#' It can also create additional folders such as `data_raw`, `data`, `admin`, `reports`, and `docs`.
#' The function copies specific files and folders from the chosen extension to the project directory.
#'
#' @param ext_name A string. The name of the project extension to use. Defaults to `"basic"`. The extension must be available in the package's `ext_proj/_extensions/` directory.
#' @param data_raw Logical. If `TRUE`, a `data_raw` directory will be created in the project. Defaults to `TRUE`.
#' @param data Logical. If `TRUE`, a `data` directory will be created in the project. Defaults to `TRUE`.
#' @param admin Logical. If `TRUE`, an `admin` directory will be created in the project. Defaults to `TRUE`.
#' @param reports Logical. If `TRUE`, a `reports` directory will be created in the project. Defaults to `TRUE`.
#' @param docs Logical. If `TRUE`, a `docs` directory will be created in the project. Defaults to `TRUE`.
#'
#' @details
#' This function helps set up the structure of a new project using a predefined extension and
#' optional additional directories. It ensures that the selected extension is valid by checking
#' the available extensions from the `thekidsbiostats` package. After creating the necessary folders,
#' the function copies the appropriate files from the package extension into the project.
#'
#' @note
#' Ensure that the `thekidsbiostats` package is installed and contains the required extension
#' in `ext_proj/_extensions/`.
#'
#' @examples
#' \dontrun{
#' create_project(ext_name = "basic")
#' create_project(ext_name = "targets", data_raw = FALSE, docs = TRUE)
#' }
#'
#' @export
create_project <- function(ext_name = "basic",
                           data_raw = T,
                           data = T,
                           admin = T,
                           reports = T,
                           docs = T) {

  valid_ext <- list.files(system.file("ext_proj/_extensions", package = "thekidsbiostats"))

  # check for available extensions
  stopifnot("Extension not in package" = ext_name %in% valid_ext)

  # add requested directories
  if(data_raw) {
    if(!file.exists("data_raw")) dir.create("data_raw")
  }
  if(data) {
    if(!file.exists("data")) dir.create("data")
  }
  if(admin) {
    if(!file.exists("admin")) dir.create("admin")
  }
  if(reports) {
    if(!file.exists("reports")) dir.create("reports")
  }
  if(docs) {
    if(!file.exists("docs")) dir.create("docs")
  }

  # copy specific files and folders from extension
  files <- list.files(system.file(paste0("ext_proj/_extensions/", ext_name), package = "thekidsbiostats"))
  file.copy(
    from = system.file(paste0("ext_proj/_extensions/", ext_name, "/", files), package = "thekidsbiostats"),
    to = paste0("."),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )
}
