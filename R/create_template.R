#' Create a Quarto report template
#'
#' Copies extension files into `_extensions/` and creates a `.qmd` report file.
#' Optionally opens the created file. If the file already exists, no overwrite occurs and a warning is shown.
#'
#' @param file_name Name of the report file (without .qmd).
#' @param directory Directory to save the report and extensions in.
#' @param ext_name Name of the template extension (e.g. "html", "word").
#' @param open_file Whether to open the file after creation.
#'
#' @return Invisibly returns path to created .qmd file, or NULL if skipped.
#' @export
create_template <- function(file_name = NULL,
                            directory = "reports",
                            ext_name = "html",
                            open_file = TRUE) {

  if (is.null(file_name)) stop("You must provide a file_name.")
  if (!dir.exists(directory)) stop("Directory does not exist.")

  valid_ext <- list.files(system.file("ext_qmd/_extensions", package = "thekidsbiostats"))
  stopifnot("Extension not in package" = ext_name %in% valid_ext)

  message("📦 Using template extension: ", ext_name)

  extfolder <- file.path(directory, "_extensions")
  dir.create(extfolder, showWarnings = FALSE)
  dir.create(file.path(extfolder, ext_name), showWarnings = FALSE)

  message("📁 _extensions folder created: ", extfolder)

  file.copy(
    from = system.file(file.path("ext_qmd/_extensions", ext_name), package = "thekidsbiostats"),
    to = extfolder,
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )
  message("📄 Template extension files copied to: ", file.path(extfolder, ext_name))

  qmd_file <- file.path(directory, paste0(file_name, ".qmd"))

  if (file.exists(qmd_file)) {
    warning("⚠️ Report file already exists: ", qmd_file, ". Skipping creation.")
    return(invisible(NULL))
  }

  file.copy(file.path(extfolder, ext_name, "template.qmd"), qmd_file)
  file.remove(file.path(extfolder, ext_name, "template.qmd"))

  message("📝 Report template created: ", qmd_file)

  if (open_file && rstudioapi::isAvailable()) {
    message("📂 Opening report: ", qmd_file)
    rstudioapi::navigateToFile(qmd_file)
  }

  invisible(qmd_file)
}
