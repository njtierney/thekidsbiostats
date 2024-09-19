#' Create Template for Quarto
#'
#' This function creates a HTML or Word template in the current working directory
#' using a specified Quarto extension. It copies the template files to the
#' `_extensions/` directory and generates a new Quarto markdown (.qmd) file.
#'
#' @param file_name A string. The name of the new Quarto markdown (.qmd) file. This must be provided.
#' @param ext_name A string. The name of the extension to use for the HTML template.
#'        Defaults to "thekids-html". "thekids-word" is an alternative which used the Word template.
#'
#' @details
#' The function first checks whether a `_extensions/` directory exists in the current working
#' directory. If not, it creates one. It then copies the necessary extension files from the
#' package's internal data to the `_extensions/` directory. Finally, it creates
#' a new Quarto markdown file based on the extension template.
#'
#' @note
#' The function assumes that the package `thekidsbiostats` contains the necessary extension files
#' under `extdata/_extensions/`.
#'
#' @examples
#' \dontrun{
#' create_template(file_name = "my_report")
#' create_template(file_name = "my_doc", ext_name = "thekids-word")
#' }
#'
#' @export
create_template <- function(file_name = NULL,
                                 ext_name = "thekids-html") {

  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }

  # check for available extensions
  stopifnot("Extension not in package" = ext_name %in% c("thekids-html", "thekids-word"))

  # check for existing _extensions directory
  if(!file.exists("_extensions")) dir.create("_extensions")
  message("Created '_extensions' folder")

  # create folder
  if(!file.exists(paste0("_extensions/", ext_name))) dir.create(paste0("_extensions/", ext_name))

  # copy from internals
  file.copy(
    from = system.file(paste0("extdata/_extensions/", ext_name), package = "thekidsbiostats"),
    to = paste0("_extensions/"),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )

  # logic check to make sure extension files were moved
  n_files <- length(dir(paste0("_extensions/", ext_name)))

  if(n_files >= 2){
    message(paste(ext_name, "was installed to _extensions folder in current working directory."))
  } else {
    message("Extension appears not to have been created")
  }

  # create new qmd report based on skeleton
  file.copy(paste0("_extensions/", ext_name, "/template.qmd"),
            paste0(file_name, ".qmd", collapse = ""))

}
