create_project <- function(ext_name = "basic",
                           data_raw = T,
                           data = T,
                           admin = T,
                           reports = T,
                           docs = T) {

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
  # remove qmd template in _extensions

}
