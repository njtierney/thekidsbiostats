#' @title .onAttach
#' @description This function runs when the package is attached via library() or require().
#' It dynamically loads the Google Font specified in the options.

.onLoad <- function(libname, pkgname) {
  # Set a default option for the font
  if (is.null(getOption("thekidsbiostats.font"))) {
    options(thekidsbiostats.font = "Barlow")  # Set default font if not specified
  }
}

.onAttach <- function(libname, pkgname) {
  # Get the primary font name from options or default to "Barlow"
  font <- getOption("thekidsbiostats.font", "Barlow")

  # Get all available Google Fonts
  all_google_fonts <- sysfonts::font_families_google()

  # Find related font families
  related_fonts <- grep(font, all_google_fonts, value = TRUE, ignore.case = TRUE)

  # Helper function to safely add a font family
  add_google_font_safe <- function(font_name) {
    tryCatch({
      sysfonts::font_add_google(name = font_name, family = font_name)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  # Register each related font family
  loaded_fonts <- sapply(related_fonts, add_google_font_safe)

  # Activate showtext for plotting
  showtext::showtext_auto()

  # Inform the user about loaded fonts
  message <- paste0("Loaded Google Fonts: ",
                    paste(names(loaded_fonts)[loaded_fonts], collapse = ", "))

  packageStartupMessage(message)
}

