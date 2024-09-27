#' Run at package load.
#' @export
.onLoad <- function(libname, pkgname) {

  sysfonts::font_add_google("Barlow")
  sysfonts::font_add_google("Barlow Semi Condensed")

  showtext::showtext_auto()

}
