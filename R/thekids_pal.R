#' @title Return function to interpolate a The Kids color palette
#'
#' @param palette Character name of palette in thekids_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
thekids_pal <- function(palette = "primary", reverse = FALSE, ...) {

  pal <- thekidsbiostats::thekids_palettes[[palette]][c(1, 2, 4, 7)]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)

}


