#' The Kids themed table output
#'
#' A function that accepts tabular data, in a range of formats, and outputs an object of class `flextable()` (intended) for display in html and word documents.
#'
#' The purpose of this function is easily coerce many different table structures in a consistent format (look and feel), with The Kids branding applied, that ultimately will look nice in either an html or word output.
#'
#' Default settings produce a relatively compact table, to avoid reports becoming excessively lengthy.
#'
#' The output can be piped (`%>%`) into further `flextable()` functions for advance customisation of the appearance.
#'
#' Currently the function works well with input in the form of data frames, tibbles, dplyr pipes (think `summarise()`), `gtsummary()` output, `kable()` output, and `flextable()` output.
#'
#' @note
#' Errors may be encountered if the input to the function (kable/gtsummary/flextable) has already received a lot of processing (merging cells, aesthetic changes). The intention is that these things would occur after running `thekids_table()`.
#'
#' Font family must be installed at a system level, otherwise the default ("sans") will be applied.
#'
#' @param x a table, typically a data.frame, tibble, or output from gtsummary
#' @param font.size the font size for text in the body of the table, defaults to 8 (passed throught to set_flextable_defaults)
#' @param font.size.header the font size for text in the header of the table, defaults to 10
#' @param line.spacing line spacing for the table, defaults to 1.5 (passed through to set_flextable_defaults)
#' @param padding padding around all four sides of the text within the cell, defaults to 2.5 (passed throught to set_flextable_defaults)
#' @param colour a colour palette from The Kids branding, options include "Saffron", "Pumpkin", "Teal", "DarkTeal", "CelestialBlue", "AzureBlue", "MidnightBlue", or "CoolGrey", defaults to 'CoolGrey'
#' @param zebra puts and alternating 'colour then white' theming onto the table, based on the selected colouring (defaults to `F`)
#' @param highlight a numeric vector indicating which rows are to receive a colour highlight, based on the selected colouring (defaults to `NULL` giving no highlighted rows)
#' @param font_family string containing the font family to apply to the table. Default "Barlow", otherwise "sans".
#' @param ... other parameters passed through to \code{\link[flextable]{set_flextable_defaults}}
#'
#' @return a flextable class object that will display in both html and word output
#'
#' @import tidyverse flextable
#'
#' @examples
#'
#' \dontrun{
#'
#' head(mtcars, 10) %>%
#'   thekids_table(colour = "Saffron")
#'
#'
#' mtcars %>%
#'   select(cyl, mpg, hp, wt, gear) %>%
#'   group_by(cyl, gear) %>%
#'   summarise(mean_mpg = mean(mpg),
#'             mean_hp = mean(hp),
#'             mean_wt = mean(wt)) %>%
#'   thekids_table(colour = "CelestialBlue", highlight = c(4:6),
#'                 padding.left = 20, padding.right = 20)

#' }
#'
#' @export
#'

thekids_table <- function(x,
                          font.size = 10,
                          font.size.header = 11,
                          line.spacing = 1.5,
                          padding = 2.5,
                          colour = "CoolGrey",
                          zebra = FALSE,
                          highlight = NULL,
                          font_family = "Barlow",
                          ...) {

  # Check font family availability
  font_family <- check_font_family(font_family)

  # Standardise argument aliasing
  call <- match.call()
  std_call <- standardise_args(call)
  if (!identical(names(call), names(std_call))) {
    return(eval(std_call, parent.frame()))
  }

  # Check colours are available
  if (!colour %in% names(thekids_palettes$primary)) {
    stop(sprintf("Invalid colour. Choose from: %s",
                 paste(shQuote(names(thekids_palettes$primary)), collapse = ", ")))
  }

  # Check zebra vs highlight
  if (zebra && !is.null(highlight)) {
    stop("Cannot use both zebra striping and highlight rows. Choose one.")
  }

  # Check if knitr_kable class object is parsed
  if (inherits(x, "knitr_kable")) {
    stop("Objects of class 'knitr_kable' are not yet supported. Please convert to a dataframe or flextable first.")
  }

  table_out <- table_coerce(x) # Coerce x to flextable

  # Select appropriate theming function
  theme_fun <- if (zebra) {
    function(x) table_zebra(x, colour)
  } else if (!is.null(highlight)) {
    function(x) table_highlight(x, colour, highlight)
  } else {
    function(x) table_non_zebra(x, colour)
  }

  table_out <- table_out %>%
    theme_fun() %>%
    flextable::fontsize(size = font.size, part = "all") %>%
    flextable::fontsize(size = font.size.header, part = "header") %>%
    flextable::font(fontname = font_family, part = "all") %>%
    flextable::line_spacing(space = line.spacing, part = "all") %>%
    flextable::padding(padding = padding, part = "all") %>%
    flextable::color(color = "white", part = "header") %>%
    flextable::color(color = "#111921", part = "body") %>%
    flextable::hline_top(part = "all") %>%
    flextable::hline_bottom(part = "all") %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::autofit()

  return(table_out)
}
