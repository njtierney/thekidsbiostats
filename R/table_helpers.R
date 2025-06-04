#' Count the number of rows in a table object
#'
#' Used internally to determine the number of rows in tbl_summary, flextable, or data.frame objects.
#'
#' @param x A table-like object
#' @return An integer indicating the number of rows
#' @noRd
table_nrow <- function(x){
  n_rows <- if (inherits(x, c("tbl_summary", "tbl_merge", "tbl_stack", "tbl_regression"))) {
    nrow(tidyr::as_tibble(x))
  } else if(inherits(x, "flextable")){
    nrow_part(x, part = "header") + nrow_part(x, part = "body") + nrow_part(x, part = "footer")
  } else {
    nrow(x)
  }

  return(n_rows)
}



#' Apply base theming to a flextable
#'
#' Internal function for setting header, footer, and body background colors,
#' with optional row highlighting.
#'
#' @param x A flextable object
#' @param header_bg Named vector of background colors for header rows (`odd`, `even`)
#' @param footer_bg Named vector of background colors for footer rows (`odd`, `even`)
#' @param body_bg Named vector of background colors for body rows (`odd`, `even` or `highlight`, `other`)
#' @param highlight Optional integer vector of row indices to highlight
#'
#' @return A styled flextable object
#' @noRd
table_theme <- function(x,
                        header_bg,
                        footer_bg,
                        body_bg,
                        highlight = NULL) {
  stopifnot(inherits(x, "flextable"))

  h_n <- nrow_part(x, "header")
  f_n <- nrow_part(x, "footer")
  b_n <- nrow_part(x, "body")

  x <- border_remove(x)
  x <- align(x, align = "center", part = "header")

  # Header formatting
  if (h_n > 0) {
    even <- seq_len(h_n) %% 2 == 0
    odd <- !even
    x <- bg(x, i = which(odd), bg = header_bg["odd"], part = "header")
    x <- bg(x, i = which(even), bg = header_bg["even"], part = "header")
    x <- bold(x, part = "header")
  }

  # Footer formatting
  if (f_n > 0) {
    even <- seq_len(f_n) %% 2 == 0
    odd <- !even
    x <- bg(x, i = which(odd), bg = footer_bg["odd"], part = "footer")
    x <- bg(x, i = which(even), bg = footer_bg["even"], part = "footer")
    x <- bold(x, part = "footer")
  }

  # Body formatting
  if (b_n > 0) {
    if (!is.null(highlight)) {
      other <- setdiff(seq_len(b_n), highlight)
      x <- bg(x, i = highlight, bg = body_bg["highlight"], part = "body")
      x <- bg(x, i = other, bg = body_bg["other"], part = "body")
    } else {
      even <- seq_len(b_n) %% 2 == 0
      odd <- !even
      x <- bg(x, i = which(odd), bg = body_bg["odd"], part = "body")
      x <- bg(x, i = which(even), bg = body_bg["even"], part = "body")
    }
  }

  x <- align_text_col(x, align = "left", header = TRUE)
  x <- align_nottext_col(x, align = "right", header = TRUE)
  x
}

#' Zebra-themed table styling
#'
#' Applies alternating body striping to a flextable with Kids palette colors.
#'
#' @param x A flextable object
#' @param colour A valid colour name from `thekids_palettes$primary`
#'
#' @return A styled flextable
#' @noRd
table_zebra <- function(x, colour) {
  table_theme(x,
              header_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              footer_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              body_bg   = c("odd" = thekids_palettes$tint50[[colour]], "even" = "transparent"))
}


#' Highlight specific rows in a flextable
#'
#' Applies row-specific highlighting to the body of a flextable.
#'
#' @param x A flextable object
#' @param colour A valid colour name from `thekids_palettes$primary`
#' @param highlight Integer vector of row indices to highlight
#'
#' @return A styled flextable
#' @noRd
table_highlight <- function(x, colour, highlight) {
  # Row indexing check
  n_rows <- table_nrow(x)
  if (!is.null(highlight) && length(highlight) > 0) {
    if (max(highlight) > n_rows) {
      stop(sprintf("You are trying to highlight a row (%s) beyond the number of rows in the table (%d).",
                   paste(highlight, collapse = ", "),
                   n_rows))
    }
  }

  table_theme(x,
              header_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              footer_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              body_bg   = c("highlight" = thekids_palettes$tint50[[colour]], "other" = "transparent"),
              highlight = highlight)
}


#' Plain non-zebra table styling
#'
#' Applies header color but no body striping or highlighting.
#'
#' @param x A flextable object
#' @param colour A valid colour name from `thekids_palettes$primary`
#'
#' @return A styled flextable
#' @noRd
table_non_zebra <- function(x, colour) {
  table_theme(x,
              header_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              footer_bg = c("odd" = thekids_palettes$primary[[colour]], "even" = "transparent"),
              body_bg   = c("odd" = "transparent", "even" = "transparent"))
}


#' Coerce various table types to a flextable
#'
#' Converts gtsummary, gt_tbl, or data.frame objects into a flextable.
#'
#' @param x A table-like object
#'
#' @return A flextable object
#' @noRd
table_coerce <- function(x) {
  if(any(class(x) %in% c("flextable"))){
    table_out <- x
  } else if(any(class(x) %in% c("gtsummary"))){
    table_out <- x %>%
      gtsummary::as_flex_table()
  } else if(any(class(x) %in% c("gt_tbl"))){
    table_out <- x %>%
      data.frame %>%
      flextable::flextable()
  }
  else {
    table_out <- x %>%
      flextable::flextable()
  }
}


#' Validate and fallback font family
#'
#' Checks whether the specified font is available on the system. If not,
#' returns a fallback font ("sans").
#'
#' @param font_family A character string specifying a font name
#' @param fallback_family Fallback font in case specified font not available on the system (default "sans").
#'
#' @return A character string of the validated or fallback font
#' @noRd
check_font_family <- function(font_family, fallback_family = "sans") {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    matched <- systemfonts::match_fonts(font_family)
    if (!is.na(matched$path)) return(font_family)
  }
  fallback_family
}
