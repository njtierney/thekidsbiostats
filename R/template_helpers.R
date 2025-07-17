#' Allows modifying text within .html and .qmd report files
#'
#' Internal function for modify a .qmd file
#'
#' @param lines The lines read from the qmd file.
#' @param title The report Title
#' @param subtitle The report Subtitle
#' @param author The name of the author
#' @param affiliation The affiliation of the author
#' @param include_reproducibility Boolean controlling whether the Reproducibility Information Section is included.
#' @return Modified lines to write out to new qmd file
#' @noRd
update_qmd_template <- function(lines, title = NULL, subtitle = NULL,
                                author = NULL, affiliation = NULL,
                                include_reproducibility = TRUE) {

  # Title
  if (!is.null(title)) {
    lines <- sub('^title:.*$', paste0('title: "', title, '"'), lines)
  }

  # Subtitle
  if (!is.null(subtitle)) {
    lines <- sub('^subtitle:.*$', paste0('subtitle: "', subtitle, '"'), lines)
  }

  # Author name (usually inside 'author:\n  - name: "J Smith"')
  if (!is.null(author)) {
    lines <- sub('^(\\s*)name:.*$', paste0('\\1name: "', author, '"'), lines)
  }

  # Affiliation (multi-line block)
  if (!is.null(affiliation)) {
    aff_index <- grep("^\\s*affiliation:", lines)

    if (length(aff_index) == 1 && aff_index + 1 <= length(lines)) {
      lines[aff_index + 1] <- paste0('  - "', affiliation, '"')
    }
  }

  # Remove reproducibility section if needed
  if (!include_reproducibility) {
    repro_start <- grep("^## Reproducibility Information", lines)
    if (length(repro_start) > 0) {
      # Look for the end of the ```{r} block
      code_ends <- grep("^```\\s*$", lines)
      block_end <- code_ends[code_ends > repro_start][1]

      if (!is.na(block_end)) {
        lines <- lines[-(repro_start:block_end)]
      }
    }
  }

  return(lines)
}


#' Internal function for modify a .html file
#'
#' @param lines The lines read from the html file.
#' @param title The report Title
#' @param subtitle The report Subtitle
#' @param author The name of the author
#' @param affiliation The affiliation of the author
#' @param include_reproducibility Boolean controlling whether the Reproducibility Information Section is included.
#'
#' @return Modified lines to write out to new qmd file
#' @noRd
update_html_template <- function(lines, title, subtitle, author, affiliation, include_reproducibility) {
  # Replace the line with the title
  if (!is.null(title)) {
    lines <- sub('<h1 class="title">.*?</h1>',
                      sprintf('<h1 class="title">%s</h1>', title),
                      lines)
  }

  if (!is.null(subtitle)) {
    lines <- sub('<p class="subtitle lead">.*?</p>',
                      sprintf('<p class="subtitle lead">%s</p>', subtitle),
                      lines)
  }


  # Replace the line with the author
  if (!is.null(author)) {
    lines <- sub(
      pattern = '<p class="author">.*?</p>',
      replacement = sprintf('<p class="author">%s</p>', author),
      lines
    )
  }

  # Replace the line with the author
  if (!is.null(affiliation)) {
    lines <- sub(
      '<p class="affiliation">.*?</p>',
      sprintf('<p class="affiliation">%s</p>', affiliation),
      lines
    )
  }

  if (!include_reproducibility) {
    # Remove reproducible info block (assumes it's marked with <!-- START REPRO --> and <!-- END REPRO -->)
    start <- grep("<!-- START REPRODUCIBILITY -->", lines)
    end <- grep("<!-- END REPRODUCIBILITY -->", lines)
    if (length(start) == 1 && length(end) == 1 && start < end) {
      lines <- lines[-(start:end)]
    }
  }

  return(lines)

}

render_preview <-  function(html_lines){

  # Save to temp file
  new_html <- tempfile(fileext = ".html")
  writeLines(html_lines, new_html)

  return(new_html)

}
