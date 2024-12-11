# Internal function to preprocess Quarto .qmd files
.preprocess_qmd <- function(file_path) {
  base_family <- getOption("thekidsbiostats.font", "Barlow")

  google_fonts_link <- paste0("https://fonts.googleapis.com/css2?family=", gsub(" ", "+", base_family), "&display=swap")

  content <- readLines(file_path)
  content <- gsub("mainfont: .*", paste0("mainfont: '", base_family, "'"), content)
  content <- gsub("@import url\\('.*'\\);", paste0("@import url('", google_fonts_link, "');"), content)

  writeLines(content, file_path)
}
