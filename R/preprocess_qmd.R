# Internal function to preprocess Quarto .qmd files
.preprocess_qmd <- function(file_path) {
  # Load the base font from options
  base_family <- getOption("thekidsbiostats.font", "Barlow")

  # Read the file
  content <- readLines(file_path)

  # Update the mainfont argument in the YAML header
  updated_content <- gsub(
    pattern = "mainfont: .*",
    replacement = paste0("mainfont: '", base_family, "'"),
    x = content
  )

  # Write the updated content back to the file
  writeLines(updated_content, file_path)
}
