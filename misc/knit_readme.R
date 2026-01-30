knit_readme <- function(input, ...) {
  # Output file is input file with .md extension
  output <- sub("\\.[R|r]md$", ".md", input)
  
  # Render with litedown
  if (requireNamespace("litedown", quietly = TRUE)) {
    litedown::fuse(input, output = output)
  } else {
    stop("litedown package is required but not installed.")
  }
  
  # Read the generated file
  lines <- readLines(output)
  
  # Strip YAML (lines between first pair of ---)
  delims <- which(lines == "---")
  if (length(delims) >= 2 && delims[1] == 1) {
    # Remove everything up to and including the second delimiter
    # Also remove any empty lines immediately following the header
    start_idx <- delims[2] + 1
    while (start_idx <= length(lines) && trimws(lines[start_idx]) == "") {
      start_idx <- start_idx + 1
    }
    
    if (start_idx <= length(lines)) {
      lines <- lines[start_idx:length(lines)]
    } else {
      lines <- character(0)
    }
  }
  
  # Write back
  writeLines(lines, output)
  message("Rendered ", input, " to ", output, " (YAML stripped).")
  return(output)
}
