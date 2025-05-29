#' Find the full path to a font file using kpsewhich
#'
#' Internal helper function to locate a font file within the TeX distribution.
#'
#' @param filename The base name of the font file (e.g., "LibertinusSerif-Regular.otf")
#' @return The full path to the font file if found, otherwise NULL.
#' @noRd
find_tex_font <- function(filename) {
  # First, check if kpsewhich is available
  kpsewhich_path <- Sys.which("kpsewhich")
  if (kpsewhich_path == "") {
    stop("`kpsewhich` command not found. Please ensure your TeX distribution (TeX Live / MiKTeX) is installed and its bin directory is in your system's PATH.")
  }

  # Try to find the font file
  result <- tryCatch({
    # Use system2 for better control and capturing output/status
    system2(kpsewhich_path, args = filename, stdout = TRUE, stderr = FALSE)
  }, warning = function(w) {
    # Handle cases where kpsewhich might issue a warning but still work (less common for file finding)
    # message("Warning while running kpsewhich for ", filename, ": ", conditionMessage(w))
    # Try to proceed if output was captured
    system2(kpsewhich_path, args = filename, stdout = TRUE, stderr = TRUE) # Capture stderr too if needed
  }, error = function(e) {
    # Handle errors during the execution of kpsewhich itself
    warning("Error running kpsewhich for ", filename, ": ", conditionMessage(e))
    return(NULL)
  })

  # Check if the command returned a non-empty path
  if (length(result) == 1 && nzchar(result) && !grepl("^(kpathsea:|!)", result)) {
     # Basic check for kpathsea error messages
    return(trimws(result)) # Return the cleaned path
  } else {
    warning("`kpsewhich` could not find: ", filename)
    return(NULL) # Font not found
  }
}

#' Set up Libertinus Serif font for use in R graphics using TeX Live/MiKTeX
#'
#' This function finds the Libertinus Serif font family within your existing
#' TeX Live or MiKTeX distribution using `kpsewhich` and registers it for use
#' with the `showtext` package in R.
#'
#' It assumes you have a working TeX distribution with the `libertinus-fonts`
#' package installed and that the `kpsewhich` command is accessible in your PATH.
#'
#' @return No return value, called for side effects (registering fonts).
#' @examples
#' \dontrun{
#' # Set up the Libertinus font from TeX Live/MiKTeX
#' try(setup_font()) # Wrap in try in case fonts/kpsewhich aren't found
#'
#' # Check if font was added
#' if ("Libertinus Serif" %in% sysfonts::font_families()) {
#'   FONT <- "Libertinus"
#'
#'   # Use in a plot (requires ggplot2)
#'   # library(ggplot2)
#'   # p <- ggplot(mtcars, aes(mpg, hp)) +
#'   #   geom_point() +
#'   #   ggtitle("Plot using Libertinus Serif") +
#'   #   labs(x = expression(beta)) + 
#'   #   theme_minimal(base_family = IFONT)
#'   #
#'   # print(p)
#'
#' } else {
#'   message("Libertinus Serif font could not be set up.")
#' }
#' }
#' @export
setup_font <- function() { # Renamed for clarity, or keep setup_font

  message("Attempting to locate Libertinus Serif fonts using kpsewhich...")

  # Define the font files needed for the Serif family
  fonts_to_find <- list(
    regular = "LibertinusSerif-Regular.otf",
    bold = "LibertinusSerif-Bold.otf",
    italic = "LibertinusSerif-Italic.otf",
    bolditalic = "LibertinusSerif-BoldItalic.otf"
  )

  # Find paths for each font file
  font_paths <- lapply(fonts_to_find, find_tex_font)

  # Check if all required fonts were found
  required_fonts <- c("regular", "italic") # Define minimum required
  found_all_required <- all(!sapply(font_paths[required_fonts], is.null))
  found_any <- any(!sapply(font_paths, is.null))

  if (!found_any) {
     stop("Failed to find any Libertinus Serif OTF files using kpsewhich.\n",
          "Please ensure the 'libertinus-fonts' package is installed in your TeX distribution (TeX Live/MiKTeX)\n",
          "and that `kpsewhich` is functioning correctly.")
  } else if (!found_all_required) {
     warning("Could not find all required Libertinus Serif styles (Regular, Italic). Proceeding with available fonts.")
     # Filter out NULL paths for font_add
     font_paths <- Filter(Negate(is.null), font_paths)
     if (length(font_paths) == 0) { # Double check after filtering
        stop("Failed to find usable Libertinus Serif OTF files.")
     }
  } else {
      message("Found required Libertinus Serif font styles.")
  }

  # Register the font family with sysfonts, providing the found paths
  sysfonts::font_add(family = "Libertinus", regular = font_paths$regular)
  sysfonts::font_add(family = "Libertinus_Italic", regular = font_paths$italic)
  # Enable showtext to use the newly added fonts globally in graphics devices
  showtext::showtext_auto()
  message("`showtext` enabled automatically. Use `FONT='Libertinus'` and `IFONT='Libertinus_Italic'` in ggplot2 themes.")

  # Optionally increase DPI for better rendering with showtext
  # showtext::showtext_opts(dpi = 300)

  invisible(NULL) # Return NULL invisibly
}
