#' Set up Libertinus font for use in R graphics
#'
#' This function downloads and installs the Libertinus font family from GitHub,
#' and sets it up for use with the showtext package. It handles checking if the
#' font is already installed and only downloads when necessary.
#'
#' @return No return value, called for side effects
#' @examples
#' \dontrun{
#' # Set up the Libertinus font
#' setup_font()
#' FONT <- "Libertinus"
#'
#' # Use in a plot
#' ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   theme(text = element_text(family = FONT))
#' }
setup_font <- function() {
  # (i) Set up user fonts directory based on OS
  if (.Platform$OS.type == "windows") {
    user_fonts_dir <- file.path(Sys.getenv("USERPROFILE"), "Fonts")
  } else {
    user_fonts_dir <- file.path(Sys.getenv("HOME"), "Fonts")
  }

  if (!dir.exists(user_fonts_dir)) {
    dir.create(user_fonts_dir, recursive = TRUE)
  }

  # (ii) Get the latest release info automatically from GitHub
  # The API endpoint returns JSON data for the latest release
  release_info <- jsonlite::fromJSON("https://api.github.com/repos/alerque/libertinus/releases/latest")
  tag <- release_info$tag_name # e.g., "v7.051"
  version <- sub("^v", "", tag) # remove the "v" to get "7.051"

  # Check if the latest version is already installed
  font_dir <- file.path(user_fonts_dir, paste0("Libertinus-", version))
  if (!dir.exists(font_dir)) {
    # Construct the URL for the zip asset
    zip_url <- paste0(
      "https://github.com/alerque/libertinus/releases/download/",
      tag,
      "/Libertinus-", version, ".zip"
    )

    # Define the destination for the downloaded zip file
    zip_dest <- file.path(user_fonts_dir, paste0("Libertinus-", version, ".zip"))

    # Download the zip file (in binary mode)
    download.file(zip_url, destfile = zip_dest, mode = "wb")

    # (iii) Unzip the downloaded file into the user_fonts_dir
    # This will create a folder named "Libertinus-{version}" in the user_fonts_dir
    unzip(zip_dest, exdir = user_fonts_dir)
    unlink(zip_dest) # Delete the zip folder after unzipping
  }

  # (iv) Load the fonts using the unzipped directory.
  # The fonts are located in "static/OTF" relative to the unzipped folder.
  withr::with_dir(font_dir, {
    sysfonts::font_add("Libertinus",
      regular = "static/OTF/LibertinusSerif-Regular.otf",
      italic  = "static/OTF/LibertinusSerif-Italic.otf"
    )
  })

  # Enable showtext to use the newly added fonts
  showtext::showtext_auto()
}
