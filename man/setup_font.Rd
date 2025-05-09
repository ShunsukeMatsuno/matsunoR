% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_font.R
\name{setup_font}
\alias{setup_font}
\title{Set up Libertinus Serif font for use in R graphics using TeX Live/MiKTeX}
\usage{
setup_font()
}
\value{
No return value, called for side effects (registering fonts).
}
\description{
This function finds the Libertinus Serif font family within your existing
TeX Live or MiKTeX distribution using \code{kpsewhich} and registers it for use
with the \code{showtext} package in R.
}
\details{
It assumes you have a working TeX distribution with the \code{libertinus-fonts}
package installed and that the \code{kpsewhich} command is accessible in your PATH.
}
\examples{
\dontrun{
# Set up the Libertinus font from TeX Live/MiKTeX
try(setup_font()) # Wrap in try in case fonts/kpsewhich aren't found

# Check if font was added
if ("Libertinus Serif" \%in\% sysfonts::font_families()) {
  FONT_FAMILY <- "Libertinus Serif"

  # Use in a plot (requires ggplot2)
  # library(ggplot2)
  # p <- ggplot(mtcars, aes(mpg, hp)) +
  #   geom_point() +
  #   ggtitle("Plot using Libertinus Serif") +
  #   theme_minimal(base_family = FONT_FAMILY)
  #
  # # Ensure showtext is active for rendering
  # showtext::showtext_begin()
  # print(p)
  # showtext::showtext_end()

} else {
  message("Libertinus Serif font could not be set up.")
}
}
}
