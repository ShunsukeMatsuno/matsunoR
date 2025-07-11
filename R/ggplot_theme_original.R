#' Custom ggplot2 theme with clean aesthetics
#'
#' A minimalist ggplot2 theme with white background, black borders, and no grid lines.
#' The theme removes axis text and ticks for a clean appearance and uses custom fonts
#' when available.
#'
#' @param base_size Base font size in points. Default is 11.
#' @param base_family Base font family. Default is "".
#' @param base_line_size Base line size, scaled relative to base_size. Default is base_size/22.
#' @param base_rect_size Base rectangle size, scaled relative to base_size. Default is base_size/22.
#'
#' @return A ggplot2 theme object that can be added to plots.
#'
#' @details
#' The theme features:
#' \itemize{
#'   \item White panel background with grey20 border
#'   \item No major or minor grid lines
#'   \item Y-axis title displayed horizontally (angle = 0)
#'   \item Black axis lines
#'   \item No axis text or ticks for minimal appearance
#'   \item Custom font family via global FONT variable (if available)
#'   \item Bold white text on grey20 background for facet strips
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_matsuno()
#'
#' # With facets
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   facet_wrap(~cyl) +
#'   theme_matsuno()
#'
#' # Custom base size
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_matsuno(base_size = 14)
#'
#' @note This theme expects a global variable FONT to be defined for custom fonts.
#'   If FONT is not defined, the theme will fall back to the default font.
#'
#' @export
theme_matsuno <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  theme_grey(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ), panel.border = element_rect(
        fill = NA,
        colour = "grey20"
      ), panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5)),
      strip.text.x = element_text(size = 11, color = "white", face = "bold"),
      strip.background = element_rect(fill = "grey20"), complete = TRUE
    ) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
      axis.line = element_line(colour = "black"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(family = if (exists("FONT")) FONT else "")
    )
}

# # test
# pacman::p_load(palmerpenguins, ggplot2)
# ggplot(penguins, aes(body_mass_g, bill_length_mm))+ # this is the data
#   geom_point() +
#   facet_wrap(~species) +
#   theme_matsuno()
