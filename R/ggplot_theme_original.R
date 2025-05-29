#' ggplot2 theme original
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
                  axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1),  # y-axis title upright
                  axis.line = element_line(colour = "black"),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  text = element_text(family=FONT)
            )
}

# # test
# pacman::p_load(palmerpenguins, ggplot2)
# ggplot(penguins, aes(body_mass_g, bill_length_mm))+ # this is the data
#   geom_point() +
#   facet_wrap(~species) +
#   theme_matsuno()
