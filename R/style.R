#' ggplot2 theme
#'
#' @param base_size theme base size
#' @return ggplot2 theme


theme_epsilon <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(

      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),

      panel.grid.minor = element_blank(),
      panel.border = element_blank(),

      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),

      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),

      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}
