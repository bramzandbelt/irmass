#' Get default ggplot theme settings to be used in this project
#'
get_theme_standards <- function() {
  title_font_size <- 10
  subtitle_font_size <- 8
  label_font_size <- 5

  ggplot2::theme(
    # Plot elements
    plot.title = ggplot2::element_text(face = "bold", size = title_font_size, hjust = 0.5),
    # Axis elements
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = label_font_size),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = subtitle_font_size),
    # Legend elements
    legend.background = ggplot2::element_rect(),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = label_font_size),
    legend.title = ggplot2::element_text(size = subtitle_font_size),
    # Panel elements
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    # Facetting elements
    strip.background = ggplot2::element_rect(),
    strip.text = ggplot2::element_text(),
    panel.spacing = ggplot2::unit(0.01,"in")
  )

}



th <-
  theme(

  )
