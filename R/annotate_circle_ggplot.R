#' Function to help draw a cirle in ggplot2.
#'
#' I found it on the internet, I need to give credit at some point
#'
#' @param radius is the radius
#' @param x_center is the x0 coordiante
#' @param y_center is the y0 coordiante
#' @param color is the color of the line
#' @param fill is whether to fill with a color
#'
#' @return A ggplot2 line object that is a circle.
annotate_circle_ggplot <- function(radius, x_center, y_center, color = "black",
                                   fill = NA, ...) {

  # Load ggplot2
  library(ggplot2)

  # Set x and y values and and create annotation.
  x <- x_center + radius * cos(seq(0, pi, length.out=100))
  ymin <- y_center + radius * sin(seq(0, -pi, length.out=100))
  ymax <- y_center + radius * sin(seq(0, pi, length.out=100))
  ggplot2::annotate("ribbon", x = x, ymin = ymin, ymax = ymax, color = color,
                    fill=fill, ...)

}
