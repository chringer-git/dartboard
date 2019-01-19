#' Converts polar coordinates to cartesian coordinates.
#'
#' @param radius radius of the vector
#' @param theta angles of the vector
#'
#' @return list with converted "x" and "y" coordinates
#'
#' @export
pol_to_cart <- function(radius, theta) {

  # Set x and y
  x <- radius * cos(theta)
  y <- radius * sin(theta)

  # Return list
  cart <- list()
  cart$x <- x
  cart$y <- y

  # Return cartesian coordinates
  cart

}

#' Converts cartesian coordinates to polar coordinates.
#'
#' @param x is the "x" value on the cartesian coordinate system
#' @param y is the "y" value on the cartesian coordinate system
#'
#' @return list with converted "radius" and "theta" coordinates
#'
#' @export
cart_to_pol <- function(x, y) {

  # Set magnitude and angle.
  radius <- (x ^ 2 + y ^2) ^ (1 / 2)
  y      <- y + 1e-15
  theta  <- ifelse(x < 0,
                   atan(y / x) + pi,
                   ifelse(y < 0,
                          atan(y / x) + (2 * pi),
                          atan(y / x)))

  # Align with dartboard wedge which begins -pi/20 and 2*pi - pi/20
  if (theta >= (2 * pi) - (pi / 20)) {
    theta <- theta - (2 * pi)
  }

  # Return list
  pols <- list()
  pols$radius <- radius
  pols$theta <- theta

  # Return polar coordinates
  pols

}
