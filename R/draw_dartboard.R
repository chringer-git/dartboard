#' Draw the dartboard object, db, to actual scale using ggplot2.
#'
#' @param db a dartboard object created by dartboard::create_dartboard()
#' @return A a ggplot2 object drawing of the dartboard.
#'
#' @export
draw_dartboard <- function(db) {

  # Load libraries
  library(ggplot2)
  library(data.table)

  # Add rings to the dartboard
  plot_d <- ggplot2::ggplot() +
    lapply(X = db$bed_values$max_distance,
           FUN = function(x) annotate_circle_ggplot(radius = x,
                                                    x_center = 0,
                                                    y_center = 0)) +
    lapply(X = db$bull_values$max_distance,
           FUN = function(x) annotate_circle_ggplot(radius = x,
                                                    x_center = 0,
                                                    y_center = 0)) +
    lapply(X = db$outer_ring$max_distance,
           FUN = function(x) annotate_circle_ggplot(radius = x,
                                                    x_center = 0,
                                                    y_center = 0)) +
    ggplot2::coord_fixed()

  # Add bed dividers to the dartboard
  segs <-
    data.table::data.table(
      "x_min" = unlist(lapply(X = c(1:20),
                              FUN = function(x) pol_to_cart(
                                radius = min(db$bed_values$min_distance),
                                theta = db$bed_angles$bed_lower_angle[x])$x
                              )
                       ),
      "y_min" = unlist(lapply(X = c(1:20),
                              FUN = function(x) pol_to_cart(
                                radius = min(db$bed_values$min_distance),
                                theta = db$bed_angles$bed_lower_angle[x])$y
                              )
                       ),
      "x_max" = unlist(lapply(X = c(1:20),
                              FUN = function(x) pol_to_cart(
                                radius = max(db$bed_values$max_distance),
                                theta = db$bed_angles$bed_lower_angle[x])$x
                              )
                       ),
      "y_max" = unlist(lapply(X = c(1:20),
                              FUN = function(x) pol_to_cart(
                                radius = max(db$bed_values$max_distance),
                                theta = db$bed_angles$bed_lower_angle[x])$y
                              )
                       )
      )
  plot_d <- plot_d +
    ggplot2::geom_segment(data = segs, ggplot2::aes(x = x_min, y = y_min,
                                                    xend = x_max, yend = y_max))

  # Add labels to the each beds
  bed_labels <- data.table::data.table(
    "x" = unlist(lapply(X = c(1:20),
                        FUN = function(x) pol_to_cart(
                          radius = min(db$outer_ring$label_distance),
                          db$bed_angles$bed_mid_angle[x])$x
                        )
                 ),
    "y" = unlist(lapply(X = c(1:20),
                        FUN = function(x) pol_to_cart(
                          radius = min(db$outer_ring$label_distance),
                          theta = db$bed_angles$bed_mid_angle[x])$y
                        )
                 ),
    "label" = db$bed_angles$bed_labels)
  plot_d <- plot_d +
    ggplot2::geom_text(data = bed_labels, ggplot2::aes(x = x, y= y,
                                                       label = label))

  # clean to just have image
  plot_d <- plot_d +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  plot_d

}
