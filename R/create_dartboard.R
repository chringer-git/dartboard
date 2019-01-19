#' Create a dartboard with Standard measurments.
#'
#' @return A list object containing data.table objects describing the dimesions of
#'         the dartboard. These are stored in data.tables called bed_thetas, bed_values,
#'         bull_values, outer_ring_values
#' @export
create_dartboard <- function() {

  # Constants for labels and angles of beds in Radians.
  bed_labels <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14,
                  11, 8, 16, 7, 19, 3, 17, 2, 15, 10)
  bed_mid_theta <- (2 * pi / 20) * (c(0:19))
  bed_lower_theta <- bed_mid_theta - (pi/20)
  bed_upper_theta <- bed_mid_theta + (pi/20)
  bed_thetas <- data.table::data.table(
    "bed_labels" = as.character(bed_labels),
    "bed_value" = as.integer(bed_labels),
    "bed_lower_theta" = bed_lower_theta,
    "bed_mid_theta" = bed_mid_theta,
    "bed_upper_theta" = bed_upper_theta
    )

  # Distances from center cork for each scoring value of the bed
  # this is a vector from center out for ("lower single", "treble",
  # "upper single", "double") in inches...this is a British game is it not?
  # I used the following measurements in mm and converted into Standard rounded
  # to the nearest 1/16th of an inch.
  # TODO: Put website here for reference.
  min_radius    <- c(0 +   5/8, 3 + 13/16, 4 +  1/4, 6 +  5/16)
  max_radius    <- c(3 + 13/16, 4 +   1/4, 6 + 5/16, 6 + 11/16)
  aim_radius    <- (min_radius + max_radius) / 2
  bed_multipliers <- c(1, 3, 1, 2)
  bed_values      <- data.table::data.table(
    "bed_multiplier" = bed_multipliers,
    "bed_multiplier_label" = c("Single", "Treble", "Single", "Double"),
    "min_radius" = min_radius,
    "max_radius" = max_radius,
    "aim_radius" = aim_radius
    )

  # Distances and values for bull and double bull
  min_radius <- c(0,   1/4)
  max_radius <- c(1/4, 5/8)
  aim_radius <- c(0,   0)
  bull_value   <- c(50,  25)
  bull_values  <- data.table::data.table(
    "bull_value" = bull_value,
    "bull_label" = c("Double Bull", "Single Bull"),
    "min_radius" = min_radius,
    "max_radius" = max_radius,
    "aim_radius" = aim_radius
    )

  # Distances of outer rign of board. For drawing purposes
  outer_ring_values <- data.table::data.table(
    "outer_ring_value" = 0,
    "min_radius" = 6 + 11/16,
    "max_radius" = 9,
    "label_radius" = (6 + 7/8 + 9) / 2
    )

  # Put all in a list
  dart_board <- list()
  dart_board$bed_thetas <- bed_thetas
  dart_board$bed_values <- bed_values
  dart_board$bull_values <- bull_values
  dart_board$outer_ring <- outer_ring_values

  # Return dart_board object
  dart_board

}
