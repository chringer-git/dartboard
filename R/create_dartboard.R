#' Create a dartboard with SEM measurments.
#'
#' @return A list object containing data.table objects describing the dimesions of
#'         the dartboard. These are stored in data.tables called bed_angles, bed_values,
#'         bull_values, outer_ring_values
#' @export
create_dartboard <- function() {

  # load libraries
  library(data.table)

  # Constants for labels and angles of beds in Radians.
  bed_labels <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14,
                  11, 8, 16, 7, 19, 3, 17, 2, 15, 10)
  bed_mid_angle <- (2 * pi / 20) * (c(0:19))
  bed_lower_angle <- bed_mid_angle - (pi/20)
  bed_upper_angle <- bed_mid_angle + (pi/20)
  bed_angles <- data.table::data.table(
    "bed_labels" = as.character(bed_labels),
    "bed_value" = as.integer(bed_labels),
    "bed_lower_angle" = bed_lower_angle,
    "bed_mid_angle" = bed_mid_angle,
    "bed_upper_angle" = bed_upper_angle
    )

  # Distances from center cork for each scoring value of the bed
  # this is a vector from center out for ("lower single", "treble",
  # "upper single", "double") in inches...this is a British game is it not?
  min_distance    <- c(0 + 5/8, 3 + 7/8, 4 + 1/4, 6 + 1/2)
  max_distance    <- c(3 + 7/8, 4 + 1/4, 6 + 1/2, 6 + 7/8)
  aim_distance    <- (min_distance + max_distance) / 2
  bed_multipliers <- c(1, 3, 1, 2)
  bed_values      <- data.table::data.table(
    "bed_multiplier" = bed_multipliers,
    "bed_multiplier_label" = c("Single", "Treble", "Single", "Double"),
    "min_distance" = min_distance,
    "max_distance" = max_distance,
    "aim_distance" = aim_distance
    )

  # Distances and values for bull and double bull
  min_distance <- c(0,   1/4)
  max_distance <- c(1/4, 5/8)
  aim_distance <- c(0,   0)
  bull_value   <- c(50,  25)
  bull_values  <- data.table::data.table(
    "bull_value" = bull_value,
    "bull_label" = c("Double Bull", "Single Bull"),
    "min_distance" = min_distance,
    "max_distance" = max_distance,
    "aim_distance" = aim_distance
    )

  # Distances of outer rign of board. For drawing purposes
  outer_ring_values <- data.table::data.table(
    "outer_ring_value" = 0,
    "min_distance" = 6 + 7/8,
    "max_distance" = 9 + 1/8,
    "label_distance" = (6 + 7/8 + 9 + 1/8) / 2
    )

  # Put all in a list
  dart_board <- list()
  dart_board$bed_angles <- bed_angles
  dart_board$bed_values <- bed_values
  dart_board$bull_values <- bull_values
  dart_board$outer_ring <- outer_ring_values

  # Return dart_board object
  dart_board

}