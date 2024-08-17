#' Checks and changes the format of input data to all routing functions
#' @param to_coords Whether to convert input sf dataframes to normal dataframes
#' holding coordinates
#' @noRd
prepare_input <- function(input, to_coords = TRUE, len = NULL) {
  assert_that(is_geometry_type(input, "POINT"))
  input <- sf::st_geometry(input)

  if (has_crs(input)) {
    input <- sf::st_transform(input, 4326)
  } else {
    input <- sf::st_set_crs(input, 4326)
  }

  if (to_coords) {
    input <- sf::st_coordinates(input)[, c("X", "Y"), drop = FALSE]
  }

  if (!is.null(len)) {
    if (nrow(input) == 1) {
      input <- replicate(len, input, simplify = FALSE)
      input <- do.call(rbind, input)
    }

    if (nrow(input) != len && len != 1L) {
      abort(
        c(
          "x" = "Datasets have non-matching number of rows.",
          "!" = paste(
            "{.var src} and {.var dst} must have either one row",
            "or the number of rows of the other dataset."
          ),
          "i" = "Got datasets with {.val {nrow(input)}} and {.val {len}} rows."
        ),
        class = "input_match_error"
      )
    }
  }

  input
}
