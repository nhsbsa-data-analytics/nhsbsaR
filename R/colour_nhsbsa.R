#' Generate NHSBSA colour palette
#'
#' Generate an accessible and appropriate colour palette for NHSBSA charts that
#' is inline with the NHS identity. Currently supports up to 8 colours.
#'
#' @param palette, String type of colour palette. Default is NA, otherwise
#'   should be one one `c("gender", "gradient", "highlight")`.
#' @param reverse Boolean, to reverse the palette. Default is FALSE.
#'
#' @return Colour palette
#'
#' @examples
#' nhsbsR::palette_nhsbsa()
#' nhsbsR::palette_nhsbsa(palette = "gender")
#' nhsbsR::palette_nhsbsa(palette = "gradient")
#' nhsbsR::palette_nhsbsa(palette = "highlight")
#'
#' @export
palette_nhsbsa <- function(palette = NA, reverse = FALSE) {

  # Depending on the palette, get the colour names
  if(!is.na(palette)) {

    if (palette == "gender") {
      names <- c("Pink", "LightBlue")
    }

    if (palette == "gradient") {
      names <- c("White", "DarkBlue") # White is custom
    }

    if (palette == "highlight") {
      names <- c("LightGrey", "DarkBlue") # LightGrey is custom
    }

  } else {

    # Base on the Wong palette (https://davidmathlogic.com/colorblind/)
    names <- c(
      "DarkBlue", # Instead of black
      "Orange",
      "LightBlue",
      "AquaGreen",
      "Yellow",
      "BrightBlue", # Instead of NHS Blue
      "Red",
      "Pink"
    )

  }

  # Reverse names if necessary
  if (reverse) {
    names <- rev(names)
  }

  # Get the NHS identity hex colour for each name (with a couple of custom 
  # colours added)
  colours <- c(
    nhsrtheme::get_nhs_colours(), 
    "White" = "#FFFFFF", 
    "LightGrey" = "#DDE1E4"
  )[names]

  # Return the unnamed colours
  unname(colours)

}


#' Scale colour an NHSBSA ggplot
#'
#' Scale colour an NHSBSA ggplot using the NHSBSA colour palette.
#'
#' @param palette, String type of colour palette. Default is NA, otherwise
#'   should be one one `c("gender", "gradient", "highlight")`.
#' @param n_discrete, Integer number of discrete categories (if any). Default
#'   is 0.
#' @param reverse Boolean, to reverse the palette. Default is FALSE.
#' @param ..., arguments to pass on
#'
#' @return
#'
#' @examples
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_point(
#'     mapping = ggplot2::aes(x = carat, y = price, col = carat)
#'   ) +
#'   nhsbsR::scale_colour_nhsbsa(palette = "gradient")
#'
#' @export
scale_colour_nhsbsa <- function(palette = NA,
                                n_discrete = 0,
                                reverse = FALSE,
                                ...) {

  # Load the NHSBSA colour palette
  colours <- palette_nhsbsa(palette = palette, reverse = reverse)

  # Deal with non-gradient palette
  if (is.na(palette) | palette != "gradient") {

    # Return scale colour manual
    ggplot2::scale_colour_manual(values = colours, ...)

    # Deal with gradient palette (discrete)
  } else if (n_discrete > 0) {

    # Return manual colour manual gradient
    ggplot2::scale_colour_manual(
      values = grDevices::colorRampPalette(colors = colours, ...)(n_discrete)
    )

    # Deal with gradient palette (continuous)
  } else {

    # Return the colour gradient
    ggplot2::scale_colour_gradient(
      low = colours[[1]],
      high = colours[[2]],
      na.value = palette_nhsbsa(palette = "highlight")[[1]],
      ...
    )

  }

}


#' Scale fill an NHSBSA ggplot
#'
#' Scale fill an NHSBSA ggplot using the NHSBSA colour palette.
#'
#' @param palette, String type of colour palette. Default is NA, otherwise
#'   should be one one `c("gender", "gradient", "highlight")`.
#' @param n_discrete, Integer number of discrete categories (if any). Default
#'   is 0.
#' @param reverse Boolean, to reverse the palette. Default is FALSE.
#' @param ..., arguments to pass on
#'
#' @return
#'
#' @examples
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut, fill = cut)) +
#'   nhsbsR::scale_fill_nhsbsa()
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut, fill = cut)) +
#'   nhsbsR::scale_fill_nhsbsa(palette = "gradient", n_discrete = 5)
#' @export
scale_fill_nhsbsa <- function(palette = NA,
                              n_discrete = 0,
                              reverse = FALSE,
                              ...) {

  # Load the NHSBSA colour palette
  colours <- palette_nhsbsa(palette = palette, reverse = reverse)

  # Deal with non-gradient palette
  if (is.na(palette) | palette != "gradient") {

    # Return scale fill manual
    ggplot2::scale_fill_manual(values = colours, ...)

  # Deal with gradient palette (discrete)
  } else if (n_discrete > 0) {

    # Return manual fill manual gradient
    ggplot2::scale_fill_manual(
      values = grDevices::colorRampPalette(colors = colours, ...)(n_discrete)
    )

  # Deal with gradient palette (continuous)
  } else {

    # Return the fill gradient
    ggplot2::scale_fill_gradient(
      low = colours[[1]],
      high = colours[[2]],
      na.value = palette_nhsbsa(palette = "highlight")[[1]],
      ...
    )

  }

}
