#' Generate NHSBSA colour palette
#'
#' Generate an accessible and appropriate colour palette for NHSBSA charts that
#' is inline with the NHS identity. Currently supports up to 8 colours.
#'
#' @param palette, String type of colour palette. Default is NA, otherwise
#'   should be one one `c("gender", "gradient", "highlight")`.
#'   Custom colour can also be defined using  `NHSRtheme::get_nhs_colours()`
#' @param reverse Boolean, to reverse the palette. Default is FALSE.
#'
#' @return Colour palette
#'
#' @examples
#' nhsbsaR::palette_nhsbsa()
#' nhsbsaR::palette_nhsbsa(palette = "gender")
#' nhsbsaR::palette_nhsbsa(palette = "gradient")
#' nhsbsaR::palette_nhsbsa(palette = "highlight")
#' #' nhsbsaR::palette_nhsbsa(palette = NHSRtheme::get_nhs_colours(c("Blue", "AquaGreen")))
#' @export
palette_nhsbsa <- function(palette = NA, reverse = FALSE) {
  # Check if palette is one of the named palettes or a custom color palette
  if (is.character(palette) & length(palette) == 1) { # Ensure palette is a single string
    if (palette == "gender") {
      names <- c("Pink", "LightBlue")
    } else if (palette == "gradient") {
      names <- c("White", "DarkBlue")
    } else if (palette == "highlight") {
      names <- c("MidGrey", "DarkBlue")
    }
  } else if (is.character(palette) & length(palette) > 1) {
    # If palette is not one of the named palettes, treat as custom colours
    return(unname(palette))
  } else if (is.na(palette)) {
    # Base on the Wong palette
    names <- c(
      "DarkBlue", "Orange", "LightBlue", "AquaGreen",
      "Yellow", "BrightBlue", "Red", "Pink"
    )
  } else {
    stop("Invalid palette specified.")
  }

  # Reverse names if necessary
  if (reverse) {
    names <- rev(names)
  }

  # Get the NHS identity hex colour for each name (with custom colours added)
  colours <- c(NHSRtheme::get_nhs_colours(), "White" = "#FFFFFF")[names]

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
#' @return colour palette
#'
#' @examples
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_point(
#'     mapping = ggplot2::aes(x = carat, y = price, col = carat)
#'   ) +
#'   nhsbsaR::scale_colour_nhsbsa(palette = "gradient")
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
#' @return colour palette
#'
#' @examples
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut, fill = cut)) +
#'   nhsbsaR::scale_fill_nhsbsa()
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut, fill = cut)) +
#'   nhsbsaR::scale_fill_nhsbsa(palette = "gradient", n_discrete = 5)
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
