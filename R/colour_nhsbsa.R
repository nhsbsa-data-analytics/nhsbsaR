#' Check input colour is correct built-in R recognisable
#' color names or HEX format
#'
#' @param x String type of colour palettes
#'
#' @return boolean
#' @export
#'
#' @examples
#' is_color(c("red", "#FFFFFF"))
is_color <- function(x) {
  x %in% grDevices::colors() |
    grepl(
      "^#(\\d|[a-f]){6,8}$",
      x,
      ignore.case = TRUE
    )
}



#' Check input color is pre-defiend NHS colours
#'
#' @param x String type of NHSRtheme colour palettes
#'
#' @return boolean
#' @export
#'
#' @examples
#' is_nhs_color(c("DarkBlue","Blue","BrightBlue"))
is_nhs_color <- function(x) {
  x %in% names(NHSRtheme::get_nhs_colours())
}





#' Generate NHSBSA colour palette
#'
#' Generate an accessible and appropriate colour palette for NHSBSA charts that
#' is inline with the NHS identity. Currently supports up to 8 colours.
#'
#' Custom colour can also be defined using a vector of NHS Identity colours with
#' NHSRtheme::get_nhs_colours() or any valid R color, e.g. hex code or named color.
#'
#' @param palette String type of colour palette. Default is NA, otherwise
#'   should be one one `c("gender", "gradient", "highlight")`.
#'   Custom colour can also be defined using  `NHSRtheme::get_nhs_colours()`
#' @param reverse Boolean, to reverse the palette. Default is FALSE.
#'
#' @return Colour palette
#'
#' @examples
#' palette_nhsbsa()
#' palette_nhsbsa(palette = "gender")
#' palette_nhsbsa(palette = "gradient")
#' palette_nhsbsa(palette = "highlight")
#' palette_nhsbsa(palette = NHSRtheme::get_nhs_colours(c("Blue", "AquaGreen")))
#' palette_nhsbsa(palette = c("red","blue","green"))
#' palette_nhsbsa(palette = c("red","blue", "#4444ffcc"))
#' palette_nhsbsa(reverse = TRUE)
#'
#' @export
palette_nhsbsa <- function(palette = NA,
                           reverse = FALSE) {

  color_names <- c()


  if (any(is.na(palette))) {
    color_names <- NHSRtheme::get_nhs_colours(c(
      "DarkBlue", "Orange", "LightBlue", "AquaGreen",
      "Yellow", "BrightBlue", "Red", "Pink"))

  } else if (all(is_color(palette))) {
    color_names <- palette

  } else if (all(is_nhs_color(palette))) {
    color_names <- NHSRtheme::get_nhs_colours(palette)

  } else if (palette == "gender") {
    color_names <- NHSRtheme::get_nhs_colours(c("Pink","LightBlue"))

  } else if (palette == "gradient") {
    color_names <- c("#FFFFFF", NHSRtheme::get_nhs_colours("DarkBlue"))

  } else if (palette == "highlight") {
    color_names <- NHSRtheme::get_nhs_colours(c("MidGrey", "DarkBlue"))

  } else {
    stop("Invalid palette specified.")
  }

  # Reverse names if necessary
  if (reverse) {
    color_names <- rev(color_names)
  }

  return(unname(color_names))
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
