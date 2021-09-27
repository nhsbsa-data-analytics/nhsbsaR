#' Theme an NHSBSA ggplot
#'
#' @description
#' Theme an NHSBSA ggplot. We would like to take several steps to brand a
#' standard ggplot to an NHSBSA looking chart. These include:
#'
#' * Make the font of any text "sans" or another `family` parameter.
#' * Colour any text NHS Black
#' * Colour background NHS White
#' * Make the title and axis titles bold
#' * Left align the subtitle
#' * Move the legend to the top with no background or key background
#' * Remove axis titles (but set them bold incase they are added later). Note
#'   that to add axis titles you must first add the element back in like this:
#'   `ggplot2::theme(axis.title.x = ggplot2::element_text()) + ggplot2::xlab()`
#' * Colour axis lines NHS Mid Grey
#' * Remove axis ticks
#' * Remove all gridlines
#' * Colour the background of the plot white
#' * Colour background NHS White
#' * For facets, bold and left align the titles and colour the panel background
#    white
#'
#' @param family, String font family to use (this must be installed and loaded).
#'   Default value is "sans" (e.g. "TT Arial").
#'
#' @return ggplot
#'
#' @examples
#' # Standard usage
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut)) +
#'   nhsbsR::theme_nhsbsa_gg()
#'
#' # Add an axis label
#' ggplot2::ggplot(data = ggplot2::diamonds) +
#'   ggplot2::geom_bar(mapping = ggplot2::aes(x = cut)) +
#'   nhsbsR::theme_nhsbsa_gg() +
#'   ggplot2::theme(axis.title.x = ggplot2::element_text()) +
#'   ggplot2::xlab("Cut")
#'
#' @export
theme_nhsbsa_gg <- function(plot, family = "sans") {

  ggplot2::theme(

    # Make the font of any text "sans" or another `family` parameter,
    # colour any text NHS Black and make it size 12
    text = ggtext::element_textbox_simple(
      color = "#231f20", 
      family = family, 
      size = 12,
      halign = 0.5
    ),

    # Colour background NHS White
    panel.background = ggplot2::element_rect(fill = "#FFFFFF"),

    # Make all titles bold (hack to make axis and legend titles bold if they
    # are added later)
    title = ggtext::element_textbox_simple(face = "bold"),

    # Un-bold and left align the subtitle
    plot.subtitle = ggtext::element_textbox_simple(
      face = "plain", 
      halign = NULL
    ),

    # Un-bold the caption
    plot.caption = ggplot2::element_text(face = "plain"),

    # Move the legend to the top with no background or key background
    # Note: the legend may need manual tweaking based on plot coordinates
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),

    # Remove axis ticks and colour axis lines NHS Mid Grey
    axis.text.x = ggtext::element_textbox_simple(margin = ggplot2::margin(t = 0)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "#768692"),

    # Remove all gridlines
    panel.grid = ggplot2::element_blank(),

    # For facets, bold and left align the titles and colour the panel background
    # white
    strip.text = ggtext::element_textbox_simple(face = "bold", halign = NULL),
    strip.background = ggplot2::element_rect(fill = "#FFFFFF")

  )

}


#' Theme an NHSBSA highcharter plot
#'
#' @description
#' Theme an NHSBSA highcharter plot. We would like to take several steps to
#' brand a standard highcharter plot to an NHSBSA looking chart. These include:
#'
#' * Make the font of any text "TT Arial" or another `family` parameter.
#' * Colour any text NHS Black
#' * Colour background NHS White
#' * Make the title and axis titles bold
#' * Move the legend to the top and set the hover colour to NHS Mid Grey
#' * Colour axis ticks and lines NHS Mid Grey
#' * Remove yaxis gridlines (x axis has no gridlines by default)
#' * Add credits (Note: Not working at the moment)
#'
#' @param family, String font family to use (this must be installed and loaded).
#'   Default value is "TT Arial"
#'
#' @return
#' @export
theme_nhsbsa_hc <- function(family = "TT Arial") {

  highcharter::hc_theme(

    chart = list(

      # Make the font of any text "TT Arial" or another `family` parameter and
      # colour any text NHS Black
      style = list(fontFamily = family, color = "#231f20"),

      # Colour background NHS White
      backgroundColor = "#FFFFFF"

    ),

    # Make the title bold
    title = list(style = list(fontWeight = "bold")),

    # Left align the subtitle
    subtitle = list(align = "left"),

    # Move the legend to the top and set the hover colour to NHS Mid Grey
    legend = list(
      verticalAlign = "top",
      itemHoverStyle = list(color = "#768692")
    ),

    xAxis = list(

      # Bold xaxis title
      title = list(style = list(fontWeight = "bold")),

      # Colour xaxis ticks and lines NHS Mid Grey
      lineColor = "#768692",
      tickColor = "#768692"

    ),

    yAxis = list(

      # Bold yaxis title
      title = list(style = list(fontWeight = "bold")),

      # Colour yaxis ticks and lines NHS Mid Grey
      lineColor = "#768692",
      tickColor = "#768692",

      # Remove yaxis gridlines
      gridLineWidth = 0

    )#,


    #credits = list(
    #  enabled = TRUE
    #)
  )

}
