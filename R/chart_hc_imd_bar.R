#' Highcharts bar plot for values split by IMD decile
#'
#' @description
#' Visualisation creation function: Create highchart object showing bar plot IMD decile distribution.
#'
#' The chart will be a highchart object for a single series chart with a bar per IMD decile. The parameters
#' supplied will allow basic formatting to be applied to the chart object.
#'
#' Some chart elements (e.g. title, caption) have been excluded from the function although this can be
#' added to the highcharts object returned from this function.
#'
#'
#' @param df A data frame containing data to plot
#' @param imd_col String defining name of column within the data that holds the IMD classification (values should be 1 to 10)
#' @param value_col String defining name of column within the data that holds the values to be plotted
#' @param metric_def String for the metric being reported (will be used as y-axis title and in tooltip)
#' @param highlight_core20 Should Core20 be highlighted
#' \itemize{
#'  \item Default = TRUE
#'  \item TRUE:- the Core20 deciles (IMD = 1 or 2) will be highlighted within the output
#'  \item FALSE:- no emphasis will be applied to highlight any of the deciles
#' }
#' @param show_lbl Should data labels be shown
#' \itemize{
#'  \item Default = TRUE
#'  \item TRUE:- data labels will be included above each decile bar
#'  \item FALSE:- no data labels will be included in the output (values will still be in tooltip)
#' }
#' @param value_dp No. of decimal places to format values to
#' \itemize{
#'  \item Default = 0
#'  \item Only values between 0 and 3 are valid
#'  \item Decimal values beyond 3 points are not easy to digest and alternative reporting should be considered
#' }
#' @param value_prefix Any value prefix that should be displayed
#' \itemize{
#'  \item Default = ""
#'  \item Prefix will be included in the axis title and prior to data values (e.g. £1)
#' }
#' @param order_of_magnitude Character to identify if figures should be adjusted to specific abbreviation
#' \itemize{
#'  \item Default = ""
#'  \item k:- values will be divided by 1,000 and figures will include "k" suffix
#'  \item m:- values will be divided by 1,000,000 and figures will include "m" suffix
#'  \item bn:- values will be divided by 1,000,000,000 and figures will include "bn" suffix
#'  \item tn:- values will be divided by 1,000,000,000,000 and figures will include "tn" suffix
#' }
#'
#' @return Highchart object for bar chart showing data by IMD decile
#'
#' @example man/examples/chart_hc_imd_bar.R
#'
#' @export
chart_hc_imd_bar <- function(
    df,
    imd_col,
    value_col,
    metric_def,
    highlight_core20 = FALSE,
    show_lbl = FALSE,
    value_dp = 0,
    value_prefix = "",
    order_of_magnitude = ""
) {

# Input review --------------------------------------------------------------------------------

  # review input parameters, providing error/warning messages as required and aborting call

  # imd_col
  if(!imd_col %in% colnames(df)){
    stop(paste0("Invalid parameter value (imd_col) : Field name (",imd_col,") does not exist in supplied data"), call. = FALSE)
  }

  # value_col
  if(!value_col %in% colnames(df)){
    stop(paste0("Invalid parameter value (value_col) : Field name (",value_col,") does not exist in supplied data"), call. = FALSE)
  }

  # highlight_core20
  if(!highlight_core20 %in% c(TRUE,FALSE)){
    stop("Invalid parameter value (highlight_core20) : Must be one of TRUE/FALSE", call. = FALSE)
  }

  # show_lbl
  if(!show_lbl %in% c(TRUE,FALSE)){
    stop("Invalid parameter value (show_lbl) : Must be one of TRUE/FALSE", call. = FALSE)
  }

  # value_dp
  if(!value_dp %in% c(0,1,2,3)){
    stop("Invalid parameter value (value_dp) : Must be one of 0/1/2/3", call. = FALSE)
  }

  # order_of_magnitude
  if(!order_of_magnitude %in% c("","k","m","bn","tn")){
    stop("Invalid parameter value (order_of_magnitude) : Must be blank or one of k/m/bn/tn", call. = FALSE)
  }

  # IMD column entries (10 rows with values 1 to 10)
  # if(nrow(df) != 10){
  if(nrow(df) != 10 || !all(sort(as.integer(df[[imd_col]])),seq(1:10))){
    stop("Invalid IMD decile values : Data must contain 10 rows with IMD decile values from 1 to 10", call. = FALSE)
  }


# Data object ---------------------------------------------------------------------------------

  # define the data object based on parameters
  df <- df |>
    dplyr::select(IMD_COL := {{ imd_col }},
                  VAL_COL := {{ value_col }}
    )

  # define grouping based on parameters
  if(highlight_core20 == TRUE){
    df <- df |>
      dplyr::mutate(GRP_COL = dplyr::case_when(IMD_COL %in% c(1,2) ~ "Core20",
                                               TRUE ~ "Non-Core20")
                    )
  } else {
    df <- df |>
      dplyr::mutate(GRP_COL = metric_def)
  }

  # adjust values based on supplied order of magnitude
  if(order_of_magnitude != ""){
    df <- df |>
      dplyr::mutate(VAL_COL = VAL_COL / switch(order_of_magnitude,
                                               "k" = 1000,
                                               "m" = 1000000,
                                               "bn" = 1000000000,
                                               "tn" = 1000000000000)
                    )
  }


# Tooltip creation ----------------------------------------------------------------------------

  # create the tooltip object
  tooltip_text <- ""


  # if core20 should be highlighted add this to the tooltip
  if(highlight_core20 == TRUE){
    tooltip_text <- glue::glue("{tooltip_text}<b>Core20 Classification:</b> {{point.GRP_COL}}<br>")
  }

  # add IMD decile and value to the tooltip
  tooltip_text <- glue::glue(
    "{tooltip_text}\\
    <b>IMD decile:</b> {{point.IMD_COL}} <br>\\
    <b>{metric_def}:</b> {value_prefix}{{point.VAL_COL:,.{value_dp}f}}{order_of_magnitude}"
  )


# Axis label creation -------------------------------------------------------------------------

  # create the x-axis label
  y_axis_lbl = metric_def

  # add the prefix and order of magnitude characters (if supplied)
  if(value_prefix != "" & order_of_magnitude != ""){
    y_axis_lbl <- glue::glue("{y_axis_lbl} ({value_prefix}{order_of_magnitude})")
  } else if(value_prefix != "" & order_of_magnitude == ""){
    y_axis_lbl <- glue::glue("{y_axis_lbl} ({value_prefix})")
  } else if(value_prefix == "" & order_of_magnitude != ""){
    y_axis_lbl <- glue::glue("{y_axis_lbl} ({order_of_magnitude})")
  }


# Chart creation ------------------------------------------------------------------------------

  # create the plot object
  plt <- df |>
    highcharter::hchart(
      type = "column",
      highcharter::hcaes(
        x = IMD_COL,
        y = VAL_COL,
        group = GRP_COL
      ),
      dataLabels = list(enabled = show_lbl,
                        format = glue::glue("{value_prefix}{{point.VAL_COL:,.{value_dp}f}}{order_of_magnitude}"),
                        inside = FALSE,
                        style = list(color = "#000000")
                        )
    ) |>
    theme_nhsbsa_highchart() |>
    highcharter::hc_yAxis(title = list(text = y_axis_lbl)) |>
    highcharter::hc_xAxis(
      min = 1,
      max = 11, # Pad to ensure we can see the 10 label
      categories = c(NA,"1<br>Most<br>deprived", seq(2,9), "10<br>Least<br>deprived",""),
      labels = list(step = 1),
      title = list(text = "Deprivation decile", y=-25)
    ) |>
    highcharter::hc_credits(enabled = TRUE)  |>
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = tooltip_text
    ) |>
    highcharter::hc_legend(enabled = TRUE)


  # specific formatting when highlighting core20
  if(highlight_core20 == TRUE){
    plt <- plt |>
      highcharter::hc_xAxis(
        plotBands = list(
          list(
            label = list(text = "Core20"),
            from = 0,
            to = 2.5,
            color = "#f0f0f0"
          )
      )
    )
  }


# Return output -------------------------------------------------------------------------------

  # return the plot object
  return(plt)

}
