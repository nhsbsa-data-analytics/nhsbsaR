# Example code for chart_hc_imd_bar

# create the test data frame
df_imd_data <- data.frame(
  IMD = c(1,2,3,4,5,6,7,8,9,10),
  VALUE = sample(100000:200000, size = 10 , replace = TRUE)
)

# create chart object
chart_hc_imd_bar(
  df = df_imd_data,
  imd_col = "IMD",
  value_col = "VALUE",
  metric_def = "Example cost values",
  highlight_core20 = TRUE,
  show_lbl = FALSE,
  value_dp = 1,
  value_prefix = "£",
  order_of_magnitude = "k"
)
