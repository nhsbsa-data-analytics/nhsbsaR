#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
#'
#' This is intended to be used with differing address strings
#' The result is a 'master' address string that contains all parts of each address
#'
#' @param df Lazy Oracle table
#' @param first_col First column
#' @param second_col Second column
#' @param merge_col Name to give merged column
#'
#' @details Requires `nhsbsaR::oracle_unnest_tokens()`.
#'
#' @examples
#' table_db %>%
#'   nhsbsaR::oracle_merge_strings(
#'     first_col = "ADDRESS_ONE",
#'     second_col = "ADDRESS_TWO",
#'     merge_col = "ADDRESS_MERGED"
#'   )
#' @returns original df with additional merged column added
#'
#' @export
oracle_merge_strings <- function(df, first_col, second_col, merge_col) {

  # Get the unique combinations we want to merge (in case there are duplicates)
  distinct_df <- df %>%
    dplyr::distinct(!!dplyr::sym(first_col), !!dplyr::sym(second_col))

  # Process columns (loop over each one as we repeat the processing)
  col_dfs <- list()
  for (col in c(first_col, second_col)) {
    col_dfs[[col]] <- distinct_df %>%
      # Get the unique values
      dplyr::distinct(!!dplyr::sym(col)) %>%
      # Tokenise
      nhsbsaR::oracle_unnest_tokens(
        col = col,
        drop = FALSE
      ) %>%
      # Give each token a rank within the string (e.g. 'CITY-1', 'CITY-2', etc)
      dplyr::group_by(!!dplyr::sym(col), TOKEN) %>%
      dplyr::mutate(TOKEN_RANK = dplyr::row_number(TOKEN_NUMBER)) %>%
      dplyr::ungroup() %>%
      # Rename the token number column
      dplyr::rename_with(.fn = ~ paste0(col, "_", .x), .cols = TOKEN_NUMBER) %>%
      # Join back to the unique combinations (handy for full_join later)
      dplyr::inner_join(
        y = distinct_df,
        copy = TRUE
      )
  }

  # Join the tokenised data together (attempt to join by TOKEN and TOKEN_RANK)
  distinct_df <-
    dplyr::full_join(
      x = col_dfs[[first_col]],
      y = col_dfs[[second_col]],
      copy = TRUE
    )

  # Pull the DB connection
  db_connection <- df$src$con

  # Build SQL Query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "WITH LT AS
    (
      SELECT ",
        dplyr::sql(first_col), ", ",
        dplyr::sql(second_col), ", ",
        dplyr::sql(first_col), "_TOKEN_NUMBER, ",
        dplyr::sql(second_col), "_TOKEN_NUMBER, ", "
        TOKEN,
        COALESCE(", dplyr::sql(first_col), "_TOKEN_NUMBER, ", "LEAD(", dplyr::sql(first_col), "_TOKEN_NUMBER IGNORE NULLS) OVER (PARTITION BY ", dplyr::sql(first_col), ", ", dplyr::sql(second_col), " ORDER BY ", dplyr::sql(second_col), "_TOKEN_NUMBER)) AS LEAD_TOKEN_NUMBER

      FROM
        (", dbplyr::sql_render(distinct_df), ")
    )

    SELECT ",
      dplyr::sql(first_col), ", ",
      dplyr::sql(second_col), ",
      LISTAGG(TOKEN, ' ') within group (order by LEAD_TOKEN_NUMBER, ", dplyr::sql(second_col), "_TOKEN_NUMBER) as ", dplyr::sql(merge_col), "

    FROM
      LT

    GROUP BY ",
      dplyr::sql(first_col), ", ",
      dplyr::sql(second_col)
  )

  # Generate merged strings from the query
  merged_df <- tbl(src = db_connection, dplyr::sql(sql_query))

  # Output the original data with the merged string joined to it
  df %>%
    dplyr::inner_join(y = merged_df)
}
