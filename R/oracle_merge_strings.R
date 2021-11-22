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
#' table_db %>% nhsbsaR::oracle_merge_strings(ADDRESS_ONE, ADDRESS_TWO)
#' @returns original df with additional merged column added
#'
#' @export
oracle_merge_strings <- function(df, first_col, second_col, merge_col) {

  # Process first column
  first_col_df <- df %>%
    # Get the unique values
    dplyr::distinct(!!sym(first_col)) %>%
    # Tokenise
    nhsbsaR::oracle_unnest_tokens(
      col = first_col,
      drop = FALSE
    ) %>%
    # Give each token a rank within the string (e.g. 'CITY-1', 'CITY-2', etc)
    dplyr::rename(
      TOKEN = TOKEN,
      FIRST_COL_TOKEN_NUMBER = TOKEN_NUMBER
    ) %>%
    dplyr::group_by(!!sym(first_col), TOKEN) %>%
    dplyr::mutate(
      TOKEN_RANK = dplyr::row_number(FIRST_COL_TOKEN_NUMBER)
    ) %>%
    dplyr::ungroup()

  # Process second column
  second_col_df <- df %>%
    # Get the unique values
    dplyr::distinct(!!sym(second_col)) %>%
    # Tokenise
    nhsbsaR::oracle_unnest_tokens(
      col = second_col,
      drop = FALSE
    ) %>%
    # Give each token a rank within the string (e.g. 'CITY-1', 'CITY-2', etc)
    dplyr::rename(
      TOKEN = TOKEN,
      SECOND_COL_TOKEN_NUMBER = TOKEN_NUMBER
    ) %>%
    dplyr::group_by(!!sym(second_col), TOKEN) %>%
    dplyr::mutate(
      TOKEN_RANK = dplyr::row_number(SECOND_COL_TOKEN_NUMBER)
    ) %>%
    dplyr::ungroup()

  # Get the unique combinations we want to merge (incase there are duplicates)
  distinct_df <- df %>%
    dplyr::distinct(!!sym(first_col), !!sym(second_col))

  # Join the tokenised data together (attempt to join by TOKEN and TOKEN_RANK)
  distinct_df <-
    dplyr::full_join(
      x = first_col_df %>% dplyr::inner_join(y = distinct_df, copy = TRUE),
      y = second_col_df %>% dplyr::inner_join(y = distinct_df, copy = TRUE),
      copy = TRUE
    )

  # Pull the DB connection
  db_connection = df$src$con

  # Build SQL Query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "WITH LT AS
    (
      SELECT ",
        dplyr::sql(first_col), ", ",
        dplyr::sql(second_col), ",
        FIRST_COL_TOKEN_NUMBER,
        SECOND_COL_TOKEN_NUMBER,
        TOKEN,
        COALESCE(FIRST_COL_TOKEN_NUMBER, LEAD(FIRST_COL_TOKEN_NUMBER IGNORE NULLS) OVER (PARTITION BY ", dplyr::sql(first_col), ", ", dplyr::sql(second_col), " ORDER BY SECOND_COL_TOKEN_NUMBER)) AS LEAD_TOKEN_NUMBER

      FROM
        (", dbplyr::sql_render(distinct_df), ")
    )

    SELECT ",
      dplyr::sql(first_col), ", ",
      dplyr::sql(second_col), ",
      LISTAGG(TOKEN, ' ') within group (order by LEAD_TOKEN_NUMBER, SECOND_COL_TOKEN_NUMBER) as ", dplyr::sql(merge_col), "

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

