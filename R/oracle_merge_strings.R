#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
#'
#' This is intended to be used with differing address strings
#' The result is a 'master' address string that contains all parts of each address
#'
#' @param df Lazy Oracle table
#' @param col_one - first string column
#' @param col_two - second string column
#'
#' @details requires nhsbsaR::oracle_unnest_tokens() function
#'
#' @examples
#' table_db %>% nhsbsaR::oracle_merge_strings(ADDRESS_ONE, ADDRESS_TWO)
#'
#' @returns original df with additional merged column added
#'
#' @export
oracle_merge_strings <- function(df, col_one, col_two){

  # Default: ties.method = "first" thus no duplicates
  df <- df %>%
    dplyr::mutate(ID = dplyr::row_number({{ col_one }}))


  # Rename selected columns, for ease of use during code
  df_edit <- df %>%
    dplyr::select(ID, {{ col_one }}, {{ col_two }}) %>%
    dplyr::rename_at(c(2, 3), ~c("STRING_ONE", "STRING_TWO"))

  string_edit <- function(df, col){

    # Get 'ONE' or 'TWO' from designated string name
    STRING_NUM = substr({{ col }}, 8, 11)

    df %>%
      dplyr::select(ID, {{ col }}) %>%
      # Unnest token using nhsbsaR function
      nhsbsaR::oracle_unnest_tokens(col = {{ col }}, drop = TRUE) %>%
      dplyr::group_by(ID, TOKEN) %>%
      # Give each token a rank, so that each occurrence of a word can be numbered
      # E.g. 'CITY-1', 'CITY-2', 'CITY-3' etc
      dplyr::mutate(TOKEN_COUNT = RANK(TOKEN_NUMBER)) %>%
      dplyr::ungroup() %>%
      # Create bespoke joining term, which is ID, which token and the token occurence number
      # This controls and manages a later outer_join
      dplyr::mutate(TOKEN_JOIN = paste(TOKEN, TOKEN_COUNT, ID, sep = "*")) %>%
      dplyr::select(ID, TOKEN, TOKEN_JOIN, TOKEN_NUMBER) %>%
      # As function output is used twice, label vars depending on whether col_one or col_two is being processed
      dplyr::rename_at("ID", ~paste0(STRING_NUM, "_ID")) %>%
      dplyr::rename_at("TOKEN", ~paste0(STRING_NUM, "_TOKEN")) %>%
      dplyr::rename_at("TOKEN_NUMBER", ~paste0(STRING_NUM, "_ROW"))
    
  }

  # Col_one and col_two processed
  one <- df_edit %>%
    string_edit(col = STRING_ONE)
  two <- df_edit %>%
    string_edit(col = STRING_TWO)

  # Join 2 outputs on bespoke generated joining term and remove unneeded vars
  one_two <- one %>%
    dplyr::full_join(y = two, by = "TOKEN_JOIN") %>%
    dplyr::mutate(ID = COALESCE(ONE_ID, TWO_ID)) %>%
    dplyr::select(-c(ONE_ID, TWO_ID, TOKEN_JOIN))

  # Generate connection for generated joined output
  db_connection = one_two$src$con

  # Build SQL Query
  # This takes the next ONE_ROW term, ordered by TWO_ROW and partitioned by ID
  # LISTAGG then concatenates these ID-rowwise terms into a single cell
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "WITH LT AS (
    SELECT ID, ONE_ROW, TWO_ROW,
    COALESCE(ONE_TOKEN, TWO_TOKEN) AS LEAD_TOKEN,
    COALESCE(ONE_ROW, LEAD(ONE_ROW IGNORE NULLS) OVER (PARTITION BY ID ORDER BY TWO_ROW)) AS LEAD_ROW
    FROM (", dbplyr::sql_render(one_two), ")
    )
    SELECT
    ID,
    LISTAGG(LEAD_TOKEN, ' ') within group (order by LEAD_ROW, TWO_ROW) as MERGE_STRING
    FROM LT
    GROUP BY ID"
  )

  # Generate output from query
  output = tbl(db_connection, sql(sql_query))

  # Rejoin back to original data, and remove now unnneeded ID term
  df %>%
    dplyr::inner_join(y = output, by = "ID") %>%
    dplyr::select(-ID)

}
