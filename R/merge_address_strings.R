#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
#'
#' This is intended to be used with differing address strings
#' The result is a 'master' address string that contains all parts of each address
#'
#' @param df Lazy Oracle table
#' @param col_one - first string column
#' @param col_two - second string column
#'
#' @details requires hsbsaR::oracle_unnest_tokens() function
#'
#' @examples
#' table_db %>% nhsbsaR::merge_address_strings(ADDRESS_ONE, ADDRESS_TWO)
#'
#' @returns original df with additional merged column added
#'
#' @export

merge_address_strings = function(df, col_one, col_two){

  df = df %>%
    # Default: ties.method = "first" thus no duplicates
    dplyr::mutate(ID = row_number({{ col_one }}))

  df_edit = df %>%
    dplyr::select(ID, {{ col_one }}, {{ col_two }}) %>%
    dplyr::rename_at(c(2,3), ~c("STRING_ONE", "STRING_TWO"))

  string_edit = function(df, STRING_NAME){

    STRING_NUM = substr(STRING_NAME, 8, 11)

    df = df %>%
      dplyr::select("ID", STRING_NAME) %>%
      nhsbsaR::oracle_unnest_tokens(df = ., col = STRING_NAME, drop = T) %>%
      dplyr::group_by(ID, TOKEN) %>%
      dplyr::mutate(TOKEN_COUNT = rank(TOKEN_NUMBER)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(TOKEN_JOIN = paste(TOKEN,TOKEN_COUNT,ID, sep = "*")) %>%
      dplyr::select(ID, TOKEN, TOKEN_JOIN, TOKEN_NUMBER) %>%
      dplyr::rename_at("ID", ~paste0(STRING_NUM, "_ID")) %>%
      dplyr::rename_at("TOKEN", ~paste0(STRING_NUM, "_TOKEN")) %>%
      dplyr::rename_at("TOKEN_NUMBER", ~paste0(STRING_NUM, "_ROW"))
    return(df)
  }

  one = string_edit(df_edit, "STRING_ONE")
  two = string_edit(df_edit, "STRING_TWO")

  one_two = one %>%
    dplyr::full_join(two, by = 'TOKEN_JOIN') %>%
    dplyr::mutate(ID = coalesce(ONE_ID, TWO_ID)) %>%
    dplyr::select(-c(ONE_ID, TWO_ID, TOKEN_JOIN))

  db_connection = one_two$src$con

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

  output = tbl(db_connection, sql(sql_query))

  df = df %>%
    dplyr::inner_join(y = output, by = "ID") %>%
    dplyr::select(-ID)

  return(df)
}

#-------------------------------------------------------------------------------
