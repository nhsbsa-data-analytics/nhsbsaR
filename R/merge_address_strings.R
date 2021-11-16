#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
#'
#' This is intended to be used with differing address strings
#' The result is a 'master' address string that contains all parts of each address
#'
#' @param df Lazy Oracle table
#'
#' @details requires hsbsaR::oracle_unnest_tokens() function
#'
#' @note df must have only 3 columns
#' @note These must the Unique Row Identifier (ID) and then the 2 strings to be merged
#' @note The ID column must be first of the 3 columns
#'
#' @examples
#' table_db %>%
#' dplyr::select(ID, STRING_ONE, STRING_TWO) %>%
#' nhsbsaR::merge_address_strings()
#'
#' @returns original df with additional column added
#'
#' @export

merge_address_strings = function(df, id_col, col_one, col_two){

  df_edit = df %>%
    dplyr::select({{ id_col }}, {{ col_one }}, {{ col_two }})

  S1 = colnames(df_edit)[1]

  df_edit = df_edit %>%
    dplyr::rename_at(c(1:3), ~c("ID", "STRING_ONE", "STRING_TWO"))

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

  output = tbl(db_connection, sql(sql_query)) %>%
    dplyr::rename_at("ID", ~S1)

  df = df %>%
    dplyr::inner_join(y = output)

  return(df)
}

#-------------------------------------------------------------------------------
