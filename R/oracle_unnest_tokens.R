#' Split a column of an Oracle table into tokens
#'
#' A rip off of `tidytext::unnest_tokens` that will work on lazy Oracle tables.
#' This function will retain all columns and add TOKEN_NUMBER and TOKEN columns
#' to the table.
#'
#' Inspiration:
#' https://stackoverflow.com/questions/59537458/how-to-pipe-sql-into-rs-dplyr
#'
#' @param df Lazy Oracle table
#' @param col Column to be tokenised
#' @param drop Whether original input column should get dropped
#' @param regex regex to tokenise by
#'
#' @examples
#' table_db %>% nhsbsaR::oracle_unnest_tokens(col = "DUMMY")
#'
#' @export
oracle_unnest_tokens <- function(df, col, drop = TRUE, pattern ="[:space:]") {

  # Pull the connection
  db_connection <- df$src$con

  # Which columns do we want to output
  output_cols <- colnames(df)
  if (drop) {
    output_cols <- output_cols[output_cols != col]
  }

  # Convert to a comma'd list sring of cols "col1, col2, col3" to use in  SQL
  output_cols <- paste0(output_cols, collapse = ", ")

  # Formulate the SQL for tokenising in Oracle
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "
      SELECT
        ", dplyr::sql(output_cols),",
        ROW_NUMBER() OVER (PARTITION BY ", dplyr::sql(output_cols), " ORDER BY lines.column_value) AS TOKEN_NUMBER,
        TRIM(REGEXP_SUBSTR(", dplyr::sql(col), ", '[^", dplyr::sql(pattern), "]+', 1, lines.column_value))        AS TOKEN

      FROM
        (", dbplyr::sql_render(df), "),
        TABLE(CAST(MULTISET(SELECT LEVEL FROM dual CONNECT BY INSTR(", dplyr::sql(col), " , ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
      "
  )

  dplyr::tbl(db_connection, dplyr::sql(sql_query))
}
