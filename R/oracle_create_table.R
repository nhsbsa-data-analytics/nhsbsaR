#' Create table from `{dbplyr}` chain
#'
#' Pipe into the end of a `{dbplyr}` to create a table in your database schema.
#'
#' @param df Lazy Oracle table
#' @param schema_name Name of the schema. Default is NULL (your own).
#' @param table_name Name of the table
#' @param overwrite Whether to overwrite an existing table
#'
#' @examples \dontrun{
#' dplyr::tbl(src = con, from = "DUAL") %>%
#'   oracle_create_table("<schema_name>", "<table_name>")
#' }
#'
#' @export
oracle_create_table <- function(df, schema_name = NULL, table_name, overwrite = TRUE) {

  # Pull the connection
  db_connection <- df$src$con

  # Define the full table name
  full_table_name <- table_name
  if (!is.null(schema_name)) {
    full_table_name <- paste0(schema_name, ".", full_table_name)
  }

  # Create table statement
  sql_statement <- paste0(
    "CREATE TABLE ", full_table_name, " AS ",
    "SELECT * FROM (", dbplyr::sql_render(df), ")"
  )

  # Check if the table exists
  exists <- DBI::dbExistsTable(conn = db_connection, name = full_table_name)

  # If we are overwriting then make sure to drop any existing table beforehand
  if (overwrite & exists) {
    DBI::dbRemoveTable(conn = db_connection, name = full_table_name)
  }

  # Create the table
  DBI::dbExecute(df$src$con, as.character(sql_statement))
}
