#' Drop a table from the database
#'
#' Drop a database table in user's own schema.
#'
#' @param table_name name of the table in SQL database
#' @param conn connection object created using `nhsbsaR::con_nhsbsa()`
#' @param purge logical. If `TRUE`, releases space associated with the table back to the tablespace for use by other tables/objects. If `FALSE`, the table and its dependent objects are placed into recycle bin and continue to count toward the user's space quota. Tables in the recycle bin can be brought back using a `FLASHBACK TABLE` statement.
#' @param silent If `TRUE`, does not output info messages `Table dropped` or `Table does not exist` to the console.
#'
#' @examples
#' \dontrun{
#' # If connection object is named `con`
#' drop_table("INT646_TABLE_DB")
#' # If connection object is not named `con`
#' drop_table("INT646_TABLE_DB", conn = my_connection_name)
#' }
#' @export
drop_table <- function(table_name, conn = con, purge = TRUE, silent = F) {

  if(DBI::dbExistsTable(
      conn,
      #name = DBI::Id(schema = "MIGAR", table = toupper(table_name))
      name = DBI::Id(schema = toupper(conn@info$username), table = toupper(table_name))
      )) {

    drop_stmt <- if (purge) glue::glue("DROP TABLE {table_name} CASCADE CONSTRAINTS PURGE")
                       else glue::glue("DROP TABLE {table_name} CASCADE CONSTRAINTS")

    DBI::dbExecute(conn, statement = drop_stmt)

    if (silent==F) print(glue::glue("{table_name} dropped"))

  } else {
    if (silent==F) print(glue::glue("{table_name} does not exist"))
  }

}
