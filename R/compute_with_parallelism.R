#' Function to create table from lazy query with specified degree of parallelism
#'
#' Compute (a-la dbplyr) a lazy query with specified degree of parallelism. Adds a parallel hint to all SELECT statements: `SELECT /* +PARALLEL(N) */...` The hint is usually taken into account, but as it is only a hint, the database optimizer ultimately makes a final decision as to which degree of parallelism to use, if any. This is based on things like the query itself and the amount of concurrent use.\cr
#' \cr
#' If hints are already present in any sub-queries, e.g. included  manually using `dbplyr::sql()`, this function will overwrite them.
#'
#' @param lazy_tbl name of the dbplyr lazy table
#' @param create_table_name name of user created table in SQL database
#' @param n the degree of parallelism to enforce
#' @param overwrite if `TRUE`, function will check if table exists, and if it does, will drop it; if `FALSE`, will intentionally throw an error if the table already exists.
#' @param compress Compression setting for the table: `high` = `compress for query high`, `low` = `compress for query low`, `no` = `nocompress` (forced non-compression), `NA`,`NULL` or any other value = compression option not passed to the database, determined by database defaults.
#' @param materialize if `TRUE`, will additionally add `MATERIALIZE` hints
#'
#' @examples
#' \dontrun{
#' compute_with_parallelism(table_db, "INT646_TABLE_DB", 32)
#' }
#' @export
compute_with_parallelism = function(lazy_tbl,
                                    create_table_name,
                                    n,
                                    overwrite = F,
                                    compress = "high",
                                    materialize = F) {

  # Pull the DB connection
  db_connection <- lazy_tbl$src$con

  # If overwrite is TRUE, check if the table exists, and if it does, drop it
  if (overwrite) {

    if (DBI::dbExistsTable(
          conn = db_connection,
          name = DBI::Id(schema = toupper(db_connection@info$username), table = create_table_name)
      ) == T
     ) {
      DBI::dbRemoveTable(
        conn = db_connection,
        name = DBI::Id(schema = toupper(db_connection@info$username), table = create_table_name)
      )
    }
  }

  if (materialize) {
    string_insert = paste0("SELECT /*+ MATERIALIZE PARALLEL(", n, ") */")
  } else {
    string_insert = paste0("SELECT /*+ PARALLEL(", n, ") */")
  }

  if (compress == "high") { compression_string = "COMPRESS FOR QUERY HIGH " } else
  if (compress == "low")  { compression_string = "COMPRESS FOR QUERY LOW " } else
  if (compress == "no")   { compression_string = "NOCOMPRESS " } else
                          { compression_string = "" }

  # Specify parallelism after each select
  new_query <- gsub(
    "SELECT(\\s+/\\*.*\\*/)?",       # Replaces existing hints if they are present
    string_insert,
    dbplyr::sql_render(lazy_tbl),
    ignore.case = T
  )

  new_query <- paste0(
    "CREATE TABLE ",
    create_table_name,
    " NOLOGGING ",
    compression_string,
    "AS ",
    new_query
  )

  # Send query to the database
  # (DBI::dbExecute returns the number of rows affected, which is silenced by assignment to 'output')
  output <- DBI::dbExecute(conn = db_connection, statement = new_query)
}
