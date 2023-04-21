#' Collect a lazy table with specified degree of parallelism
#'
#' Execute and collect dbplyr lazy table with specified degree of parallelism. Adds a parallel hint to the first SELECT statement only: `SELECT /* +PARALLEL(N) */...` The hint is usually taken into account, but as it is only a hint, the database optimizer ultimately makes a final decision as to which degree of parallelism to use, if any. This is based on things like the query itself and the amount of concurrent use. To override the optimizer and force a specific degree, the session needs to be altered: `alter session enable parallel query; alter session force parallel query parallel n;` (not supported here).
#'
#' @param lazy_tbl A dbplyr lazy table.
#'
#' @param n Degree of parallelism.
#'
#' @return Returns a local data frame.
#'
#' @examples
#' df <- collect_with_parallelism(df_lazy, 12)
#'
#' @export
collect_with_parallelism = function(lazy_tbl, n){

  # Pull the DB connection
  db_connection <- lazy_tbl$src$con

  # Specify degree of parallelism
  string_insert = paste0("SELECT /*+ PARALLEL(", n, ") */")

  # Specify parallelism for first select
  query = gsub("SELECT", string_insert, sql_render(lazy_tbl))

  # Build new query
  new_query = dbplyr::build_sql(con = db_connection, query)

  # Collect newly generated sql
  dplyr::tbl(src = db_connection, dplyr::sql(new_query)) %>% collect()

}
