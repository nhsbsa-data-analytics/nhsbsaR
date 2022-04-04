#' Query NHSBSA DBs and return results
#'
#' Use a DB connection built with `con_nhsbsa()` to deploy a query to the
#' database and return its results.
#'
#' @param con a DB connection. Use `con_nhsbsa()`
#' @param query a file path to a .sql file or character string containing a SQL
#' query
#'
#' @return a dataframe object
#' @export
#'
#' @examples
#' # build connection to DB. Requirse environment variables to be set
#' con <- nhsbsaR::con_nhsbsa(database = "DWCP")
#'
#' # create SQL file with query
#' file.create("example.sql")
#' writeLines(con = "example.sql", "SELECT * FROM all_tab_cols")
#'
#' df1 <- query_nhsbsa(con = con, query = "example.sql")
#'
#' # use query string
#' df2 <- query_nhsbsa(con = con, query = "select * from all_tab_cols")
#'
query_nhsbsa <- function(con, query) {

  if(file.exists(query)) {
    sql <- readr::read_file(query)
  } else {
    sql <- query
  }

  DBI::dbGetQuery(con, sql)

}
